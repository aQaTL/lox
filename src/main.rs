use std::{
	fmt::Display,
	io::{self, Write},
	path::Path,
	sync::atomic::{AtomicBool, Ordering},
};

use crate::cli::Args;
use crate::scanner::Scanner;

mod cli;
mod scanner;
mod token;

fn main() {
	let args = match Args::from_args() {
		Ok(v) => v,
		/*
		Err(cli::Error::MissingParameter(param_name)) => {
			eprintln!("Missing parameter {param_name}");
			cli::print_usage();
			std::process::exit(143);
		},
		*/
		Err(_) => {
			std::process::exit(143);
		}
	};

	if args.help {
		cli::print_usage();
		return;
	}

	let result = match args.script {
		Some(ref script) => run_file(&args, script),
		None => run_prompt(&args),
	};

	if let Err(err) = result {
		eprintln!("Error: {err:#?}");
		std::process::exit(144);
	}
}

#[derive(Debug)]
enum Error {
	Io(io::Error),
	ExecutionError(ExecutionError),
}

impl From<ExecutionError> for Error {
	fn from(v: ExecutionError) -> Self {
		Error::ExecutionError(v)
	}
}

impl From<io::Error> for Error {
	fn from(v: io::Error) -> Self {
		Error::Io(v)
	}
}

fn run_prompt(_args: &Args) -> Result<(), Error> {
	let stdin = std::io::stdin();
	let stdout = std::io::stdout();

	let mut line = String::new();

	loop {
		line.clear();
		{
			let mut stdout = stdout.lock();
			stdout.write_all(b"> ")?;
			stdout.flush()?;
		}
		if stdin.read_line(&mut line)? == 0 {
			break;
		}
		if let Err(err) = run(&line) {
			eprintln!("Error: {err:#?}");
		}
		HAD_ERROR.store(false, Ordering::Relaxed);
	}

	Ok(())
}

fn run_file(_args: &Args, script: &Path) -> Result<(), Error> {
	let source = std::fs::read_to_string(script)?;
	run(&source)?;
	if HAD_ERROR.load(Ordering::Relaxed) {
		return Err(ExecutionError::GenericError.into());
	}
	Ok(())
}

#[derive(Debug)]
enum ExecutionError {
	GenericError,
}

static HAD_ERROR: AtomicBool = AtomicBool::new(false);

fn error(line: usize, msg: impl Display) {
	report(line, "", msg);
}

fn report(line: usize, place: &str, msg: impl Display) {
	eprintln!("[line {line}] Error {place}: {msg}");
	HAD_ERROR.store(true, Ordering::Relaxed);
}

fn run(source: &str) -> Result<(), ExecutionError> {
	let scanner = Scanner::new(source);
	let tokens = scanner.scan_tokens();
	for token in tokens {
		println!("Token: {token:?}");
	}
	Ok(())
}

enum Expr {
	Literal(token::Token),
	Unary {
		operator: token::Token,
		expr: Box<Expr>,
	},
	Binary {
		left: Box<Expr>,
		operator: token::Token,
		right: Box<Expr>,
	},
	Grouping(Box<Expr>),
}

fn print_ast(expr: &Expr, w: &mut impl std::fmt::Write) -> std::fmt::Result {
	use token::{Token, TokenType};

	fn parenthesize(w: &mut impl std::fmt::Write, name: &str, exprs: &[&Expr]) -> std::fmt::Result {
		write!(w, "({name}")?;
		for expr in exprs {
			write!(w, " ")?;
			print_ast(expr, w)?;
		}
		write!(w, ")")?;
		Ok(())
	}

	match expr {
		Expr::Literal(Token {
			token_type: TokenType::Number(v),
			..
		}) => write!(w, "{v}"),
		Expr::Literal(Token {
			token_type: TokenType::String(v),
			..
		}) => write!(w, "{v}"),
		Expr::Literal(Token {
			token_type: TokenType::Identifier,
			lexeme,
			..
		}) => write!(w, "{lexeme}"),
		Expr::Literal(l) => panic!("{l:?}"),
		Expr::Binary {
			left,
			operator: Token { lexeme, .. },
			right,
		} => parenthesize(w, lexeme, &[left, right]),
		Expr::Grouping(expr) => parenthesize(w, "group", &[expr]),
		Expr::Unary { operator, expr } => parenthesize(w, &operator.lexeme, &[expr]),
	}
}

#[cfg(test)]
mod tests {
	use super::Expr;
	use crate::token::{Token, TokenType};

	#[test]
	fn test_ast_printer() {
		let expr = Expr::Binary {
			left: Box::new(Expr::Unary {
				operator: Token {
					token_type: TokenType::Minus,
					lexeme: "-".to_string(),
					line: 1,
				},
				expr: Box::new(Expr::Literal(Token {
					token_type: TokenType::Number(123.0),
					lexeme: "123".to_string(),
					line: 1,
				})),
			}),
			operator: Token {
				token_type: TokenType::Star,
				lexeme: "*".to_string(),
				line: 1,
			},
			right: Box::new(Expr::Grouping(Box::new(Expr::Literal(Token {
				token_type: TokenType::Number(45.67),
				lexeme: "45.67".to_string(),
				line: 1,
			})))),
		};

		let expected = "(* (- 123) (group 45.67))";

		let mut actual = String::new();
		super::print_ast(&expr, &mut actual).unwrap();

		assert_eq!(expected, actual);
	}
}
