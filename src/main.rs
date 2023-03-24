#![allow(clippy::result_large_err)]
#![allow(clippy::large_enum_variant)]

use std::{
	fmt::Display,
	io::{self, Write},
	path::Path,
	sync::atomic::{AtomicBool, Ordering},
};

use crate::cli::Args;
use crate::interpreter::Interpreter;
use crate::scanner::Scanner;

mod cli;
mod environment;
mod interpreter;
mod parser;
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
		eprintln!("Error: {err}");
		let exit_code = match err {
			Error::Io(_) => 1,
			Error::ExecutionError(ExecutionError::GenericError) => 65,
			Error::ExecutionError(ExecutionError::Parse(_)) => 66,
			Error::ExecutionError(ExecutionError::Eval(_)) => 70,
		};
		std::process::exit(exit_code);
	}
}

#[derive(Debug)]
enum Error {
	Io(io::Error),
	ExecutionError(ExecutionError),
}

impl Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Error::Io(err) => write!(f, "io error: {err}"),
			Error::ExecutionError(err) => err.fmt(f),
		}
	}
}

impl std::error::Error for Error {}

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

	let mut interpreter = Interpreter::default();
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
		if let Err(err) = run(&mut interpreter, &line) {
			eprintln!("Error: {err}");
		}
		HAD_ERROR.store(false, Ordering::Relaxed);
	}

	Ok(())
}

fn run_file(_args: &Args, script: &Path) -> Result<(), Error> {
	let source = std::fs::read_to_string(script)?;
	run(&mut Interpreter::default(), &source)?;
	if HAD_ERROR.load(Ordering::Relaxed) {
		return Err(ExecutionError::GenericError.into());
	}
	Ok(())
}

#[derive(Debug)]
enum ExecutionError {
	GenericError,
	Parse(parser::Error),
	Eval(interpreter::Error),
}

impl Display for ExecutionError {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			ExecutionError::GenericError => write!(f, "generic error"),
			ExecutionError::Parse(err) => write!(f, "parse error: {err}"),
			ExecutionError::Eval(err) => write!(f, "runtime error: {err}"),
		}
	}
}

impl std::error::Error for ExecutionError {}

impl From<parser::Error> for ExecutionError {
	fn from(v: parser::Error) -> Self {
		ExecutionError::Parse(v)
	}
}

impl From<interpreter::Error> for ExecutionError {
	fn from(v: interpreter::Error) -> Self {
		ExecutionError::Eval(v)
	}
}

static HAD_ERROR: AtomicBool = AtomicBool::new(false);

fn error(line: usize, msg: impl Display) {
	report(line, "", msg);
}

fn report(line: usize, place: &str, msg: impl Display) {
	eprintln!("[line {line}] Error {place}: {msg}");
	HAD_ERROR.store(true, Ordering::Relaxed);
}

fn run(interpreter: &mut Interpreter, source: &str) -> Result<(), ExecutionError> {
	let scanner = Scanner::new(source);
	let tokens = scanner.scan_tokens();
	#[cfg(debug_assertions)]
	for token in &tokens {
		println!("Token: {token:?}");
	}
	let statements = parser::Parser::new(tokens).parse()?;
	/*
	println!("Expr: {expr:#?}");
	println!("Expr: {expr}");
	*/
	interpreter.interpret(statements)?;
	//println!("Result: {value}");
	Ok(())
}
