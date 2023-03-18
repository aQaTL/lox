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
