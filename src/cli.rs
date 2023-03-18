use std::path::PathBuf;

pub fn print_usage() {
	println!("USAGE: \n\tlox-v1 [script]");
}

pub struct Args {
	pub help: bool,
	pub script: Option<PathBuf>,
}

pub enum Error {
	//MissingParameter(&'static str),
}

impl Args {
	#[allow(clippy::while_let_on_iterator)]
	pub fn from_args() -> Result<Self, Error> {
		let mut args = std::env::args().skip(1).peekable();

		let mut help = false;
		let mut script = None;

		while let Some(arg) = args.next() {
			match arg.as_str() {
				"--help" => help = true,
				_ => script = Some(PathBuf::from(arg)),
			}
		}

		Ok(Args { help, script })
	}
}
