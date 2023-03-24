use std::{collections::HashMap, fmt::Display};

use crate::{interpreter::Value, token::Token};

#[derive(Default)]
pub struct Environment {
	values: HashMap<String, Option<Value>>,
}

#[derive(Debug)]
pub enum Error {
	UndefinedVariable { name: String },
}

impl Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Error::UndefinedVariable { name } => write!(f, "undefined variable `{name}`"),
		}
	}
}

impl Environment {
	pub fn get(&self, token: &Token) -> Option<&Option<Value>> {
		self.values.get(&token.lexeme)
	}

	pub fn define(&mut self, name: String, v: Option<Value>) {
		self.values.insert(name, v);
	}

	pub fn assign(&mut self, name: &str, v: Value) -> Result<(), Error> {
		let var = self
			.values
			.get_mut(name)
			.ok_or_else(|| Error::UndefinedVariable {
				name: name.to_string(),
			})?;
		*var = Some(v);
		Ok(())
	}
}
