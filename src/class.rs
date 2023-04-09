use crate::interpreter::function::Callable;
use crate::interpreter::{Error, Value};
use crate::token::Token;
use crate::Interpreter;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct Class {
	pub name: Token,
}

impl Display for Class {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		write!(f, "{}", self.name.lexeme)
	}
}

impl Callable for Class {
	fn arity(&self) -> usize {
		0
	}

	fn type_name(&self) -> String {
		self.to_string()
	}

	fn call(&self, _interpreter: &mut Interpreter, _arguments: Vec<Value>) -> Result<Value, Error> {
		Ok(Value::Object(Instance {
			class: self.clone(),
		}))
	}
}

#[derive(Debug, Clone)]
pub struct Instance {
	pub class: Class,
}

impl Display for Instance {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		write!(f, "{} instance", self.class)
	}
}
