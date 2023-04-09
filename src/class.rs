use crate::interpreter::function::Callable;
use crate::interpreter::{Error, Value};
use crate::token::Token;
use crate::Interpreter;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

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
		Ok(Value::Instance(Rc::new(RefCell::new(Instance::new(
			self.clone(),
		)))))
	}
}

#[derive(Debug, Clone)]
pub struct Instance {
	pub class: Class,
	fields: HashMap<String, Value>,
}

impl Display for Instance {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		write!(f, "{} instance", self.class)
	}
}

impl Instance {
	pub fn new(class: Class) -> Self {
		Instance {
			class,
			fields: HashMap::new(),
		}
	}

	pub fn get(&self, name: &str) -> Option<Value> {
		self.fields.get(name).cloned()
	}

	pub fn set(&mut self, name: String, v: Value) {
		self.fields.insert(name, v);
	}
}
