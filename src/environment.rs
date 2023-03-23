use std::collections::HashMap;

use crate::{interpreter::Value, token::Token};

#[derive(Default)]
pub struct Environment {
	values: HashMap<String, Option<Value>>,
}

impl Environment {
	pub fn get(&self, token: &Token) -> Option<&Option<Value>> {
		self.values.get(&token.lexeme)
	}

	pub fn define(&mut self, name: String, v: Option<Value>) {
		self.values.insert(name, v);
	}
}
