use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{interpreter::Value, token::Token};

#[derive(Debug, Default, Clone)]
pub struct Environment {
	// outer / parent scope
	enclosing: Option<Rc<RefCell<Environment>>>,
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
	pub fn new(enclosing: Rc<RefCell<Environment>>) -> Rc<RefCell<Self>> {
		Rc::new(RefCell::new(Environment {
			enclosing: Some(enclosing),
			..Default::default()
		}))
	}

	pub fn get(&self, token: &Token) -> Option<Option<Value>> {
		self.values.get(&token.lexeme).cloned().or_else(|| {
			self.enclosing
				.as_ref()
				.and_then(|env| env.borrow().get(token))
		})
	}

	pub fn get_at(this: Rc<RefCell<Self>>, token: &Token, distance: i32) -> Value {
		Environment::ancestor(this, distance)
			.borrow()
			.values
			.get(&token.lexeme)
			.unwrap()
			.clone()
			.unwrap()
	}

	fn ancestor(this: Rc<RefCell<Self>>, distance: i32) -> Rc<RefCell<Environment>> {
		let mut env: Rc<RefCell<Environment>> = this;
		for _ in 0..distance {
			let enclosing = env
				.borrow()
				.enclosing
				.clone()
				.expect("Ancestor env is None");
			env = enclosing;
		}
		env
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
			});
		match var {
			Ok(var) => {
				*var = Some(v);
				Ok(())
			}
			Err(err) => match self.enclosing {
				Some(ref enclosing) => enclosing.borrow_mut().assign(name, v),
				None => Err(err),
			},
		}
	}

	pub fn assign_at(
		this: Rc<RefCell<Self>>,
		distance: i32,
		name: &str,
		v: Value,
	) -> Result<(), Error> {
		*Environment::ancestor(this, distance)
			.borrow_mut()
			.values
			.get_mut(name)
			.expect("Undefined variable") = Some(v);
		Ok(())
	}
}
