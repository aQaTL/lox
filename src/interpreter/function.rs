use std::cell::RefCell;
use std::rc::Rc;

use crate::environment::Environment;
use crate::interpreter::{Error, Interpreter, Value};
use crate::parser::Stmt;
use crate::token::Token;

pub trait Callable: std::fmt::Debug {
	fn arity(&self) -> usize;
	fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value, Error>;
}

pub type BoxedCallable = Rc<dyn Fn(&mut Interpreter, Vec<Value>) -> Result<Value, Error>>;

#[derive(Clone)]
pub struct NativeFunction {
	pub arity: usize,
	pub callable: BoxedCallable,
}

impl std::fmt::Debug for NativeFunction {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		f.debug_struct("NativeFunction")
			.field("arity", &self.arity)
			.field("callable", &"<native code>")
			.finish()
	}
}

impl Callable for NativeFunction {
	fn arity(&self) -> usize {
		self.arity
	}

	fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value, Error> {
		(self.callable)(interpreter, arguments)
	}
}

#[derive(Clone, Debug)]
pub struct Function {
	pub declaration_name: Token,
	pub declaration_params: Vec<Token>,
	pub declaration_body: Vec<Stmt>,

	pub closure: Rc<RefCell<Environment>>,
}

impl Callable for Function {
	fn arity(&self) -> usize {
		self.declaration_params.len()
	}

	fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value, Error> {
		let mut env = Environment::new(Rc::clone(&self.closure));
		{
			let env = Rc::get_mut(&mut env).unwrap();
			let mut env = RefCell::borrow_mut(env);
			for (param, argument) in self.declaration_params.iter().zip(arguments) {
				env.define(param.lexeme.clone(), Some(argument))
			}
		}
		interpreter.interpret_block(self.declaration_body.clone(), env)?;
		Ok(Value::Null)
	}
}
