use crate::interpreter::function::{Callable, Function};
use crate::interpreter::{Error, Value};
use crate::token::Token;
use crate::Interpreter;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Class(Rc<ClassInner>);

impl Display for Class {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		self.0.fmt(f)
	}
}

impl Class {
	pub fn new(name: Token, methods: HashMap<String, Function>) -> Self {
		Class(Rc::new(ClassInner { name, methods }))
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

#[derive(Debug)]
struct ClassInner {
	name: Token,
	methods: HashMap<String, Function>,
}

impl Display for ClassInner {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		write!(f, "{}", self.name.lexeme)
	}
}

impl ClassInner {
	fn find_method(&self, name: &str) -> Option<Function> {
		self.methods.get(name).cloned()
	}
}

#[derive(Debug, Clone)]
pub struct Instance {
	class: Rc<ClassInner>,
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
			class: Rc::clone(&class.0),
			fields: HashMap::new(),
		}
	}

	pub fn get(this: &Rc<RefCell<Self>>, name: &str) -> Option<Value> {
		let this_ref = this.borrow();
		if let field @ Some(_) = this_ref.fields.get(name).cloned() {
			return field;
		}

		if let Some(method) = this_ref.class.find_method(name) {
			return Some(Value::Function(Rc::new(method.bind(Rc::clone(this)))));
		}

		None
	}

	pub fn set(&mut self, name: String, v: Value) {
		self.fields.insert(name, v);
	}
}
