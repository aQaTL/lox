use std::rc::Rc;

use crate::{
	environment::Environment,
	interpreter::{function::NativeFunction, Interpreter, Value},
};

pub fn globals() -> Environment {
	let mut globals = Environment::default();

	globals.define(
		"clock".to_string(),
		Some(Value::Function(Rc::new(NativeFunction {
			arity: 0,
			callable: Rc::new(|_interpreter: &mut Interpreter, _arguments: Vec<Value>| {
				let time = std::time::UNIX_EPOCH.elapsed().unwrap().as_secs_f64();
				Ok(Value::Number(time))
			}),
		}))),
	);

	globals
}
