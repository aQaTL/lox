use std::collections::HashMap;
use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::{
	environment::{self, Environment},
	parser::{Expr, Stmt},
	token::{Token, TokenType},
};

pub mod function;

pub struct Interpreter {
	globals: Rc<RefCell<Environment>>,
	environment: Rc<RefCell<Environment>>,
	locals: HashMap<Expr, i32>,
}

impl Default for Interpreter {
	fn default() -> Self {
		let globals = Rc::new(RefCell::new(crate::globals::globals()));
		let environment = Rc::clone(&globals);
		Interpreter {
			globals,
			environment,
			locals: HashMap::default(),
		}
	}
}

#[derive(Debug, Clone)]
pub enum Value {
	Null,
	Bool(bool),
	Number(f64),
	String(String),
	/// TODO(aqatl): objects
	Object,
	Function(Rc<dyn function::Callable>),
}

impl Value {
	fn type_name(&self) -> &'static str {
		match self {
			Value::Null => "Null",
			Value::Bool(_) => "Bool",
			Value::Number(_) => "Number",
			Value::String(_) => "String",
			Value::Object => "Object",
			Value::Function(_) => "<native fn>",
		}
	}

	fn is_truthy(&self) -> bool {
		match self {
			Value::Null => false,
			Value::Bool(b) => *b,
			_ => true,
		}
	}

	fn is_equal(&self, v: &Value) -> bool {
		match (self, v) {
			(Value::Null, Value::Null) => true,
			(Value::Bool(a), Value::Bool(b)) => a == b,
			(Value::Number(a), Value::Number(b)) => a == b,
			(Value::String(a), Value::String(b)) => a == b,
			(Value::Object, Value::Object) => true,
			_ => false,
		}
	}

	fn into_callable(self) -> Option<Rc<dyn function::Callable>> {
		match self {
			Value::Function(callable) => Some(callable),
			_ => None,
		}
	}
}

impl Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Value::Null => write!(f, "nil"),
			Value::Bool(v) => write!(f, "{v}"),
			Value::Number(n) => write!(f, "{n}"),
			Value::String(s) => write!(f, "{s}"),
			Value::Object => write!(f, "{{}}"),
			Value::Function(_) => todo!(),
		}
	}
}

#[derive(Debug)]
pub struct DowncastError {
	got: Value,
}

impl TryInto<f64> for Value {
	type Error = DowncastError;

	fn try_into(self) -> Result<f64, Self::Error> {
		match self {
			Value::Number(n) => Ok(n),
			v => Err(DowncastError { got: v }),
		}
	}
}

impl TryInto<String> for Value {
	type Error = DowncastError;

	fn try_into(self) -> Result<String, Self::Error> {
		match self {
			Value::String(s) => Ok(s),
			v => Err(DowncastError { got: v }),
		}
	}
}

impl TryInto<bool> for Value {
	type Error = DowncastError;

	fn try_into(self) -> Result<bool, Self::Error> {
		match self {
			Value::Bool(b) => Ok(b),
			v => Err(DowncastError { got: v }),
		}
	}
}

#[derive(Debug)]
pub enum Error {
	UnexpectedLiteral(Token),
	InvalidUnaryOperator(Token),
	ExpectedNumber {
		err: DowncastError,
		token: Token,
	},
	InvalidPlusOperatorOperands {
		left: Value,
		right: Value,
		token: Token,
	},
	InvalidBinaryOperator(Token),
	UnknownVariable(Token),
	UninitializedVariable(Token),
	Environment(environment::Error),
	InvalidLogicalOperator(Token),
	InvalidFunctionCallee(Token),
	InvalidNumberOfParameters {
		expected: usize,
		got: usize,
		token: Token,
	},

	ReturnStatement(Value),
}

impl Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Error::UnexpectedLiteral(Token {
				token_type,
				lexeme,
				line,
				..
			}) => {
				write!(f, "[line {line}] unexpeted {token_type:?} `{lexeme}`")
			}
			Error::InvalidUnaryOperator(Token {
				token_type,
				lexeme,
				line,
				..
			}) => {
				write!(
					f,
					"[line {line}] invalid unary operator {token_type:?} at `{lexeme}`"
				)
			}
			Error::ExpectedNumber {
				err: DowncastError { got },
				token: Token { line, .. },
			} => {
				write!(
					f,
					"[line {line}] expected a Number, got {}",
					got.type_name(),
				)
			}
			Error::InvalidPlusOperatorOperands {
				left,
				right,
				token: Token { line, .. },
			} => {
				write!(
					f,
					"[line {line}] expected a Number or a String, got {} and {}",
					left.type_name(),
					right.type_name(),
				)
			}
			Error::InvalidBinaryOperator(Token {
				token_type,
				lexeme,
				line,
				..
			}) => write!(
				f,
				"[line {line}] invalid binary operator `{lexeme}` ({token_type:?})"
			),
			Error::UnknownVariable(Token { lexeme, line, .. }) => {
				write!(f, "[line {line}] unknown variable `{lexeme}`")
			}
			Error::UninitializedVariable(Token { lexeme, line, .. }) => {
				write!(f, "[line {line}] uninitialized variable `{lexeme}`")
			}
			Error::Environment(err) => err.fmt(f),
			Error::InvalidLogicalOperator(Token {
				token_type,
				lexeme,
				line,
				..
			}) => write!(
				f,
				"[line {line}] invalid logical operator `{lexeme}` ({token_type:?})"
			),
			Error::InvalidFunctionCallee(Token { line, .. }) => {
				write!(f, "[line {line}] can only call functions and classes")
			}
			Error::InvalidNumberOfParameters {
				expected,
				got,
				token: Token { line, .. },
			} => write!(
				f,
				"[line {line}] expected {expected} arguments, but got {got}"
			),
			Error::ReturnStatement(_) => write!(f, "return"),
		}
	}
}

impl From<environment::Error> for Error {
	fn from(v: environment::Error) -> Self {
		Error::Environment(v)
	}
}

impl Interpreter {
	pub fn interpret(&mut self, statements: impl IntoIterator<Item = Stmt>) -> Result<(), Error> {
		for statement in statements {
			match statement {
				Stmt::Print(expr) => {
					let v = self.eval(expr)?;
					println!("{v}");
				}
				Stmt::Expr(expr) => {
					self.eval(expr)?;
				}
				Stmt::Var { name, initializer } => {
					let value = match initializer {
						Some(expr) => Some(self.eval(expr)?),
						None => None,
					};
					self.environment.borrow_mut().define(name.lexeme, value);
				}
				Stmt::Block(statements) => {
					let env = Environment::new(Rc::clone(&self.environment));
					// println!("new block, new env {env:?}");
					self.interpret_block(statements, env)?;
				}
				Stmt::If {
					condition,
					then_branch,
					else_branch,
				} => {
					if self.eval(condition)?.is_truthy() {
						self.interpret(std::iter::once(*then_branch))?;
					} else if let Some(else_branch) = else_branch {
						self.interpret(std::iter::once(*else_branch))?;
					}
				}
				Stmt::While { condition, body } => {
					let body = *body;
					while self.eval(condition.clone())?.is_truthy() {
						self.interpret(std::iter::once(body.clone()))?;
					}
				}
				Stmt::Function { name, params, body } => {
					self.environment.borrow_mut().define(
						name.lexeme.clone(),
						Some(Value::Function(Rc::new(function::Function {
							declaration_name: name,
							declaration_params: params,
							declaration_body: body,
							closure: Rc::clone(&self.environment),
						}))),
					);
				}
				Stmt::Return {
					keyword: _keyword,
					value,
				} => {
					let value = self.eval(value)?;
					return Err(Error::ReturnStatement(value));
				}
			}
		}
		Ok(())
	}

	pub fn interpret_block(
		&mut self,
		statements: Vec<Stmt>,
		env: Rc<RefCell<Environment>>,
	) -> Result<(), Error> {
		let original = std::mem::replace(&mut self.environment, env);
		let result = statements
			.into_iter()
			.try_for_each(|statement| self.interpret(std::iter::once(statement)));
		self.environment = original;
		result
	}

	pub fn eval(&mut self, expr: Expr) -> Result<Value, Error> {
		match expr {
			Expr::Literal(Token {
				token_type: TokenType::Number(v),
				..
			}) => Ok(Value::Number(v)),
			Expr::Literal(Token {
				token_type: TokenType::String(v),
				..
			}) => Ok(Value::String(v)),
			Expr::Literal(Token {
				token_type: TokenType::True,
				..
			}) => Ok(Value::Bool(true)),
			Expr::Literal(Token {
				token_type: TokenType::False,
				..
			}) => Ok(Value::Bool(false)),
			Expr::Literal(Token {
				token_type: TokenType::Nil,
				..
			}) => Ok(Value::Null),
			Expr::Literal(token) => Err(Error::UnexpectedLiteral(token)),
			Expr::Variable(ref token) => self.look_up_variable(token.clone(), expr),
			ref expr @ Expr::Assign {
				ref name,
				ref value,
			} => {
				let value = self.eval(*value.clone())?;

				match self.locals.get(expr) {
					Some(distance) => {
						Environment::assign_at(
							Rc::clone(&self.environment),
							*distance,
							&name.lexeme,
							value.clone(),
						)?;
					}
					None => {
						self.globals
							.borrow_mut()
							.assign(&name.lexeme, value.clone())?;
					}
				}
				Ok(value)
			}
			Expr::Grouping(expr) => self.eval(*expr),
			Expr::Unary {
				operator: token @ Token {
					token_type: TokenType::Minus,
					..
				},
				expr,
			} => {
				let v: f64 = self
					.eval(*expr)?
					.try_into()
					.map_err(|err| Error::ExpectedNumber { err, token })?;
				Ok(Value::Number(-v))
			}
			Expr::Unary {
				operator: Token {
					token_type: TokenType::Bang,
					..
				},
				expr,
			} => {
				let v = self.eval(*expr)?;
				Ok(Value::Bool(!v.is_truthy()))
			}
			Expr::Unary { operator, .. } => Err(Error::InvalidUnaryOperator(operator)),
			Expr::Binary {
				left,
				operator: token @ Token {
					token_type: TokenType::Minus,
					..
				},
				right,
			} => {
				let left: f64 =
					self.eval(*left)?
						.try_into()
						.map_err(|err| Error::ExpectedNumber {
							err,
							token: token.clone(),
						})?;
				let right: f64 = self
					.eval(*right)?
					.try_into()
					.map_err(|err| Error::ExpectedNumber { err, token })?;
				Ok(Value::Number(left - right))
			}
			Expr::Binary {
				left,
				operator: token @ Token {
					token_type: TokenType::Slash,
					..
				},
				right,
			} => {
				let left: f64 =
					self.eval(*left)?
						.try_into()
						.map_err(|err| Error::ExpectedNumber {
							err,
							token: token.clone(),
						})?;
				let right: f64 = self
					.eval(*right)?
					.try_into()
					.map_err(|err| Error::ExpectedNumber { err, token })?;
				Ok(Value::Number(left / right))
			}
			Expr::Binary {
				left,
				operator: token @ Token {
					token_type: TokenType::Star,
					..
				},
				right,
			} => {
				let left: f64 =
					self.eval(*left)?
						.try_into()
						.map_err(|err| Error::ExpectedNumber {
							err,
							token: token.clone(),
						})?;
				let right: f64 = self
					.eval(*right)?
					.try_into()
					.map_err(|err| Error::ExpectedNumber { err, token })?;
				Ok(Value::Number(left * right))
			}
			Expr::Binary {
				left,
				operator: token @ Token {
					token_type: TokenType::Plus,
					..
				},
				right,
			} => {
				let left = self.eval(*left)?;
				let right = self.eval(*right)?;
				match (left, right) {
					(Value::String(s1), Value::String(s2)) => Ok(Value::String(s1 + &s2)),
					(Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
					(Value::String(s1), Value::Number(n2)) => {
						Ok(Value::String(s1 + &n2.to_string()))
					}
					(Value::Number(n1), Value::String(s2)) => {
						Ok(Value::String(format!("{n1}{s2}")))
					}
					(left, right) => Err(Error::InvalidPlusOperatorOperands { left, right, token }),
				}
			}
			Expr::Binary {
				left,
				operator: Token {
					token_type: TokenType::EqualEqual,
					..
				},
				right,
			} => {
				let left = self.eval(*left)?;
				let right = self.eval(*right)?;
				Ok(Value::Bool(left.is_equal(&right)))
			}
			Expr::Binary {
				left,
				operator: Token {
					token_type: TokenType::BangEqual,
					..
				},
				right,
			} => {
				let left = self.eval(*left)?;
				let right = self.eval(*right)?;
				Ok(Value::Bool(!left.is_equal(&right)))
			}
			Expr::Binary {
				left,
				operator: token @ Token {
					token_type: TokenType::Less,
					..
				},
				right,
			} => {
				let left: f64 =
					self.eval(*left)?
						.try_into()
						.map_err(|err| Error::ExpectedNumber {
							err,
							token: token.clone(),
						})?;
				let right: f64 =
					self.eval(*right)?
						.try_into()
						.map_err(|err| Error::ExpectedNumber {
							err,
							token: token.clone(),
						})?;
				Ok(Value::Bool(left < right))
			}
			Expr::Binary {
				left,
				operator: token @ Token {
					token_type: TokenType::LessEqual,
					..
				},
				right,
			} => {
				let left: f64 =
					self.eval(*left)?
						.try_into()
						.map_err(|err| Error::ExpectedNumber {
							err,
							token: token.clone(),
						})?;
				let right: f64 =
					self.eval(*right)?
						.try_into()
						.map_err(|err| Error::ExpectedNumber {
							err,
							token: token.clone(),
						})?;
				Ok(Value::Bool(left <= right))
			}
			Expr::Binary {
				left,
				operator: token @ Token {
					token_type: TokenType::Greater,
					..
				},
				right,
			} => {
				let left: f64 =
					self.eval(*left)?
						.try_into()
						.map_err(|err| Error::ExpectedNumber {
							err,
							token: token.clone(),
						})?;
				let right: f64 =
					self.eval(*right)?
						.try_into()
						.map_err(|err| Error::ExpectedNumber {
							err,
							token: token.clone(),
						})?;
				Ok(Value::Bool(left > right))
			}
			Expr::Binary {
				left,
				operator: token @ Token {
					token_type: TokenType::GreaterEqual,
					..
				},
				right,
			} => {
				let left: f64 =
					self.eval(*left)?
						.try_into()
						.map_err(|err| Error::ExpectedNumber {
							err,
							token: token.clone(),
						})?;
				let right: f64 =
					self.eval(*right)?
						.try_into()
						.map_err(|err| Error::ExpectedNumber {
							err,
							token: token.clone(),
						})?;
				Ok(Value::Bool(left >= right))
			}
			Expr::Binary { operator, .. } => Err(Error::InvalidBinaryOperator(operator)),
			Expr::Logical {
				left,
				operator: Token {
					token_type: TokenType::Or,
					..
				},
				right,
			} => {
				let left = self.eval(*left)?;
				if left.is_truthy() {
					Ok(left)
				} else {
					self.eval(*right)
				}
			}
			Expr::Logical {
				left,
				operator: Token {
					token_type: TokenType::And,
					..
				},
				right,
			} => {
				let left = self.eval(*left)?;
				if !left.is_truthy() {
					Ok(left)
				} else {
					self.eval(*right)
				}
			}
			Expr::Logical { operator, .. } => Err(Error::InvalidLogicalOperator(operator)),
			Expr::Call {
				callee,
				closing_parenthesis,
				arguments,
			} => {
				let callee = self.eval(*callee)?;

				let mut evaluted_arguments = Vec::with_capacity(arguments.len());
				for argument in arguments {
					evaluted_arguments.push(self.eval(argument)?);
				}

				let Some(function) = callee.into_callable() else {
                    return Err(Error::InvalidFunctionCallee(closing_parenthesis));
                };

				if evaluted_arguments.len() != function.arity() {
					return Err(Error::InvalidNumberOfParameters {
						expected: function.arity(),
						got: evaluted_arguments.len(),
						token: closing_parenthesis,
					});
				}

				match function.call(self, evaluted_arguments) {
					Err(Error::ReturnStatement(value)) => Ok(value),
					result => result,
				}
			}
		}
	}

	pub fn look_up_variable(&mut self, name: Token, expr: Expr) -> Result<Value, Error> {
		match self.locals.get(&expr) {
			Some(distance) => Ok(Environment::get_at(
				Rc::clone(&self.environment),
				&name,
				*distance,
			)),
			None => match self.globals.borrow().get(&name) {
				Some(Some(v)) => Ok(v),
				Some(None) => Err(Error::UninitializedVariable(name)),
				None => Err(Error::UnknownVariable(name)),
			},
		}
	}

	pub fn resolve(&mut self, expr: Expr, depth: i32) {
		self.locals.insert(expr, depth);
	}
}
