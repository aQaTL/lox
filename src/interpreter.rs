use std::fmt::Display;

use crate::{
	parser::{Expr, Stmt},
	token::{Token, TokenType},
};

pub struct Interpreter {}

#[derive(Debug)]
pub enum Value {
	Null,
	Bool(bool),
	Number(f64),
	String(String),
	/// TODO(aqatl): objects
	Object,
}

impl Value {
	fn type_name(&self) -> &'static str {
		match self {
			Value::Null => "Null",
			Value::Bool(_) => "Bool",
			Value::Number(_) => "Number",
			Value::String(_) => "String",
			Value::Object => "Object",
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
}

impl Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Value::Null => write!(f, "null"),
			Value::Bool(v) => write!(f, "{v}"),
			Value::Number(n) => write!(f, "{n}"),
			Value::String(s) => write!(f, "{s}"),
			Value::Object => write!(f, "{{}}"),
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
}

impl Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Error::UnexpectedLiteral(Token {
				token_type,
				lexeme,
				line,
			}) => {
				write!(f, "[line {line}] unexpeted {token_type:?} `{lexeme}`")
			}
			Error::InvalidUnaryOperator(Token {
				token_type,
				lexeme,
				line,
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
			}) => write!(
				f,
				"[line {line}] Invalid binary operator {token_type:?} at `{lexeme}`"
			),
		}
	}
}

impl Interpreter {
	pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), Error> {
		for statement in statements {
			match statement {
				Stmt::Print(expr) => {
					let v = self.eval(expr)?;
					println!("{v}");
				}
				Stmt::Expr(expr) => {
					self.eval(expr)?;
				}
			}
		}
		Ok(())
	}

	#[allow(clippy::only_used_in_recursion)]
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
		}
	}
}
