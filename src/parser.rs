#![allow(clippy::while_let_loop)]

use std::fmt::Display;

use crate::token::{Token, TokenType};

pub struct Parser {
	tokens: std::iter::Peekable<std::vec::IntoIter<Token>>,
}

#[derive(Debug)]
pub struct Error {
	pub kind: ErrorKind,
	pub token: Option<Token>,
}

#[derive(Debug)]
pub enum ErrorKind {
	ExpectedExpression,
	ExpectedRightParenthesis,
}

impl Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match &self.token {
			Some(Token { line, .. }) => write!(f, "[line {line}] ")?,
			None => write!(f, "[line unknown] ")?,
		}
		match self.kind {
			ErrorKind::ExpectedExpression => write!(f, "expected expression")?,
			ErrorKind::ExpectedRightParenthesis => write!(f, "expected `)` after expression")?,
		}
		match &self.token {
			None
			| Some(Token {
				token_type: TokenType::Eof,
				..
			}) => write!(f, " at the end")?,
			Some(Token { lexeme, .. }) => write!(f, " at `{lexeme}`")?,
		}
		Ok(())
	}
}

impl std::error::Error for Error {}

impl Parser {
	pub fn new(tokens: Vec<Token>) -> Self {
		Parser {
			tokens: tokens.into_iter().peekable(),
		}
	}

	pub fn parse(mut self) -> Result<Expr, Error> {
		self.expression()
	}

	fn expression(&mut self) -> Result<Expr, Error> {
		self.equality()
	}

	fn equality(&mut self) -> Result<Expr, Error> {
		let mut expr = self.comparison()?;
		loop {
			let operator = match self.tokens.peek().map(|t| &t.token_type) {
				Some(TokenType::BangEqual | TokenType::EqualEqual) => self.tokens.next().unwrap(),
				_ => break,
			};

			expr = Expr::Binary {
				left: Box::new(expr),
				operator,
				right: Box::new(self.comparison()?),
			};
		}

		Ok(expr)
	}

	fn comparison(&mut self) -> Result<Expr, Error> {
		let mut expr = self.term()?;

		loop {
			let operator = match self.tokens.peek().map(|t| &t.token_type) {
				Some(
					TokenType::Greater
					| TokenType::GreaterEqual
					| TokenType::Less
					| TokenType::LessEqual,
				) => self.tokens.next().unwrap(),
				_ => break,
			};

			expr = Expr::Binary {
				left: Box::new(expr),
				operator,
				right: Box::new(self.term()?),
			};
		}

		Ok(expr)
	}

	fn term(&mut self) -> Result<Expr, Error> {
		let mut expr = self.factor()?;

		loop {
			let operator = match self.tokens.peek().map(|t| &t.token_type) {
				Some(TokenType::Minus | TokenType::Plus) => self.tokens.next().unwrap(),
				_ => break,
			};

			expr = Expr::Binary {
				left: Box::new(expr),
				operator,
				right: Box::new(self.factor()?),
			};
		}

		Ok(expr)
	}

	fn factor(&mut self) -> Result<Expr, Error> {
		let mut expr = self.unary()?;

		loop {
			let operator = match self.tokens.peek().map(|t| &t.token_type) {
				Some(TokenType::Slash | TokenType::Star) => self.tokens.next().unwrap(),
				_ => break,
			};

			expr = Expr::Binary {
				left: Box::new(expr),
				operator,
				right: Box::new(self.unary()?),
			};
		}

		Ok(expr)
	}

	fn unary(&mut self) -> Result<Expr, Error> {
		let token = self.tokens.peek().ok_or(Error {
			kind: ErrorKind::ExpectedExpression,
			token: None,
		})?;

		match token.token_type {
			TokenType::Bang | TokenType::Minus => {
				let token = self.tokens.next().unwrap();
				Ok(Expr::Unary {
					operator: token,
					expr: Box::new(self.unary()?),
				})
			}
			_ => self.primary(),
		}
	}

	fn primary(&mut self) -> Result<Expr, Error> {
		let token = self.tokens.next().ok_or(Error {
			kind: ErrorKind::ExpectedExpression,
			token: None,
		})?;

		match token.token_type {
			TokenType::Number(_)
			| TokenType::String(_)
			| TokenType::True
			| TokenType::False
			| TokenType::Nil => Ok(Expr::Literal(token)),
			TokenType::LeftParen => {
				let expr = self.expression()?;
				match self.tokens.next() {
					Some(Token {
						token_type: TokenType::RightParen,
						..
					}) => (),
					Some(token) => {
						return Err(Error {
							kind: ErrorKind::ExpectedRightParenthesis,
							token: Some(token),
						})
					}
					None => {
						return Err(Error {
							kind: ErrorKind::ExpectedRightParenthesis,
							token: None,
						})
					}
				}
				Ok(Expr::Grouping(Box::new(expr)))
			}
			_ => Err(Error {
				kind: ErrorKind::ExpectedExpression,
				token: Some(token),
			}),
		}
	}

	#[allow(dead_code)]
	fn synchronize(&mut self) {
		while let Some(token) = self.tokens.next() {
			if matches!(token.token_type, TokenType::Semicolon) {
				return;
			}

			let Some(token) = self.tokens.peek() else {
                return;
            };
			match token.token_type {
				TokenType::Class
				| TokenType::For
				| TokenType::Fun
				| TokenType::If
				| TokenType::Print
				| TokenType::Return
				| TokenType::Var
				| TokenType::While => {
					return;
				}
				_ => (),
			}
		}
	}
}

#[derive(Debug)]
pub enum Expr {
	Literal(Token),
	Unary {
		operator: Token,
		expr: Box<Expr>,
	},
	Binary {
		left: Box<Expr>,
		operator: Token,
		right: Box<Expr>,
	},
	Grouping(Box<Expr>),
}

impl Display for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		print_ast(self, f)
	}
}

fn print_ast(expr: &Expr, w: &mut impl std::fmt::Write) -> std::fmt::Result {
	fn parenthesize(w: &mut impl std::fmt::Write, name: &str, exprs: &[&Expr]) -> std::fmt::Result {
		write!(w, "({name}")?;
		for expr in exprs {
			write!(w, " ")?;
			print_ast(expr, w)?;
		}
		write!(w, ")")?;
		Ok(())
	}

	match expr {
		Expr::Literal(Token {
			token_type: TokenType::Number(v),
			..
		}) => write!(w, "{v}"),
		Expr::Literal(Token {
			token_type: TokenType::String(v),
			..
		}) => write!(w, "{v}"),
		Expr::Literal(Token {
			token_type: TokenType::Identifier,
			lexeme,
			..
		}) => write!(w, "{lexeme}"),
		Expr::Literal(l) => panic!("{l:?}"),
		Expr::Binary {
			left,
			operator: Token { lexeme, .. },
			right,
		} => parenthesize(w, lexeme, &[left, right]),
		Expr::Grouping(expr) => parenthesize(w, "group", &[expr]),
		Expr::Unary { operator, expr } => parenthesize(w, &operator.lexeme, &[expr]),
	}
}

#[cfg(test)]
mod tests {
	use super::Expr;
	use crate::token::{Token, TokenType};

	#[test]
	fn test_ast_printer() {
		let expr = Expr::Binary {
			left: Box::new(Expr::Unary {
				operator: Token {
					token_type: TokenType::Minus,
					lexeme: "-".to_string(),
					line: 1,
				},
				expr: Box::new(Expr::Literal(Token {
					token_type: TokenType::Number(123.0),
					lexeme: "123".to_string(),
					line: 1,
				})),
			}),
			operator: Token {
				token_type: TokenType::Star,
				lexeme: "*".to_string(),
				line: 1,
			},
			right: Box::new(Expr::Grouping(Box::new(Expr::Literal(Token {
				token_type: TokenType::Number(45.67),
				lexeme: "45.67".to_string(),
				line: 1,
			})))),
		};

		let expected = "(* (- 123) (group 45.67))";

		let mut actual = String::new();
		super::print_ast(&expr, &mut actual).unwrap();

		assert_eq!(expected, actual);
	}
}
