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

#[allow(clippy::enum_variant_names)]
#[derive(Debug)]
pub enum ErrorKind {
	ExpectedExpression,
	ExpectedRightParenthesis,
	ExpectedSemicolon,
	ExpectedIdentifier,
	InvalidAssignmentTarget,
	ExpectedRightBrace,
	ExpectedLeftParenthesis,
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
			ErrorKind::ExpectedSemicolon => write!(f, "expected `;` after statement")?,
			ErrorKind::ExpectedIdentifier => write!(f, "expected identifier")?,
			ErrorKind::InvalidAssignmentTarget => write!(f, "invalid assignment target")?,
			ErrorKind::ExpectedRightBrace => write!(f, "expected `}}` at the end of a block")?,
			ErrorKind::ExpectedLeftParenthesis => write!(f, "expected `(`")?,
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

macro_rules! expect_token {
	($parser:ident, $pattern:pat) => {{
		match $parser.tokens.next() {
			Some(token) if matches!(token, $pattern) => Ok(token),
			token => Err(token),
		}
	}};
}

impl Parser {
	pub fn new(tokens: Vec<Token>) -> Self {
		Parser {
			tokens: tokens.into_iter().peekable(),
		}
	}

	pub fn parse(mut self) -> Result<Vec<Stmt>, Error> {
		let mut statements = Vec::new();
		while self
			.tokens
			.peek()
			.map(|t| !matches!(t.token_type, TokenType::Eof))
			.unwrap_or_default()
		{
			//TODO(aqatl): if this fails, we should call [self.synchronize]
			let declaration = self.declaration()?;
			statements.push(declaration);
		}
		Ok(statements)
	}

	fn declaration(&mut self) -> Result<Stmt, Error> {
		match self.tokens.peek() {
			Some(Token {
				token_type: TokenType::Var,
				..
			}) => {
				let _ = self.tokens.next().unwrap();
				self.var_declaration()
			}
			_ => self.statement(),
		}
	}

	fn var_declaration(&mut self) -> Result<Stmt, Error> {
		let name = match self.tokens.next() {
			Some(
				t @ Token {
					token_type: TokenType::Identifier(_),
					..
				},
			) => t,
			t => {
				return Err(Error {
					kind: ErrorKind::ExpectedIdentifier,
					token: t,
				})
			}
		};

		let initializer = match self.tokens.next() {
			Some(Token {
				token_type: TokenType::Equal,
				..
			}) => Some(self.expression()?),
			Some(Token {
				token_type: TokenType::Semicolon,
				..
			}) => None,
			token => {
				return Err(Error {
					kind: ErrorKind::ExpectedSemicolon,
					token,
				})
			}
		};

		if initializer.is_some() {
			match self.tokens.next() {
				Some(Token {
					token_type: TokenType::Semicolon,
					..
				}) => (),
				t => {
					return Err(Error {
						kind: ErrorKind::ExpectedSemicolon,
						token: t,
					})
				}
			}
		}

		Ok(Stmt::Var { name, initializer })
	}

	fn statement(&mut self) -> Result<Stmt, Error> {
		match self.tokens.peek().map(|t| &t.token_type) {
			Some(TokenType::If) => {
				let _ = self.tokens.next().unwrap();
				self.if_statement()
			}
			Some(TokenType::Print) => {
				let _ = self.tokens.next().unwrap();
				self.print_statement()
			}
			Some(TokenType::LeftBrace) => {
				let _ = self.tokens.next().unwrap();
				self.block().map(Stmt::Block)
			}
			_ => self.expression_statement(),
		}
	}

	fn if_statement(&mut self) -> Result<Stmt, Error> {
		expect_token!(
			self,
			Token {
				token_type: TokenType::LeftParen,
				..
			}
		)
		.map_err(|token| Error {
			kind: ErrorKind::ExpectedLeftParenthesis,
			token,
		})?;
		let condition = self.expression()?;
		expect_token!(
			self,
			Token {
				token_type: TokenType::RightParen,
				..
			}
		)
		.map_err(|token| Error {
			kind: ErrorKind::ExpectedRightParenthesis,
			token,
		})?;

		let then_branch = Box::new(self.statement()?);
		let else_branch = match self.tokens.peek() {
			Some(Token {
				token_type: TokenType::Else,
				..
			}) => {
				let _ = self.tokens.next();
				Some(Box::new(self.statement()?))
			}
			_ => None,
		};

		Ok(Stmt::If {
			condition,
			then_branch,
			else_branch,
		})
	}

	fn print_statement(&mut self) -> Result<Stmt, Error> {
		let value = self.expression()?;
		//TODO(aqatl): report token
		let token = self.tokens.next().ok_or(Error {
			kind: ErrorKind::ExpectedSemicolon,
			token: None,
		})?;
		if !matches!(token.token_type, TokenType::Semicolon) {
			return Err(Error {
				kind: ErrorKind::ExpectedSemicolon,
				token: None,
			});
		}
		Ok(Stmt::Print(value))
	}

	fn block(&mut self) -> Result<Vec<Stmt>, Error> {
		let mut statements = Vec::new();

		loop {
			match self.tokens.peek() {
				Some(Token {
					token_type: TokenType::RightBrace,
					..
				})
				| None => {
					break;
				}
				_ => (),
			}
			let statement = self.declaration()?;
			statements.push(statement);
		}

		let token = self.tokens.next();
		if !matches!(
			token,
			Some(Token {
				token_type: TokenType::RightBrace,
				..
			})
		) {
			return Err(Error {
				kind: ErrorKind::ExpectedRightBrace,
				token,
			});
		}

		Ok(statements)
	}

	fn expression_statement(&mut self) -> Result<Stmt, Error> {
		let expr = self.expression()?;
		let token = self.tokens.next().ok_or(Error {
			kind: ErrorKind::ExpectedSemicolon,
			token: None,
		})?;
		if !matches!(token.token_type, TokenType::Semicolon) {
			return Err(Error {
				kind: ErrorKind::ExpectedSemicolon,
				token: None,
			});
		}
		Ok(Stmt::Expr(expr))
	}

	fn expression(&mut self) -> Result<Expr, Error> {
		self.assignment()
	}

	fn assignment(&mut self) -> Result<Expr, Error> {
		let expr = self.equality()?;

		if let Some(Token {
			token_type: TokenType::Equal,
			..
		}) = self.tokens.peek()
		{
			let equals = self.tokens.next();
			let value = self.assignment()?;
			match expr {
				Expr::Variable(name) => Ok(Expr::Assign {
					name,
					value: Box::new(value),
				}),
				_ => Err(Error {
					kind: ErrorKind::InvalidAssignmentTarget,
					token: equals,
				}),
			}
		} else {
			Ok(expr)
		}
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
			TokenType::Identifier(_) => Ok(Expr::Variable(token)),
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

pub enum Stmt {
	Expr(Expr),
	Print(Expr),
	Var {
		name: Token,
		initializer: Option<Expr>,
	},
	Block(Vec<Stmt>),
	If {
		condition: Expr,
		then_branch: Box<Stmt>,
		else_branch: Option<Box<Stmt>>,
	},
}

#[derive(Debug)]
pub enum Expr {
	Literal(Token),
	Variable(Token),
	Assign {
		name: Token,
		value: Box<Expr>,
	},
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
			token_type: TokenType::Identifier(v),
			..
		}) => write!(w, "{v}"),
		Expr::Literal(Token {
			token_type: TokenType::True,
			..
		}) => write!(w, "true"),
		Expr::Literal(Token {
			token_type: TokenType::False,
			..
		}) => write!(w, "false"),
		Expr::Literal(Token {
			token_type: TokenType::Nil,
			..
		}) => write!(w, "nil"),
		Expr::Literal(l) => panic!("{l:?}"),
		Expr::Variable(Token {
			token_type: TokenType::Identifier(var_name),
			..
		}) => write!(w, "{var_name}"),
		Expr::Variable(v) => panic!("{v:?}"),
		Expr::Assign {
			name: Token {
				token_type: TokenType::Identifier(name),
				..
			},
			value,
		} => parenthesize(w, &format!("= {name}"), &[value]),
		Expr::Assign { name, .. } => panic!("{name:?}"),
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
