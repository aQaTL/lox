use std::{collections::HashMap, fmt::Display};

use crate::token::{Token, TokenType};

pub struct Scanner<'a> {
	source: &'a str,

	/// Index of start of current lexeme
	start: usize,
	/// Index of current character
	current: usize,
	/// Tracks which source line [Scanner.current] is on
	line: usize,

	keywords: HashMap<&'static str, TokenType>,
}

#[derive(Debug)]
pub enum Error {
	UnexpectedCharacter(u8),
	// I know it's kinda weird to specify comment as an "error"
	LineComment,
	Whitespace,
	UnteminatedString,
}

impl Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Error::UnexpectedCharacter(b) => write!(f, "Unexpected character {}", *b as char),
			Error::LineComment => write!(f, "Line comment"),
			Error::Whitespace => write!(f, "Whitespace"),
			Error::UnteminatedString => write!(f, "Unteminated string"),
		}
	}
}

fn keywords() -> HashMap<&'static str, TokenType> {
	[
		("and", TokenType::And),
		("class", TokenType::Class),
		("else", TokenType::Else),
		("false", TokenType::False),
		("for", TokenType::For),
		("fun", TokenType::Fun),
		("if", TokenType::If),
		("nil", TokenType::Nil),
		("or", TokenType::Or),
		("print", TokenType::Print),
		("return", TokenType::Return),
		("super", TokenType::Super),
		("this", TokenType::This),
		("true", TokenType::True),
		("var", TokenType::Var),
		("while", TokenType::While),
	]
	.into_iter()
	.collect()
}

impl<'a> Scanner<'a> {
	pub fn new(source: &'a str) -> Self {
		Self {
			source,
			start: 0,
			current: 0,
			line: 1,
			keywords: keywords(),
		}
	}

	pub fn scan_tokens(mut self) -> Vec<Token> {
		let mut tokens = Vec::new();
		while !self.is_at_end() {
			self.start = self.current;
			let token = match self.scan_token() {
				Ok(v) => v,
				Err(Error::LineComment | Error::Whitespace) => continue,
				Err(err @ Error::UnexpectedCharacter(_) | err @ Error::UnteminatedString) => {
					crate::error(self.line, err);
					continue;
				}
			};
			tokens.push(token);
		}

		tokens.push(Token {
			token_type: TokenType::Eof,
			lexeme: "".to_string(),
			//literal: Box::new(Option::<()>::None),
			line: self.line,
		});

		tokens
	}

	fn scan_token(&mut self) -> Result<Token, Error> {
		let b = self.advance();
		let token_type = match b {
			b'(' => TokenType::LeftParen,
			b')' => TokenType::RightParen,
			b'{' => TokenType::LeftBrace,
			b'}' => TokenType::RightBrace,
			b',' => TokenType::Comma,
			b'.' => TokenType::Dot,
			b'-' => TokenType::Minus,
			b'+' => TokenType::Plus,
			b';' => TokenType::Semicolon,
			b'*' => TokenType::Star,
			b'!' if self.next_matches(b'=') => TokenType::BangEqual,
			b'!' => TokenType::Bang,
			b'=' if self.next_matches(b'=') => TokenType::EqualEqual,
			b'=' => TokenType::Equal,
			b'<' if self.next_matches(b'=') => TokenType::LessEqual,
			b'<' => TokenType::Less,
			b'>' if self.next_matches(b'=') => TokenType::GreaterEqual,
			b'>' => TokenType::Greater,
			b'/' if self.next_matches(b'/') => {
				while !matches!(self.peek(), b'\n' | b'\0') {
					self.advance();
				}
				return Err(Error::LineComment);
			}
			b'/' => TokenType::Slash,
			b' ' | b'\r' | b'\t' => return Err(Error::Whitespace),
			b'\n' => {
				self.line += 1;
				return Err(Error::Whitespace);
			}
			b'"' => {
				while !matches!(self.peek(), b'"' | b'\0') {
					if self.peek() == b'\n' {
						self.line += 1;
					}
					self.advance();
				}
				if self.is_at_end() {
					return Err(Error::UnteminatedString);
				}

				self.advance(); // The closing ".

				// Trim the surrounding quotes.
				let literal_value = self.source[(self.start + 1)..(self.current - 1)].to_string();
				TokenType::String(literal_value)
			}
			b'0'..=b'9' => {
				while self.peek().is_ascii_digit() {
					self.advance();
				}
				if self.peek() == b'.' && self.peek_offset(1).is_ascii_digit() {
					self.advance();
					while self.peek().is_ascii_digit() {
						self.advance();
					}
				}

				let literal_value = &self.source[self.start..self.current];
				TokenType::Number(literal_value.parse::<f64>().unwrap())
			}
			b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
				while matches!(self.peek(), b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'0'..=b'9') {
					self.advance();
				}
				let text = &self.source[self.start..self.current];
				self.keywords
					.get(text)
					.cloned()
					.unwrap_or(TokenType::Identifier)
			}

			_ => return Err(Error::UnexpectedCharacter(b)),
		};
		Ok(Token {
			token_type,
			lexeme: self.source[self.start..self.current].to_string(),
			//literal: Box::new(Option::<()>::None),
			line: self.line,
		})
	}

	fn next_matches(&mut self, expected: u8) -> bool {
		let matches = self
			.source
			.as_bytes()
			.get(self.current)
			.copied()
			.map(|next| next == expected)
			.unwrap_or_default();
		self.current += 1;
		matches
	}

	fn advance(&mut self) -> u8 {
		let c = self.source.as_bytes()[self.current];
		self.current += 1;
		c
	}

	fn peek(&self) -> u8 {
		self.peek_offset(0)
	}

	fn peek_offset(&self, offset: usize) -> u8 {
		self.source
			.as_bytes()
			.get(self.current + offset)
			.copied()
			.unwrap_or(b'\0')
	}

	fn is_at_end(&self) -> bool {
		self.current >= self.source.len()
	}
}
