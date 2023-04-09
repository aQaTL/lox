use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicU64, Ordering};

#[derive(Debug, Clone)]
pub struct Token {
	pub token_type: TokenType,
	pub lexeme: String,
	//pub literal: Box<dyn std::any::Any>,
	pub line: usize,
	pub universal_index: u64,
}

pub fn next_universal_index() -> u64 {
	static UNIVERSAL_INDEX: AtomicU64 = AtomicU64::new(0);
	UNIVERSAL_INDEX.fetch_add(1, Ordering::Relaxed)
}

impl PartialEq for Token {
	fn eq(&self, other: &Self) -> bool {
		self.universal_index == other.universal_index
	}
}

impl Eq for Token {}

impl Hash for Token {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.universal_index.hash(state)
	}
}

#[derive(Debug, Clone)]
pub enum TokenType {
	// Single-character tokens.
	LeftParen,
	RightParen,
	LeftBrace,
	RightBrace,
	Comma,
	Dot,
	Minus,
	Plus,
	Semicolon,
	Slash,
	Star,

	// One or two character tokens.
	Bang,
	BangEqual,
	Equal,
	EqualEqual,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,

	// Literals.
	Identifier(String),
	String(String),
	Number(f64),

	// Keywords.
	And,
	Class,
	Else,
	False,
	Fun,
	For,
	If,
	Nil,
	Or,
	Print,
	Return,
	Super,
	This,
	True,
	Var,
	While,

	Eof,
}

/*
#[derive(Debug, Clone)]
pub enum BetterTokenType {
	Literal(Literal),
	Keyword(Keyword),

	// Single-character tokens.
	LeftParen,
	RightParen,
	LeftBrace,
	RightBrace,
	Comma,
	Dot,
	Minus,
	Plus,
	Semicolon,
	Slash,
	Star,

	// One or two character tokens.
	Bang,
	BangEqual,
	Equal,
	EqualEqual,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,

	Eof,
}

#[derive(Debug, Clone)]
pub enum Literal {
	Identifier,
	String(String),
	Number(f64),
}

#[derive(Debug, Clone)]
pub enum Keyword {
	And,
	Class,
	Else,
	False,
	Fun,
	For,
	If,
	Nil,
	Or,
	Print,
	Return,
	Super,
	This,
	True,
	Var,
	While,
}
*/
