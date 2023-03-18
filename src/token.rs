#[derive(Debug)]
pub struct Token {
	pub token_type: TokenType,
	pub lexeme: String,
	//pub literal: Box<dyn std::any::Any>,
	pub line: usize,
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
	Identifier,
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
