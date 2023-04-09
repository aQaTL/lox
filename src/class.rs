use crate::token::Token;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct Class {
	pub name: Token,
}

impl Display for Class {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		write!(f, "{}", self.name.lexeme)
	}
}
