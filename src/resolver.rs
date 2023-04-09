use crate::parser::{Expr, FunctionStatement, Stmt};
use crate::token::{Token, TokenType};
use crate::Interpreter;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

pub struct Resolver<'a> {
	interpreter: &'a mut Interpreter,
	scopes: Vec<HashMap<String, InitializerResolving>>,

	current_function: FunctionType,
	current_class: ClassType,
}

enum InitializerResolving {
	Finished,
	InProgress,
}

#[derive(Copy, Clone)]
enum FunctionType {
	None,
	Function,
	Method,
	Initializer,
}

#[derive(Copy, Clone)]
enum ClassType {
	None,
	Class,
}

#[derive(Debug)]
pub enum Error {
	VariableReadFromItsInitializer,
	VariableAlreadyExists(Token),
	ReturnFromGlobalScope(Token),
	ThisKeywordOutsideClass(Token),
	ReturnValueFromInitializer(Token),
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
		match self {
			Error::VariableReadFromItsInitializer => {
				write!(f, "Cant read local variable in its own initializer.")
			}
			Error::VariableAlreadyExists(Token { lexeme, .. }) => {
				write!(f, "Variable {lexeme} already exists in this scope.")
			}
			Error::ReturnFromGlobalScope(Token { line, .. }) => {
				write!(f, "[line {line}] Can't return from global scope.")
			}
			Error::ThisKeywordOutsideClass(Token { line, .. }) => {
				write!(f, "[line {line}] Can't use `this` outside of a class.")
			}
			Error::ReturnValueFromInitializer(Token { line, .. }) => {
				write!(f, "[line {line}] Can't return a value from an initializer.")
			}
		}
	}
}

impl std::error::Error for Error {}

impl<'a> Resolver<'a> {
	pub fn new(interpreter: &'a mut Interpreter) -> Self {
		Resolver {
			interpreter,
			scopes: Vec::default(),
			current_function: FunctionType::None,
			current_class: ClassType::None,
		}
	}

	pub fn resolve_statements(
		&mut self,
		statements: impl IntoIterator<Item = Stmt>,
	) -> Result<(), Error> {
		for statement in statements {
			match statement {
				Stmt::Expr(expr) => self.resolve_expr(expr)?,
				Stmt::Print(expr) => self.resolve_expr(expr)?,
				Stmt::Var { name, initializer } => {
					self.declare(name.clone())?;
					if let Some(initializer) = initializer {
						self.resolve_expr(initializer)?;
					}
					self.define(name);
				}
				Stmt::Block(statements) => {
					self.begin_scope();
					self.resolve_statements(statements)?;
					self.end_scope();
				}
				Stmt::If {
					condition,
					then_branch,
					else_branch,
				} => {
					self.resolve_expr(condition)?;
					self.resolve_statements(std::iter::once(*then_branch))?;
					if let Some(else_branch) = else_branch {
						self.resolve_statements(std::iter::once(*else_branch))?;
					}
				}
				Stmt::While { condition, body } => {
					self.resolve_expr(condition)?;
					self.resolve_statements(std::iter::once(*body))?;
				}
				Stmt::Function(function) => {
					self.declare(function.name.clone())?;
					self.define(function.name.clone());
					self.resolve_function(function, FunctionType::Function)?;
				}
				Stmt::Return { keyword, value } => {
					if matches!(self.current_function, FunctionType::None) {
						return Err(Error::ReturnFromGlobalScope(keyword));
					}

					if !matches!(
						value,
						Expr::Literal(Token {
							token_type: TokenType::Nil,
							..
						})
					) {
						if let FunctionType::Initializer = self.current_function {
							return Err(Error::ReturnValueFromInitializer(keyword));
						}

						self.resolve_expr(value)?;
					}
				}
				Stmt::Class { name, methods } => {
					let enclosing_class = self.current_class;
					self.current_class = ClassType::Class;

					self.declare(name.clone())?;
					self.define(name);

					self.begin_scope();

					self.scopes
						.last_mut()
						.unwrap()
						.insert("this".to_string(), InitializerResolving::Finished);

					for method in methods {
						let kind = if method.name.lexeme == "init" {
							FunctionType::Initializer
						} else {
							FunctionType::Method
						};
						self.resolve_function(method, kind)?;
					}

					self.end_scope();

					self.current_class = enclosing_class;
				}
			}
		}
		Ok(())
	}

	fn resolve_expr(&mut self, expr: Expr) -> Result<(), Error> {
		match expr {
			Expr::Literal(_token) => {}
			Expr::Variable(ref token) => {
				if let Some(scope) = self.scopes.last() {
					if let Some(InitializerResolving::InProgress) = scope.get(&token.lexeme) {
						return Err(Error::VariableReadFromItsInitializer);
					}
				}

				let name = token.clone();
				self.resolve_local(expr, name);
			}
			Expr::Assign {
				ref name,
				ref value,
			} => {
				let name = name.clone();
				self.resolve_expr(*value.clone())?;
				self.resolve_local(expr, name);
			}
			Expr::Unary {
				operator: _operator,
				expr,
			} => {
				self.resolve_expr(*expr)?;
			}
			Expr::Binary {
				left,
				operator: _operator,
				right,
			} => {
				self.resolve_expr(*left)?;
				self.resolve_expr(*right)?;
			}
			Expr::Grouping(expr) => self.resolve_expr(*expr)?,
			Expr::Logical {
				left,
				operator: _operator,
				right,
			} => {
				self.resolve_expr(*left)?;
				self.resolve_expr(*right)?;
			}
			Expr::Call {
				callee,
				closing_parenthesis: _closing_parenthesis,
				arguments,
			} => {
				self.resolve_expr(*callee)?;
				for argument in arguments {
					self.resolve_expr(argument)?;
				}
			}
			Expr::Get {
				object,
				name: _name,
			} => {
				self.resolve_expr(*object)?;
			}
			Expr::Set {
				object,
				name: _name,
				value,
			} => {
				self.resolve_expr(*value)?;
				self.resolve_expr(*object)?;
			}
			Expr::This { ref keyword } => {
				let keyword = keyword.clone();
				if let ClassType::None = self.current_class {
					return Err(Error::ThisKeywordOutsideClass(keyword));
				}
				self.resolve_local(expr, keyword);
			}
		}
		Ok(())
	}

	fn resolve_local(&mut self, expr: Expr, name: Token) {
		for (depth, scope) in self.scopes.iter_mut().rev().enumerate() {
			if scope.contains_key(&name.lexeme) {
				self.interpreter.resolve(expr, depth as i32);
				return;
			}
		}
	}

	fn resolve_function(
		&mut self,
		FunctionStatement { params, body, .. }: FunctionStatement,
		kind: FunctionType,
	) -> Result<(), Error> {
		let enclosing_function = self.current_function;
		self.current_function = kind;

		self.begin_scope();
		for param in params {
			self.declare(param.clone())?;
			self.define(param);
		}
		self.resolve_statements(body)?;
		self.end_scope();

		self.current_function = enclosing_function;

		Ok(())
	}

	fn declare(&mut self, name: Token) -> Result<(), Error> {
		let Some(ref mut scope) = self.scopes.last_mut() else {
			return Ok(());
		};
		if scope.contains_key(&name.lexeme) {
			return Err(Error::VariableAlreadyExists(name));
		}
		scope.insert(name.lexeme, InitializerResolving::InProgress);
		Ok(())
	}

	fn define(&mut self, name: Token) {
		let Some(ref mut scope) = self.scopes.last_mut() else {
			return;
		};
		scope.insert(name.lexeme, InitializerResolving::Finished);
	}

	fn begin_scope(&mut self) {
		self.scopes.push(HashMap::new());
	}

	fn end_scope(&mut self) {
		self.scopes.pop();
	}
}
