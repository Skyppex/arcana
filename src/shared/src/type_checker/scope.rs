use std::{fmt::Display, vec};

use super::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    pub scope_type: ScopeType,
    pub types: Vec<Type>,
}

impl Scope {
    pub fn active(&self) -> bool {
        !self.types.is_empty()
    }

    pub fn fold(&self) -> Result<Type, String> {
        let type_ = self.types.iter().fold(Ok(Type::Void), |acc, t| {
            let acc = acc?;

            if acc == Type::Void || &acc == t {
                Ok(t.clone())
            } else {
                Err(format!("Type mismatch: {} != {}", acc, t))
            }
        })?;

        Ok(type_)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeType {
    Break,
    Return,
}

impl Into<Scope> for ScopeType {
    fn into(self) -> Scope {
        Scope {
            scope_type: self,
            types: vec![],
        }
    }
}

impl Display for ScopeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ScopeType::Break => write!(f, "break"),
            ScopeType::Return => write!(f, "return"),
        }
    }
}
