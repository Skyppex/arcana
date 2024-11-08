use std::{fmt::Display, vec};

use super::{type_equals_coerce, Type};

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
        let type_ = self.types.iter().try_fold(Type::Void, |acc, t| {
            if acc == Type::Void || type_equals_coerce(&acc, t) {
                Ok(t.clone())
            } else {
                Err(format!("Type mismatch in scope: {} != {}", acc, t))
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

impl From<ScopeType> for Scope {
    fn from(val: ScopeType) -> Self {
        Scope {
            scope_type: val,
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
