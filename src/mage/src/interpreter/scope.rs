use super::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Scope {
    Break(Option<Value>),
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeType {
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopeState {
    pub scope: Scope,
    pub scope_type: ScopeType,
    pub active: bool,
}

impl Into<ScopeState> for Scope {
    fn into(self) -> ScopeState {
        ScopeState {
            scope_type: self.clone().into(),
            scope: self,
            active: false,
        }
    }
}

impl Into<ScopeType> for Scope {
    fn into(self) -> ScopeType {
        match self {
            Scope::Break(_) => ScopeType::Break,
            Scope::Continue => ScopeType::Continue,
        }
    }
}