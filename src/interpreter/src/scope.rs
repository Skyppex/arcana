use super::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Scope {
    Break(Option<Value>),
    Continue,
    Return(Option<Value>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeType {
    Break,
    Continue,
    Return,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopeState {
    pub scope: Scope,
    pub scope_type: ScopeType,
    pub active: bool,
}

impl From<ScopeType> for Scope {
    fn from(val: ScopeType) -> Self {
        match val {
            ScopeType::Break => Scope::Break(None),
            ScopeType::Continue => Scope::Continue,
            ScopeType::Return => Scope::Return(None),
        }
    }
}

impl From<Scope> for ScopeState {
    fn from(val: Scope) -> Self {
        ScopeState {
            scope_type: val.clone().into(),
            scope: val,
            active: false,
        }
    }
}

impl From<ScopeType> for ScopeState {
    fn from(val: ScopeType) -> Self {
        <ScopeType as Into<Scope>>::into(val).into()
    }
}

impl From<Scope> for ScopeType {
    fn from(val: Scope) -> Self {
        match val {
            Scope::Break(_) => ScopeType::Break,
            Scope::Continue => ScopeType::Continue,
            Scope::Return(_) => ScopeType::Return,
        }
    }
}
