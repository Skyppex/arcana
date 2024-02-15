use super::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Scope {
    Break(Option<Type>),
    Return(Option<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeType {
    Break,
    Return,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopeState {
    pub scope: Scope,
    pub scope_type: ScopeType,
    pub active: bool,
}

impl Into<Scope> for ScopeType {
    fn into(self) -> Scope {
        match self {
            ScopeType::Break => Scope::Break(None),
            ScopeType::Return => Scope::Return(None),
        }
    }

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

impl Into<ScopeState> for ScopeType {
    fn into(self) -> ScopeState {
        <ScopeType as Into<Scope>>::into(self).into()
    }
}

impl Into<ScopeType> for Scope {
    fn into(self) -> ScopeType {
        match self {
            Scope::Break(_) => ScopeType::Break,
            Scope::Return(_) => ScopeType::Return,
        }
    }
}