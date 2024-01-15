use super::value::Variable;

pub struct Environment {
    pub parent: Option<Box<Environment>>,
    pub variables: Vec<Variable>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            parent: None,
            variables: Vec::new(),
        }
    }

    pub fn new_with_parent(parent: Box<Environment>) -> Self {
        Self {
            parent: Some(parent),
            variables: Vec::new(),
        }
    }
}