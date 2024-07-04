use std::collections::HashMap;

use crate::parser::{Assignment, Expression, VariableDeclaration};

use super::{type_equals, Type};

pub struct TypeInferenceContext {
    variables: HashMap<String, Type>,
}

impl TypeInferenceContext {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn add_variable(&mut self, name: String, type_: Type) {
        self.variables.insert(name, type_);
    }

    pub fn update_variable(&mut self, name: String, type_: Type) -> Result<(), String> {
        match self.get_variable(&name) {
            Some(t) => {
                if t != &Type::Unknown && !type_equals(t, &type_) {
                    return Err(format!(
                        "Variable {} has already been declared with type {:?}",
                        name, t
                    ));
                }

                self.variables.insert(name, type_);
                Ok(())
            }
            None => Err(format!("Variable {} has not been declared", name)),
        }
    }

    pub fn get_variable(&self, name: &str) -> Option<&Type> {
        self.variables.get(name)
    }

    pub fn infer_type(
        &mut self,
        expression: &Expression,
        _context: Option<&Type>,
    ) -> Result<Type, String> {
        match expression {
            Expression::None => Ok(Type::Unknown),
            Expression::VariableDeclaration(VariableDeclaration {
                identifier,
                initializer: Some(initializer),
                ..
            }) => {
                let inferred = self.infer_type(initializer, None)?;
                self.add_variable(identifier.clone(), inferred.clone());
                Ok(inferred)
            }
            Expression::VariableDeclaration(VariableDeclaration {
                identifier,
                initializer: None,
                ..
            }) => {
                let inferred_type = Type::Unknown;
                self.add_variable(identifier.clone(), inferred_type.clone());

                Ok(inferred_type)
            }
            Expression::If(_) => todo!(),
            Expression::Assignment(Assignment {
                member,
                initializer,
            }) => {
                let inferred = self.infer_type(initializer, None)?;

                let identifier = match *member.clone() {
                    crate::parser::Member::Identifier { symbol } => symbol,
                    crate::parser::Member::MemberAccess { symbol, .. } => symbol,
                    crate::parser::Member::ParamPropagation { symbol, .. } => symbol,
                };

                self.update_variable(identifier, inferred.clone())?;
                Ok(inferred)
            }
            Expression::Member(_) => todo!(),
            Expression::Literal(l) => match l {
                crate::parser::Literal::Unit => Ok(Type::Unit),
                crate::parser::Literal::Int(_) => Ok(Type::Int),
                crate::parser::Literal::UInt(_) => Ok(Type::UInt),
                crate::parser::Literal::Float(_) => Ok(Type::Float),
                crate::parser::Literal::String(_) => Ok(Type::String),
                crate::parser::Literal::Char(_) => Ok(Type::Char),
                crate::parser::Literal::Bool(_) => Ok(Type::Bool),
                crate::parser::Literal::Array(_) => todo!(),
                crate::parser::Literal::Struct { .. } => todo!(),
                crate::parser::Literal::Enum { .. } => todo!(),
            },
            Expression::Closure(_) => todo!(),
            Expression::Call(_) => todo!(),
            Expression::Unary(_) => todo!(),
            Expression::Binary(_) => todo!(),
            Expression::Block(_) => todo!(),
            Expression::Loop(_) => todo!(),
            Expression::While(_) => todo!(),
            Expression::Drop(_) => todo!(),
        }
    }
}
