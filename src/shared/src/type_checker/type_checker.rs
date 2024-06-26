use std::collections::HashMap;

use crate::{
    parser::Statement,
    types::{TypeAnnotation, TypeIdentifier},
};

use super::{ast::TypedStatement, statements, type_environment::TypeEnvironment, Rcrc};

#[derive(Debug)]
pub enum DiscoveredType {
    Struct(TypeIdentifier, HashMap<String, TypeAnnotation>),
    Enum(
        TypeIdentifier,
        HashMap<String, HashMap<String, TypeAnnotation>>,
    ),
    Union(TypeIdentifier, Vec<TypeAnnotation>),
    Function(
        TypeIdentifier,
        HashMap<String, TypeAnnotation>,
        TypeAnnotation,
    ),
}

pub fn create_typed_ast<'a>(
    program: Statement,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<TypedStatement, String> {
    // Discover user-defined types. Only store their names and fields with type names.
    let discovered_types = statements::discover_user_defined_types(&program)?;

    // Then check the types of the entire AST.
    statements::check_type(&program, &discovered_types, type_environment)
}
