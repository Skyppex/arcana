use std::collections::HashMap;

use crate::{parser::Statement, types::{TypeAnnotation, TypeName}};

use super::{ast::TypedStatement, statements, type_environment::TypeEnvironment, Rcrc};

pub enum DiscoveredType {
    Struct(TypeName, HashMap<String, TypeAnnotation>),
    Union(TypeName, HashMap<String, HashMap<String, TypeAnnotation>>),
    Function(TypeName, HashMap<String, TypeAnnotation>, TypeAnnotation),
}

pub fn create_typed_ast<'a>(program: Statement, type_environment: Rcrc<TypeEnvironment>) -> Result<TypedStatement, String> {
    // Discover user-defined types. Only store their names and fields with type names.
    let discovered_types = statements::discover_user_defined_types(&program)?;

    // Then check the types of the entire AST.
    statements::check_type(&program, &discovered_types, type_environment)
}