use std::collections::HashMap;

use crate::parser::Statement;

use super::{ast::TypedStatement, statements, type_environment::TypeEnvironment, Rcrc};

pub enum DiscoveredType {
    Struct(String, HashMap<String, String>),
    Union(String, HashMap<String, HashMap<String, String>>),
    Function(String, HashMap<String, String>, String),
}

pub fn create_typed_ast<'a>(program: Statement, type_environment: Rcrc<TypeEnvironment>) -> Result<TypedStatement, String> {
    // Discover user-defined types. Only store their names and fields with type names.
    let discovered_types = statements::discover_user_defined_types(&program)?;

    // Then check the types of the entire AST.
    statements::check_type(&program, &discovered_types, type_environment)
}