use std::collections::HashMap;

use crate::parser::Statement;

use super::{statements, ast::TypedStatement, type_environment::TypeEnvironment};

pub enum DiscoveredType {
    Struct(String, HashMap<String, String>),
    Union(String, HashMap<String, HashMap<Option<String>, String>>),
    Function(String, HashMap<String, String>, String),
}

pub fn create_typed_ast<'a>(program: Statement, type_environment: &mut TypeEnvironment<'a>) -> Result<TypedStatement, String> {
    // Discover user-defined types. Only store their names and fields with type names.
    let discovered_types = statements::discover_user_defined_types(&program)?;

    // Then check the types of the entire AST.
    statements::check_type(&program, &discovered_types, type_environment)
}