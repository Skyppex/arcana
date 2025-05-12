use std::collections::HashMap;

use crate::{
    ast::{Expression, Parameter, Statement, StructData},
    types::{TypeAnnotation, TypeIdentifier},
};

use super::{model::TypedStatement, statements, type_environment::TypeEnvironment, Rcrc};

#[derive(Debug, Clone, PartialEq)]
pub struct DiscoveredEmbeddedStruct {
    pub type_annotation: TypeAnnotation,
    pub initialized_fields: Vec<(String, Expression)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DiscoveredType {
    Struct {
        type_identifier: TypeIdentifier,
        embedded_structs: Vec<DiscoveredEmbeddedStruct>,
        fields: HashMap<String, TypeAnnotation>,
    },
    Enum {
        type_identifier: TypeIdentifier,
        shared_fields: HashMap<String, TypeAnnotation>,
        members: Vec<StructData>,
    },
    Union(TypeIdentifier, Vec<TypeAnnotation>),
    TypeAlias(TypeIdentifier, Vec<TypeAnnotation>),
    Protocol {
        type_identifier: TypeIdentifier,
        associated_types: Vec<TypeIdentifier>,
        function_identifiers: Vec<TypeIdentifier>,
    },
    Function {
        type_identifier: TypeIdentifier,
        param: Option<Parameter>,
        return_type_annotation: TypeAnnotation,
    },
    UseItem {
        type_identifier: TypeIdentifier,
    },
}

pub fn discover_user_defined_types(
    program: Statement,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<Vec<DiscoveredType>, String> {
    // Discover user-defined types. Only store their names and fields with type names.
    let discovered_types = statements::discover_user_defined_types(&program)?;

    type_environment
        .borrow_mut()
        .set_discovered_types(discovered_types.clone());

    Ok(discovered_types)
}

pub fn type_check_program(
    program: &Statement,
    discovered_types: &Vec<DiscoveredType>,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<TypedStatement, String> {
    statements::check_type(program, discovered_types, type_environment)
}

pub fn create_typed_ast(
    program: Statement,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<TypedStatement, String> {
    // Discover user-defined types. Only store their names and fields with type names.
    let discovered_types = statements::discover_user_defined_types(&program)?;

    type_environment
        .borrow_mut()
        .set_discovered_types(discovered_types.clone());

    // Then check the types of the entire AST.
    statements::check_type(&program, &discovered_types, type_environment)
}
