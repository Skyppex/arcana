use std::collections::HashMap;

use crate::{
    parser::{Parameter, Statement, StructData},
    types::{TypeAnnotation, TypeIdentifier},
};

use super::{ast::TypedStatement, statements, type_environment::TypeEnvironment, Rcrc};

#[derive(Debug)]
pub enum DiscoveredType {
    Struct {
        type_identifier: TypeIdentifier,
        embedded_structs: Vec<TypeAnnotation>,
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
}

pub fn create_typed_ast(
    program: Statement,
    type_environment: Rcrc<TypeEnvironment>,
) -> Result<TypedStatement, String> {
    // Discover user-defined types. Only store their names and fields with type names.
    let discovered_types = statements::discover_user_defined_types(&program)?;

    // Then check the types of the entire AST.
    statements::check_type(&program, &discovered_types, type_environment)
}
