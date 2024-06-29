use std::{fmt::Display, hash::Hash};

use crate::{
    lexer::token::{self, Keyword, TokenKind},
    parser::{cursor::Cursor, Literal},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeAnnotation {
    Type(String),
    ConcreteType(String, Vec<TypeAnnotation>),
    Array(Box<TypeAnnotation>),
    Literal(Box<Literal>),
    Function(Vec<TypeAnnotation>, Box<TypeAnnotation>),
}

impl From<TypeIdentifier> for TypeAnnotation {
    fn from(type_identifier: TypeIdentifier) -> Self {
        match type_identifier {
            TypeIdentifier::Type(name) => TypeAnnotation::Type(name),
            TypeIdentifier::GenericType(name, generics) => TypeAnnotation::ConcreteType(
                name,
                generics.into_iter().map(|g| g.type_annotation()).collect(),
            ),
            TypeIdentifier::ConcreteType(name, generics) => {
                TypeAnnotation::ConcreteType(name, generics)
            }
            TypeIdentifier::MemberType(_, _) => {
                panic!("Cannot convert member type to type annotation")
            }
        }
    }
}

pub enum LiteralType {
    Unit,
    Int,
    Float,
    String,
    Char,
    Bool,
}

impl TypeAnnotation {
    pub fn name(&self) -> String {
        match self {
            TypeAnnotation::Type(name) => name.clone(),
            TypeAnnotation::ConcreteType(name, _) => name.clone(),
            TypeAnnotation::Array(type_identifier) => type_identifier.name(),
            TypeAnnotation::Literal(literal) => literal.to_string(),
            TypeAnnotation::Function(type_annotations, return_type_annotation) => {
                format!(
                    "fun({}): {}",
                    type_annotations
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    return_type_annotation.to_string()
                )
            }
        }
    }
}

impl Display for TypeAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeAnnotation::Type(type_name) => write!(f, "{}", type_name),
            TypeAnnotation::ConcreteType(type_name, generics) => {
                if generics.is_empty() {
                    return write!(f, "{}", type_name);
                }

                write!(
                    f,
                    "{}<{}>",
                    type_name,
                    generics
                        .iter()
                        .map(|g| g.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            TypeAnnotation::Array(type_identifier) => write!(f, "[{}]", type_identifier),
            TypeAnnotation::Literal(literal) => write!(f, "{:?}", literal),
            TypeAnnotation::Function(type_annotations, return_type_annotation) => {
                write!(
                    f,
                    "fun({}): {}",
                    type_annotations
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    return_type_annotation.to_string()
                )
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeIdentifier {
    Type(String),
    GenericType(String, Vec<GenericType>),
    ConcreteType(String, Vec<TypeAnnotation>),
    MemberType(Box<TypeIdentifier>, String),
}

impl TypeIdentifier {
    pub fn name(&self) -> &str {
        match self {
            TypeIdentifier::Type(name) => name,
            TypeIdentifier::GenericType(name, _) => name,
            TypeIdentifier::ConcreteType(name, _) => name,
            TypeIdentifier::MemberType(_, name) => name,
        }
    }

    pub fn eq_names(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Type(name_left), Self::Type(name_right)) => name_left == name_right,
            (Self::GenericType(name_left, _), Self::GenericType(name_right, _)) => {
                name_left == name_right
            }
            _ => false,
        }
    }
}

impl Hash for TypeIdentifier {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl Display for TypeIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeIdentifier::Type(type_name) => write!(f, "{}", type_name),
            TypeIdentifier::GenericType(type_name, generics) => {
                write!(
                    f,
                    "{}<{}>",
                    type_name,
                    generics
                        .iter()
                        .map(|g| g.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            TypeIdentifier::ConcreteType(type_name, generics) => {
                write!(
                    f,
                    "{}<{}>",
                    type_name,
                    generics
                        .iter()
                        .map(|g| g.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            TypeIdentifier::MemberType(parent, member) => write!(f, "{}->{}", parent, member),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericType {
    pub type_name: String,
}

impl GenericType {
    pub fn type_annotation(&self) -> TypeAnnotation {
        TypeAnnotation::Type(self.type_name.clone())
    }
}

impl Display for GenericType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericConstraint {
    pub generic: GenericType,
    pub constraints: Vec<TypeIdentifier>,
}

pub(super) fn can_be_type_annotation(cursor: &Cursor) -> bool {
    let mut cloned_cursor = cursor.clone();

    match cloned_cursor.first().kind {
        TokenKind::Literal(token::Literal::Void) => true,
        TokenKind::Literal(token::Literal::Unit) => true,
        TokenKind::Identifier(_) => true,
        TokenKind::OpenBracket => {
            let _ = cloned_cursor.bump();
            can_be_type_annotation(&cloned_cursor)
        }
        TokenKind::Keyword(Keyword::Fun) => true,
        _ => false,
    }
}

pub(super) fn parse_type_annotation_from_str(
    type_str: &str,
    allow_void: bool,
) -> Result<TypeAnnotation, String> {
    let tokens = crate::lexer::tokenize(type_str)?;
    let mut cursor = Cursor::new(tokens);
    parse_type_annotation(&mut cursor, allow_void)
}

pub(super) fn parse_type_annotation(
    cursor: &mut Cursor,
    allow_void: bool,
) -> Result<TypeAnnotation, String> {
    match cursor.first().kind {
        TokenKind::Literal(token::Literal::Void) => {
            if !allow_void {
                return Err("Void type is not allowed here".to_string());
            }

            cursor.bump()?; // Consume the void
            Ok(TypeAnnotation::Type("void".to_string()))
        }
        TokenKind::Literal(token::Literal::Unit) => {
            cursor.bump()?; // Consume the unit
            Ok(TypeAnnotation::Type("unit".to_string()))
        }
        TokenKind::Identifier(type_name) => {
            cursor.bump()?; // Consume the type identifier

            let mut generics = None;

            if cursor.first().kind == TokenKind::Less {
                cursor.bump()?; // Consume the <
                generics = Some(parse_comma_separated_type_annotations(
                    cursor,
                    |kind| kind != TokenKind::Greater,
                    allow_void,
                )?);

                cursor.bump()?; // Consume the >
            }

            if cursor.first().kind == TokenKind::DoubleColon {
                cursor.bump()?; // Consume the ::

                let TokenKind::Identifier(variant_name) = cursor.bump()?.kind else {
                    return Err(format!(
                        "Expected variant name but found {:?}",
                        cursor.first().kind
                    ));
                };

                let type_name = format!("{}::{}", type_name, variant_name);

                if let Some(generics) = generics {
                    return Ok(TypeAnnotation::ConcreteType(type_name, generics));
                }

                return Ok(TypeAnnotation::Type(type_name));
            }

            if let Some(generics) = generics {
                return Ok(TypeAnnotation::ConcreteType(type_name, generics));
            }

            Ok(TypeAnnotation::Type(type_name))
        }
        TokenKind::OpenBracket => {
            cursor.bump()?; // Consume the [

            let type_annotation = parse_type_annotation(cursor, allow_void)?;

            if cursor.first().kind != TokenKind::CloseBracket {
                return Err(format!("Expected ] but found {:?}", cursor.first().kind));
            }

            cursor.bump()?; // Consume the ]
            Ok(TypeAnnotation::Array(Box::new(type_annotation)))
        }
        TokenKind::Keyword(Keyword::Fun) => {
            cursor.bump()?; // Consume the fun

            if cursor.first().kind != TokenKind::OpenParen {
                return Err(format!("Expected ( but found {:?}", cursor.first().kind));
            }

            cursor.bump()?; // Consume the (

            let parameters = parse_comma_separated_type_annotations(
                cursor,
                |kind| kind != TokenKind::CloseParen,
                allow_void,
            )?;

            cursor.bump()?; // Consume the )

            if cursor.first().kind != TokenKind::Colon {
                return Err(format!("Expected : but found {:?}", cursor.first().kind));
            }

            cursor.bump()?; // Consume the :

            let return_type = Box::new(parse_type_annotation(cursor, true)?);

            Ok(TypeAnnotation::Function(parameters, return_type))
        }
        _ => Err(format!(
            "Expected type identifier but found {:?}",
            cursor.first().kind
        )),
    }
}

fn parse_comma_separated_type_annotations<F: Fn(TokenKind) -> bool>(
    cursor: &mut Cursor,
    check: F,
    allow_void: bool,
) -> Result<Vec<TypeAnnotation>, String> {
    let mut types = Vec::new();

    while check(cursor.first().kind) {
        let type_annotation = parse_type_annotation(cursor, allow_void)?;
        types.push(type_annotation);

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        }
    }

    Ok(types)
}

pub(super) fn parse_type_identifier(
    cursor: &mut Cursor,
    use_double_colon: bool,
) -> Result<TypeIdentifier, String> {
    match cursor.first().kind {
        TokenKind::Identifier(type_name) => {
            cursor.bump()?; // Consume the type identifier

            if use_double_colon {
                if cursor.first().kind != TokenKind::DoubleColon {
                    return Err(format!("Expected :: but found {:?}", cursor.first().kind));
                }

                cursor.bump()?; // Consume the ::
            }

            if cursor.first().kind == TokenKind::Less {
                cursor.bump()?; // Consume the <
                let generics = parse_generics_in_type_name(cursor)?;

                cursor.bump()?; // Consume the >
                return Ok(TypeIdentifier::GenericType(type_name, generics));
            }

            Ok(TypeIdentifier::Type(type_name))
        }
        _ => Err(format!(
            "Expected type identifier but found {:?}",
            cursor.first().kind
        )),
    }
}

fn parse_generics_in_type_name(cursor: &mut Cursor) -> Result<Vec<GenericType>, String> {
    let mut types = Vec::new();

    while cursor.first().kind != TokenKind::Greater {
        let TokenKind::Identifier(type_name) = cursor.bump()?.kind else {
            return Err(format!(
                "Expected type identifier but found {:?}",
                cursor.first().kind
            ));
        };

        types.push(GenericType { type_name });

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        }
    }

    Ok(types)
}
