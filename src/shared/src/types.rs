use std::{fmt::Display, hash::Hash};

use crate::{lexer::token::{self, TokenKind}, parser::cursor::Cursor};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeAnnotation {
    Type(String),
    ConcreteType(String, Vec<TypeAnnotation>),
    Array(Box<TypeAnnotation>),
}

impl TypeAnnotation {
    pub fn name(&self) -> &str {
        match self {
            TypeAnnotation::Type(name) => name,
            TypeAnnotation::ConcreteType(name, _) => name,
            TypeAnnotation::Array(type_identifier) => type_identifier.name(),
        }
    }
}

impl Display for TypeAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeAnnotation::Type(type_name) =>
                write!(f, "{}", type_name),
            TypeAnnotation::ConcreteType(type_name, generics) => {
                write!(f, "{}<{}>",
                    type_name,
                    generics.iter()
                        .map(|g| g.to_string())
                        .collect::<Vec<String>>()
                        .join(", "))
            },
            TypeAnnotation::Array(type_identifier) =>
                write!(f, "[{}]", type_identifier),
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
            (Self::GenericType(name_left, _), Self::GenericType(name_right, _)) => name_left == name_right,
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
            TypeIdentifier::Type(type_name) =>
                write!(f, "{}", type_name),
            TypeIdentifier::GenericType(type_name, generics) => {
                write!(f, "{}<{}>",
                    type_name,
                    generics.iter()
                        .map(|g| g.to_string())
                        .collect::<Vec<String>>()
                        .join(", "))
            },
            TypeIdentifier::ConcreteType(type_name, generics) => {
                write!(f, "{}<{}>",
                    type_name,
                    generics.iter()
                        .map(|g| g.to_string())
                        .collect::<Vec<String>>()
                        .join(", "))
            },
            TypeIdentifier::MemberType(parent, member) =>
                write!(f, "{} -> {}", parent, member),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericType {
    pub type_name: String,
}

impl Display for GenericType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_name)
    }
}

pub(super) fn can_be_type_annotation(cursor: &Cursor) -> bool {
    let mut cloned_cursor = cursor.clone();

    match cloned_cursor.first().kind {
        TokenKind::Literal(token::Literal::Unit) => true,
        TokenKind::Identifier(_) => true,
        TokenKind::OpenBracket => {
            let _ = cloned_cursor.bump();
            can_be_type_annotation(&cloned_cursor)
        },
        _ => false,
    }
}

pub(super) fn parse_type_annotation_from_str(type_str: &str, use_double_colon: bool) -> Result<TypeAnnotation, String> {
    let tokens = crate::lexer::tokenize(type_str)?;
    let mut cursor = Cursor::new(tokens);
    parse_type_annotation(&mut cursor, use_double_colon)
}

pub(super) fn parse_type_annotation(cursor: &mut Cursor, use_double_colon: bool) -> Result<TypeAnnotation, String> {
    match cursor.first().kind {
        TokenKind::Literal(token::Literal::Unit) => {
            cursor.bump()?; // Consume the unit
            Ok(TypeAnnotation::Type("unit".to_string()))
        },
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
                let generics =
                    parse_comma_separated_type_annotations(cursor,
                        |kind| kind != TokenKind::Greater)?;

                cursor.bump()?; // Consume the >
                return Ok(TypeAnnotation::ConcreteType(type_name, generics));
            }

            Ok(TypeAnnotation::Type(type_name))
        },
        TokenKind::OpenBracket => {
            cursor.bump()?; // Consume the [

            let type_annotation = parse_type_annotation(cursor, false)?;

            if cursor.first().kind != TokenKind::CloseBracket {
                return Err(format!("Expected ] but found {:?}", cursor.first().kind));
            }

            cursor.bump()?; // Consume the ]
            Ok(TypeAnnotation::Array(Box::new(type_annotation)))
        },
        _ => Err(format!("Expected type identifier but found {:?}", cursor.first().kind)),
    }
}

fn parse_comma_separated_type_annotations<F: Fn(TokenKind) -> bool>(cursor: &mut Cursor, check: F) -> Result<Vec<TypeAnnotation>, String> {
    let mut types = Vec::new();

    while check(cursor.first().kind) {
        let type_annotation = parse_type_annotation(cursor, false)?;
        types.push(type_annotation);

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        }
    }

    Ok(types)
}

pub(super) fn parse_type_identifier(cursor: &mut Cursor, use_double_colon: bool) -> Result<TypeIdentifier, String> {
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
                let generics =
                    parse_generics_in_type_name(cursor)?;

                cursor.bump()?; // Consume the >
                return Ok(TypeIdentifier::GenericType(type_name, generics));
            }

            Ok(TypeIdentifier::Type(type_name))
        },
        _ => Err(format!("Expected type identifier but found {:?}", cursor.first().kind)),
    }
}

fn parse_generics_in_type_name(cursor: &mut Cursor) -> Result<Vec<GenericType>, String> {
    let mut types = Vec::new();

    while cursor.first().kind != TokenKind::Greater {
        let TokenKind::Identifier(type_name) = cursor.bump()?.kind else {
            return Err(format!("Expected type identifier but found {:?}", cursor.first().kind));
        };

        types.push(GenericType { type_name });

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        }
    }

    Ok(types)
}
