use std::fmt::Display;

use crate::{lexer::token::{self, TokenKind}, parser::cursor::Cursor};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation {
    Type(String),
    GenericType(String, Vec<TypeAnnotation>),
    Slice(Box<TypeAnnotation>),
}

impl Display for TypeAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeAnnotation::Type(type_name) =>
                write!(f, "{}", type_name),
            TypeAnnotation::GenericType(type_name, generics) => {
                write!(f, "{}<{}>",
                    type_name,
                    generics.iter()
                        .map(|g| g.to_string())
                        .collect::<Vec<String>>()
                        .join(", "))
            },
            TypeAnnotation::Slice(type_name) =>
                write!(f, "[{}]", type_name),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeName {
    Type(String),
    GenericType(String, Vec<GenericType>),
}

impl Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeName::Type(type_name) =>
                write!(f, "{}", type_name),
            TypeName::GenericType(type_name, generics) => {
                write!(f, "{}<{}>",
                    type_name,
                    generics.iter()
                        .map(|g| g.to_string())
                        .collect::<Vec<String>>()
                        .join(", "))
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

pub(super) fn parse_type_annotation_from_str(type_str: &str) -> Result<TypeAnnotation, String> {
    let tokens = crate::lexer::tokenize(type_str)?;
    let mut cursor = Cursor::new(tokens);
    parse_type_annotation(&mut cursor)
}

pub(super) fn parse_type_annotation(cursor: &mut Cursor) -> Result<TypeAnnotation, String> {
    match cursor.first().kind {
        TokenKind::Literal(token::Literal::Unit) => {
            cursor.bump()?; // Consume the unit
            Ok(TypeAnnotation::Type("unit".to_string()))
        },
        TokenKind::Identifier(type_name) => {
            cursor.bump()?; // Consume the type identifier

            if cursor.first().kind == TokenKind::Less {
                cursor.bump()?; // Consume the <
                let generics =
                    parse_comma_separated_type_annotations(cursor,
                        |kind| kind != TokenKind::Greater)?;

                cursor.bump()?; // Consume the >
                return Ok(TypeAnnotation::GenericType(type_name, generics));
            }

            Ok(TypeAnnotation::Type(type_name))
        },
        TokenKind::OpenBracket => {
            cursor.bump()?; // Consume the [

            let type_name = parse_type_annotation(cursor)?;

            if cursor.first().kind != TokenKind::CloseBracket {
                return Err(format!("Expected ] but found {:?}", cursor.first().kind));
            }

            cursor.bump()?; // Consume the ]
            Ok(TypeAnnotation::Slice(Box::new(type_name)))
        },
        _ => Err(format!("Expected type identifier but found {:?}", cursor.first().kind)),
    }
}

fn parse_comma_separated_type_annotations<F: Fn(TokenKind) -> bool>(cursor: &mut Cursor, check: F) -> Result<Vec<TypeAnnotation>, String> {
    let mut types = Vec::new();

    while check(cursor.first().kind) {
        let type_annotation = parse_type_annotation(cursor)?;
        types.push(type_annotation);

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        }
    }

    Ok(types)
}

pub(super) fn parse_type_name(type_name: &str) -> Result<TypeName, String> {
    let tokens = crate::lexer::tokenize(type_name)?;
    let cursor = &mut Cursor::new(tokens);

    match cursor.first().kind {
        TokenKind::Identifier(type_name) => {
            cursor.bump()?; // Consume the type identifier
            
            if cursor.first().kind == TokenKind::Less {
                cursor.bump()?; // Consume the <
                let generics =
                    parse_generics_in_type_name(cursor)?;

                cursor.bump()?; // Consume the >
                return Ok(TypeName::GenericType(type_name, generics));
            }

            Ok(TypeName::Type(type_name))
        },
        _ => Err(format!("Expected type identifier but found {:?}", cursor.first().kind)),
    }
}

fn parse_generics_in_type_name(cursor: &mut Cursor) -> Result<Vec<GenericType>, String> {
    let mut types = Vec::new();

    while cursor.first().kind != TokenKind::Greater {
        let TokenKind::Identifier(type_name) = cursor.first().kind else {
            return Err(format!("Expected type identifier but found {:?}", cursor.first().kind));
        };

        types.push(GenericType { type_name });

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        }
    }

    Ok(types)
}
