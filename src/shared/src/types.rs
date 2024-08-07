use std::{fmt::Display, hash::Hash, ops::Deref};

use crate::{
    lexer::token::{self, IntLiteral, Keyword, TokenKind},
    parser::{cursor::Cursor, Literal},
    type_checker::{Function, Type},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeAnnotation {
    Type(String),
    ConcreteType(String, Vec<TypeAnnotation>),
    Array(Box<TypeAnnotation>),
    Literal(Box<Literal>),
    Function(Option<Box<TypeAnnotation>>, Option<Box<TypeAnnotation>>),
}

impl TypeAnnotation {
    pub fn void() -> Self {
        TypeAnnotation::Type("void".to_string())
    }

    pub fn is_enum_member(&self) -> bool {
        if let TypeAnnotation::Type(s) = self {
            return s.contains("::");
        }

        if let TypeAnnotation::ConcreteType(s, _) = self {
            return s.contains("::");
        }

        return false;
    }
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

impl From<Type> for TypeAnnotation {
    fn from(t: Type) -> Self {
        match t {
            Type::Unknown => panic!("Cannot convert unknown type to type annotation"),
            Type::Void => TypeAnnotation::Type("void".to_string()),
            Type::Unit => TypeAnnotation::Type("unit".to_string()),
            Type::Int => TypeAnnotation::Type("int".to_string()),
            Type::Float => TypeAnnotation::Type("float".to_string()),
            Type::String => TypeAnnotation::Type("string".to_string()),
            Type::Char => TypeAnnotation::Type("char".to_string()),
            Type::Bool => TypeAnnotation::Type("bool".to_string()),
            Type::Array(t) => TypeAnnotation::Array(Box::new(t.deref().clone().into())),
            Type::Function(Function {
                param, return_type, ..
            }) => TypeAnnotation::Function(
                param.map(|p| Box::new(p.type_.deref().clone().into())),
                Some(Box::new(return_type.deref().clone().into())),
            ),
            Type::Generic(g) => TypeAnnotation::Type(g.type_name),
            Type::UInt => TypeAnnotation::Type("uint".to_string()),
            Type::Struct(_) => todo!(),
            Type::Enum(_) => todo!(),
            Type::EnumMember(_) => todo!(),
            Type::Union(_) => todo!(),
            Type::TypeAlias(_) => todo!(),
            Type::Trait(_) => todo!(),
            Type::Literal { .. } => todo!(),
        }
    }
}

pub enum LiteralType {
    Bool(bool),
    Int(IntLiteral<i64>),
    UInt(IntLiteral<u64>),
    Float(f64),
    Char(String),
    String(String),
}

impl Display for LiteralType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralType::Bool(value) => write!(f, "#{}", value),
            LiteralType::Int(value) => write!(f, "#{}", value.to_string()),
            LiteralType::UInt(value) => write!(f, "#{}", value.to_string()),
            LiteralType::Float(value) => write!(f, "#{}", value),
            LiteralType::Char(value) => write!(f, "#{}", value),
            LiteralType::String(value) => write!(f, "#{}", value),
        }
    }
}

impl TypeAnnotation {
    pub fn name(&self) -> String {
        match self {
            TypeAnnotation::Type(name) => name.clone(),
            TypeAnnotation::ConcreteType(name, _) => name.clone(),
            TypeAnnotation::Array(type_identifier) => type_identifier.name(),
            TypeAnnotation::Literal(literal) => literal.to_string(),
            TypeAnnotation::Function(type_annotation, return_type_annotation) => {
                format!(
                    "fun({}): {}",
                    type_annotation
                        .clone()
                        .map(|t| t.to_string())
                        .unwrap_or("".to_string()),
                    return_type_annotation
                        .clone()
                        .map(|rt| rt.to_string())
                        .unwrap_or("".to_string())
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
            TypeAnnotation::Function(type_annotation, return_type_annotation) => {
                write!(
                    f,
                    "fun({}): {}",
                    type_annotation
                        .clone()
                        .map(|t| format!("{}", t))
                        .unwrap_or("".to_string()),
                    return_type_annotation
                        .clone()
                        .map(|rt| format!("{}", rt))
                        .unwrap_or("".to_string())
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
            (Self::MemberType(type_left, member_left), Self::GenericType(name_right, generics)) => {
                let split = name_right.split("::").collect::<Vec<&str>>();

                if split.len() < 2 {
                    return false;
                }

                let parent_right = split[0];
                let member_right = split[1];

                type_left.eq_names(&TypeIdentifier::GenericType(
                    parent_right.to_string(),
                    generics.clone(),
                )) && member_left == member_right
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

impl From<TypeAnnotation> for TypeIdentifier {
    fn from(value: TypeAnnotation) -> Self {
        match value {
            TypeAnnotation::Type(name) => {
                if name.contains("::") {
                    let mut parts = name.split("::");
                    let parent = parts.next().unwrap();
                    let member = parts.next().unwrap();

                    TypeIdentifier::MemberType(
                        Box::new(TypeIdentifier::Type(parent.to_string())),
                        member.to_string(),
                    )
                } else {
                    TypeIdentifier::Type(name)
                }
            }
            TypeAnnotation::ConcreteType(name, generics) => {
                TypeIdentifier::ConcreteType(name, generics)
            }
            _ => panic!("Cannot convert {:?} to TypeIdentifier", value),
        }
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

pub(super) fn parse_optional_type_annotation(
    cursor: &mut Cursor,
    allow_void: bool,
) -> Result<Option<TypeAnnotation>, String> {
    if cursor.first().kind != TokenKind::Colon {
        Ok(None)
    } else {
        cursor.bump()?; // Consume the :

        if !can_be_type_annotation(cursor) {
            return Err(format!(
                "Expected type annotation but found {:?}",
                cursor.first().kind
            ));
        }

        Ok(Some(parse_type_annotation(cursor, allow_void)?))
    }
}

pub(super) fn can_be_type_annotation(cursor: &Cursor) -> bool {
    let mut cloned_cursor = cursor.clone();

    match cloned_cursor.first().kind {
        TokenKind::Literal(token::Literal::Void) => true,
        TokenKind::Literal(token::Literal::Unit) => true,
        TokenKind::Hash => true,
        TokenKind::Identifier(_) => true,
        TokenKind::OpenBracket => {
            let _ = cloned_cursor.bump();
            can_be_type_annotation(&cloned_cursor)
        }
        TokenKind::Keyword(Keyword::Fun) => true,
        _ => false,
    }
}

// pub(super) fn parse_type_annotation_from_str(
//     type_str: &str,
//     allow_void: bool,
// ) -> Result<TypeAnnotation, String> {
//     let tokens = crate::lexer::tokenize(type_str)?;
//     let mut cursor = Cursor::new(tokens);
//     parse_type_annotation(&mut cursor, allow_void)
// }

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
        TokenKind::Hash => {
            cursor.bump()?; // Consume the #
            match parse_literal_type(cursor)? {
                LiteralType::Bool(v) => Ok(TypeAnnotation::Literal(Box::new(Literal::Bool(v)))),
                LiteralType::Int(v) => Ok(TypeAnnotation::Literal(Box::new(Literal::Int(v.value)))),
                LiteralType::UInt(v) => {
                    Ok(TypeAnnotation::Literal(Box::new(Literal::UInt(v.value))))
                }
                LiteralType::Float(v) => Ok(TypeAnnotation::Literal(Box::new(Literal::Float(v)))),
                LiteralType::Char(v) => Ok(TypeAnnotation::Literal(Box::new(Literal::Char(
                    v.parse::<char>().expect("Failed to parse char literal"),
                )))),
                LiteralType::String(v) => Ok(TypeAnnotation::Literal(Box::new(Literal::String(v)))),
            }
        }
        TokenKind::Identifier(type_name) => {
            cursor.bump()?; // Consume the type identifier

            let mut generics = None;

            if cursor.first().kind == TokenKind::DoubleColon
                && cursor.second().kind == TokenKind::Less
            {
                cursor.bump()?; // Consume the ::
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

            let params = parse_comma_separated_type_annotations(
                cursor,
                |kind| kind != TokenKind::CloseParen,
                allow_void,
            )?;

            cursor.bump()?; // Consume the )

            let return_type_annotation = parse_optional_type_annotation(cursor, true)?;

            unwrap_function_annotation(params, return_type_annotation)
        }
        _ => Err(format!(
            "Expected type identifier but found {:?}",
            cursor.first().kind
        )),
    }
}

fn unwrap_function_annotation(
    params: Vec<TypeAnnotation>,
    return_type_annotation: Option<TypeAnnotation>,
) -> Result<TypeAnnotation, String> {
    match params.first().cloned() {
        None => Ok(TypeAnnotation::Function(
            None,
            return_type_annotation.map(Box::new),
        )),
        Some(first) => {
            let new_return_type_annotation = unwrap_function_annotation_recurse(
                params.into_iter().skip(1).collect(),
                return_type_annotation,
            )?;

            Ok(TypeAnnotation::Function(
                Some(Box::new(first)),
                new_return_type_annotation.map(Box::new),
            ))
        }
    }
}

fn unwrap_function_annotation_recurse(
    params: Vec<TypeAnnotation>,
    return_type_annotation: Option<TypeAnnotation>,
) -> Result<Option<TypeAnnotation>, String> {
    match params.last().cloned() {
        None => Ok(return_type_annotation),
        Some(last) => {
            let new_return_type_annotation = TypeAnnotation::Function(
                Some(Box::new(last)),
                return_type_annotation.map(Box::new),
            );

            unwrap_function_annotation_recurse(
                params.into_iter().rev().skip(1).rev().collect(),
                Some(new_return_type_annotation),
            )
        }
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

fn parse_literal_type(cursor: &mut Cursor) -> Result<LiteralType, String> {
    match cursor.first().kind {
        TokenKind::Literal(token::Literal::Bool(value)) => {
            cursor.bump()?; // Consume the bool
            Ok(LiteralType::Bool(value))
        }
        TokenKind::Literal(token::Literal::Int(value)) => {
            cursor.bump()?; // Consume the int
            Ok(LiteralType::Int(value))
        }
        TokenKind::Literal(token::Literal::Float(value)) => {
            cursor.bump()?; // Consume the float
            Ok(LiteralType::Float(value))
        }
        TokenKind::Literal(token::Literal::Char(value)) => {
            cursor.bump()?; // Consume the char
            Ok(LiteralType::Char(value))
        }
        TokenKind::Literal(token::Literal::String(value)) => {
            cursor.bump()?; // Consume the string
            Ok(LiteralType::String(value))
        }
        TokenKind::Minus => {
            cursor.bump()?; // Consume the -

            match cursor.first().kind {
                TokenKind::Literal(token::Literal::Int(literal)) => {
                    cursor.bump()?; // Consume the int
                    Ok(LiteralType::Int(IntLiteral {
                        value: -literal.value,
                        base: literal.base,
                    }))
                }
                TokenKind::Literal(token::Literal::Float(value)) => {
                    cursor.bump()?; // Consume the float
                    Ok(LiteralType::Float(-value))
                }
                _ => Err(format!("Cannot negate type: {:?}", cursor.first().kind)),
            }
        }
        _ => Err(format!(
            "Expected literal type but found {:?}",
            cursor.first().kind
        )),
    }
}
