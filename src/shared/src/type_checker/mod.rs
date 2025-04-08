pub mod ast;
pub mod decision_tree;
pub mod full_name;
pub mod type_checker;
pub mod type_environment;

mod expressions;
mod scope;
mod statements;

pub use full_name::*;
pub use type_checker::*;
pub use type_environment::*;

use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc, str::FromStr};

use crate::{
    parser::{self, Expression},
    types::{GenericType, ToKey, TypeAnnotation, TypeIdentifier},
};

use ast::EmbeddedStruct;

use self::statements::check_type_annotation;

#[derive(Debug, Clone)]
pub struct Struct {
    pub type_identifier: TypeIdentifier,
    pub embedded_structs: Vec<EmbeddedStruct>,
    pub fields: Vec<StructField>,
}

impl Struct {
    pub fn type_annotation(&self) -> TypeAnnotation {
        TypeAnnotation::from(self.type_identifier.clone())
    }
}

impl FullName for Struct {
    fn full_name(&self) -> String {
        self.type_identifier.to_string()
    }
}

impl PartialEq for Struct {
    fn eq(&self, other: &Self) -> bool {
        self.type_identifier == other.type_identifier && self.fields == other.fields
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub struct_name: TypeIdentifier,
    pub field_name: String,
    pub default_value: Option<Type>,
    pub field_type: Type,
}

pub fn get_field_by_name<'a>(
    struct_fields: &'a [StructField],
    field_name: &'a str,
) -> Option<&'a StructField> {
    struct_fields.iter().find(|f| f.field_name == field_name)
}

impl FullName for StructField {
    fn full_name(&self) -> String {
        format!(
            "{}.{}: {}",
            self.struct_name, self.field_name, self.field_type
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub type_identifier: TypeIdentifier,
    pub shared_fields: Vec<StructField>,
    pub members: HashMap<String, Type>,
}

impl Enum {
    pub fn type_annotation(&self) -> TypeAnnotation {
        TypeAnnotation::from(self.type_identifier.clone())
    }
}

impl FullName for Enum {
    fn full_name(&self) -> String {
        self.type_identifier.to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Union {
    pub type_identifier: TypeIdentifier,
    pub literal_type: Box<Type>,
    pub literals: Vec<Type>,
}

impl Union {
    pub fn type_annotation(&self) -> TypeAnnotation {
        TypeAnnotation::from(self.type_identifier.clone())
    }
}

impl FullName for Union {
    fn full_name(&self) -> String {
        format!(
            "{} {{ {} }}",
            self.type_identifier,
            self.literals
                .iter()
                .map(|l| l.to_string())
                .collect::<Vec<String>>()
                .join(" | ")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAlias {
    pub type_identifier: TypeIdentifier,
    pub types: Vec<Type>,
}

impl TypeAlias {
    pub fn type_annotation(&self) -> TypeAnnotation {
        TypeAnnotation::from(self.type_identifier.clone())
    }
}

impl FullName for TypeAlias {
    fn full_name(&self) -> String {
        format!(
            "{} = {}",
            self.type_identifier,
            self.types
                .iter()
                .map(|l| l.to_string())
                .collect::<Vec<String>>()
                .join(" | ")
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Protocol {
    pub type_identifier: TypeIdentifier,
    pub functions: Vec<(TypeIdentifier, Type)>,
}

impl FullName for Protocol {
    fn full_name(&self) -> String {
        self.type_identifier.to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub identifier: Option<TypeIdentifier>,
    pub param: Option<Parameter>,
    pub return_type: Box<Type>,
}

impl Function {
    pub fn type_annotation(&self) -> Option<TypeAnnotation> {
        self.identifier
            .clone()
            .map(|id| TypeAnnotation::from(id.clone()))
            .or_else(|| {
                Some(TypeAnnotation::Function(
                    self.param
                        .clone()
                        .map(|p| Box::new(p.type_.type_annotation())),
                    Some(Box::new(self.return_type.type_annotation())),
                ))
            })
    }
}

impl FullName for Function {
    fn full_name(&self) -> String {
        format!(
            "fun({}): {}",
            self.param
                .clone()
                .map(|p| p.type_.full_name())
                .unwrap_or_default(),
            self.return_type.full_name()
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub identifier: String,
    pub type_: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // This is a meta type used for type substitution
    Substitution {
        type_identifier: TypeIdentifier,
        actual_type: Box<Type>,
    },

    Unknown,
    Generic(GenericType),
    Void,
    Unit,
    Int,
    UInt,
    Float,
    String,
    Char,
    Bool,
    Array(Box<Type>),
    Struct(Struct),
    Enum(Enum),
    Union(Union),
    TypeAlias(TypeAlias),
    Protocol(Protocol),
    Function(Function),
    Literal {
        name: String, // String representation of the literal
        type_: Box<Type>,
    },
    Tuple(Vec<Type>),
}

impl Type {
    pub fn unsubstitute(self) -> Type {
        match self {
            Type::Substitution { actual_type, .. } => (*actual_type).unsubstitute(),
            _ => self,
        }
    }
    pub fn option() -> Type {
        let option_name = "Option".to_string();

        let option_ident = TypeIdentifier::GenericType(
            option_name,
            vec![GenericType {
                type_name: "T".to_string(),
            }],
        );

        let some_member_ident =
            TypeIdentifier::MemberType(Box::new(option_ident.clone()), "Some".to_string());

        let none_member_ident =
            TypeIdentifier::MemberType(Box::new(option_ident.clone()), "None".to_string());

        Type::Enum(Enum {
            type_identifier: option_ident,
            shared_fields: Vec::new(),
            members: vec![
                (
                    "Some".to_string(),
                    Type::Struct(Struct {
                        type_identifier: some_member_ident.clone(),
                        embedded_structs: vec![],
                        fields: vec![StructField {
                            struct_name: some_member_ident.clone(),
                            field_name: "value".to_string(),
                            default_value: None,
                            field_type: Type::Generic(GenericType {
                                type_name: "T".to_string(),
                            }),
                        }],
                    }),
                ),
                (
                    "None".to_string(),
                    Type::Struct(Struct {
                        type_identifier: none_member_ident,
                        embedded_structs: vec![],
                        fields: vec![],
                    }),
                ),
            ]
            .into_iter()
            .collect(),
        })
    }

    pub fn option_of(concrete: Type) -> Type {
        let option_name = "Option".to_string();

        let option_ident = TypeIdentifier::GenericType(
            option_name,
            vec![GenericType {
                type_name: "T".to_string(),
            }],
        );

        let some_member_ident =
            TypeIdentifier::MemberType(Box::new(option_ident.clone()), "Some".to_string());

        Type::Enum(Enum {
            type_identifier: option_ident,
            shared_fields: Vec::new(),
            members: vec![
                (
                    "Some".to_string(),
                    Type::Struct(Struct {
                        type_identifier: TypeIdentifier::MemberType(
                            Box::new(TypeIdentifier::ConcreteType(
                                "Option".to_string(),
                                vec![concrete.type_annotation()],
                            )),
                            "Some".to_string(),
                        ),
                        embedded_structs: vec![],
                        fields: vec![StructField {
                            struct_name: some_member_ident.clone(),
                            field_name: "value".to_string(),
                            default_value: None,
                            field_type: concrete.clone(),
                        }],
                    }),
                ),
                (
                    "None".to_string(),
                    Type::Struct(Struct {
                        type_identifier: TypeIdentifier::MemberType(
                            Box::new(TypeIdentifier::ConcreteType(
                                "Option".to_string(),
                                vec![concrete.type_annotation()],
                            )),
                            "None".to_string(),
                        ),
                        embedded_structs: vec![],
                        fields: Vec::new(),
                    }),
                ),
            ]
            .into_iter()
            .collect(),
        })
    }

    pub fn from_literal(literal: &parser::Literal) -> Result<Type, String> {
        match literal {
            parser::Literal::Unit => Ok(Type::Literal {
                name: Type::Unit.to_string(),
                type_: Box::new(Type::Unit),
            }),
            parser::Literal::Int(v) => Ok(Type::Literal {
                name: v.to_string(),
                type_: Box::new(Type::Int),
            }),
            parser::Literal::UInt(v) => Ok(Type::Literal {
                name: v.to_string(),
                type_: Box::new(Type::UInt),
            }),
            parser::Literal::Float(v) => Ok(Type::Literal {
                name: v.to_string(),
                type_: Box::new(Type::Float),
            }),
            parser::Literal::Char(v) => Ok(Type::Literal {
                name: format!("'{}'", v),
                type_: Box::new(Type::Char),
            }),
            parser::Literal::String(v) => Ok(Type::Literal {
                name: format!("\"{}\"", v),
                type_: Box::new(Type::String),
            }),
            parser::Literal::Bool(v) => Ok(Type::Literal {
                name: v.to_string(),
                type_: Box::new(Type::Bool),
            }),
            _ => Err(format!("Cannot convert literal {:?} to type", literal)),
        }
    }

    pub fn type_identifier(&self) -> TypeIdentifier {
        match self {
            Type::Substitution {
                type_identifier, ..
            } => type_identifier.clone(),
            Type::Generic(name) => TypeIdentifier::Type(name.type_name.clone()),
            Type::Void => TypeIdentifier::Type("Void".to_string()),
            Type::Unit => TypeIdentifier::Type("Unit".to_string()),
            Type::Int => TypeIdentifier::Type("Int".to_string()),
            Type::UInt => TypeIdentifier::Type("UInt".to_string()),
            Type::Float => TypeIdentifier::Type("Float".to_string()),
            Type::String => TypeIdentifier::Type("String".to_string()),
            Type::Char => TypeIdentifier::Type("Char".to_string()),
            Type::Bool => TypeIdentifier::Type("Bool".to_string()),
            Type::Struct(s) => s.type_identifier.clone(),
            Type::Enum(u) => u.type_identifier.clone(),
            Type::Union(u) => u.type_identifier.clone(),
            Type::TypeAlias(u) => u.type_identifier.clone(),
            Type::Protocol(u) => u.type_identifier.clone(),
            Type::Function(f) => f
                .identifier
                .clone()
                .unwrap_or_else(|| panic!("Closure has no identifier")),
            _ => panic!("Cannot get type identifier for type {}", self.full_name()),
        }
    }

    pub fn type_annotation(&self) -> TypeAnnotation {
        match self {
            // NOTE: Uncomment this line if you want to see the underlying errors related to unknown types in protocols.
            // Type::Unknown => TypeAnnotation::Type("{unknown}".to_string())
            Type::Substitution { actual_type, .. } => actual_type.type_annotation(),
            Type::Generic(name) => TypeAnnotation::Type(name.type_name.clone()),
            Type::Void => TypeAnnotation::Type("Void".to_string()),
            Type::Unit => TypeAnnotation::Type("Unit".to_string()),
            Type::Int => TypeAnnotation::Type("Int".to_string()),
            Type::UInt => TypeAnnotation::Type("UInt".to_string()),
            Type::Float => TypeAnnotation::Type("Float".to_string()),
            Type::String => TypeAnnotation::Type("String".to_string()),
            Type::Char => TypeAnnotation::Type("Char".to_string()),
            Type::Bool => TypeAnnotation::Type("Bool".to_string()),
            Type::Array(type_) => TypeAnnotation::Array(Box::new(type_.type_annotation())),
            Type::Struct(s) => s.type_annotation(),
            Type::Enum(e) => e.type_annotation(),
            Type::Union(u) => u.type_annotation(),
            Type::TypeAlias(u) => u.type_annotation(),
            Type::Function(f) => f
                .type_annotation()
                .unwrap_or_else(|| panic!("Closure has no type annotation")),
            Type::Literal { type_, .. } => {
                TypeAnnotation::Literal(Box::new((*type_.clone()).into()))
            }
            _ => panic!("Cannot get type annotation for type {}", self.full_name()),
        }
    }

    pub fn clone_with_concrete_types(
        &self,
        concrete_types: Vec<TypeAnnotation>,
        type_environment: Rc<RefCell<TypeEnvironment>>,
    ) -> Result<Type, String> {
        match self {
            Type::Struct(s) if !matches!(s.type_identifier, TypeIdentifier::MemberType(_, _)) => {
                let TypeIdentifier::GenericType(name, generics) = s.type_identifier.clone() else {
                    return Err(format!(
                        "Cannot clone concrete types for struct {}",
                        self.full_name()
                    ));
                };

                let mut type_map = HashMap::new();

                for (ta, gt) in concrete_types.iter().zip(generics) {
                    type_map.insert(gt, ta);
                }

                let mut fields = Vec::new();

                for StructField {
                    struct_name,
                    field_name,
                    default_value,
                    field_type,
                } in s.fields.iter()
                {
                    let Type::Generic(generic) = field_type.clone() else {
                        fields.push(StructField {
                            struct_name: struct_name.clone(),
                            field_name: field_name.clone(),
                            default_value: default_value.clone(),
                            field_type: field_type.clone(),
                        });

                        continue;
                    };

                    let concrete_type = type_map.get(&generic).ok_or(format!(
                        "No concrete type found for generic type {}",
                        generic.type_name
                    ))?;

                    let concrete_type =
                        check_type_annotation(concrete_type, &vec![], type_environment.clone())?;

                    fields.push(StructField {
                        struct_name: struct_name.clone(),
                        field_name: field_name.clone(),
                        default_value: default_value.clone(),
                        field_type: concrete_type,
                    });
                }

                Ok(Type::Struct(Struct {
                    type_identifier: TypeIdentifier::ConcreteType(name, concrete_types),
                    embedded_structs: vec![], // TODO: Implement embedded structs,
                    fields,
                }))
            }
            Type::Struct(s) if matches!(s.type_identifier, TypeIdentifier::MemberType(_, _)) => {
                let TypeIdentifier::MemberType(enum_name, _) = s.type_identifier.clone() else {
                    return Err(format!(
                        "Cannot clone concrete types for enum {}",
                        self.full_name()
                    ));
                };

                let TypeIdentifier::GenericType(name, generics) = *enum_name else {
                    return Err(format!(
                        "Cannot clone concrete types for enum {}",
                        self.full_name()
                    ));
                };

                let mut type_map = HashMap::new();

                for (ta, gt) in concrete_types.iter().zip(generics) {
                    type_map.insert(gt, ta);
                }

                let mut cloned_fields = Vec::new();

                for StructField {
                    struct_name,
                    field_name,
                    default_value,
                    field_type,
                } in s.fields.iter()
                {
                    let Type::Generic(generic) = field_type.clone() else {
                        cloned_fields.push(StructField {
                            struct_name: struct_name.clone(),
                            field_name: field_name.clone(),
                            default_value: default_value.clone(),
                            field_type: field_type.clone(),
                        });
                        continue;
                    };

                    let concrete_type = type_map.get(&generic).ok_or(format!(
                        "No concrete type found for generic type {}",
                        generic.type_name
                    ))?;

                    let concrete_type =
                        check_type_annotation(concrete_type, &vec![], type_environment.clone())?;

                    cloned_fields.push(StructField {
                        struct_name: struct_name.clone(),
                        field_name: field_name.clone(),
                        default_value: default_value.clone(),
                        field_type: concrete_type,
                    });
                }

                let member_type = Type::Struct(Struct {
                    type_identifier: TypeIdentifier::MemberType(
                        Box::new(TypeIdentifier::ConcreteType(
                            name.clone(),
                            concrete_types.clone(),
                        )),
                        s.type_identifier.to_string(),
                    ),
                    embedded_structs: s.embedded_structs.clone(),
                    fields: cloned_fields,
                });

                Ok(member_type)
            }
            Type::Enum(r#enum) => {
                let TypeIdentifier::GenericType(name, generics) = r#enum.type_identifier.clone()
                else {
                    return Err(format!(
                        "Cannot clone concrete types for enum {}",
                        self.full_name()
                    ));
                };

                let mut type_map = HashMap::new();

                for (ta, gt) in concrete_types.iter().zip(generics) {
                    type_map.insert(gt, ta);
                }

                let type_identifier =
                    TypeIdentifier::ConcreteType(name.clone(), concrete_types.clone());

                let mut shared_fields = Vec::new();

                for StructField {
                    struct_name,
                    field_name,
                    default_value,
                    field_type,
                } in r#enum.shared_fields.iter()
                {
                    let Type::Generic(generic) = field_type.clone() else {
                        shared_fields.push(StructField {
                            struct_name: struct_name.clone(),
                            field_name: field_name.clone(),
                            default_value: default_value.clone(),
                            field_type: field_type.clone(),
                        });
                        continue;
                    };

                    let concrete_type = type_map.get(&generic).ok_or(format!(
                        "No concrete type found for generic type {}",
                        generic.type_name
                    ))?;

                    let concrete_type =
                        check_type_annotation(concrete_type, &vec![], type_environment.clone())?;

                    shared_fields.push(StructField {
                        struct_name: struct_name.clone(),
                        field_name: field_name.clone(),
                        default_value: default_value.clone(),
                        field_type: concrete_type,
                    });
                }

                let mut members = HashMap::new();

                for (member_name, type_) in r#enum.members.iter() {
                    let Type::Struct(r#struct) = type_ else {
                        return Err(format!(
                            "Enum members are not of type EnumMember {}",
                            type_.full_name()
                        ));
                    };

                    let mut fields = Vec::new();

                    for StructField {
                        struct_name,
                        field_name,
                        default_value,
                        field_type,
                    } in r#struct.fields.iter()
                    {
                        let field_name = field_name.clone();
                        let field_type = field_type.clone();

                        let Type::Generic(generic) = field_type else {
                            fields.push(StructField {
                                struct_name: struct_name.clone(),
                                field_name: field_name.clone(),
                                default_value: default_value.clone(),
                                field_type,
                            });

                            continue;
                        };

                        let concrete_type = type_map.get(&generic).ok_or(format!(
                            "No concrete type found for generic type {}",
                            generic.type_name
                        ))?;

                        let concrete_type = check_type_annotation(
                            concrete_type,
                            &vec![],
                            type_environment.clone(),
                        )?;

                        fields.push(StructField {
                            struct_name: struct_name.clone(),
                            field_name: field_name.clone(),
                            default_value: default_value.clone(),
                            field_type: concrete_type,
                        });
                    }

                    let member_type = Type::Struct(Struct {
                        type_identifier: TypeIdentifier::MemberType(
                            Box::new(TypeIdentifier::ConcreteType(
                                name.clone(),
                                concrete_types.clone(),
                            )),
                            member_name.clone(),
                        ),
                        embedded_structs: r#struct.embedded_structs.clone(),
                        fields,
                    });

                    members.insert(member_name.clone(), member_type);
                }

                let enum_ = Type::Enum(Enum {
                    type_identifier,
                    shared_fields,
                    members,
                });

                Ok(enum_)
            }
            Type::Function(Function {
                identifier,
                param,
                return_type,
            }) => {
                let Some(TypeIdentifier::GenericType(name, generics)) = identifier else {
                    return Err(format!(
                        "Cannot clone concrete types for enum {}",
                        self.full_name()
                    ));
                };

                let mut type_map = HashMap::new();

                for (ta, gt) in concrete_types.iter().zip(generics) {
                    type_map.insert(gt, ta);
                }

                let cloned_param = match param {
                    Some(Parameter { identifier, type_ }) => {
                        if let Type::Generic(generic) = *type_.clone() {
                            let concrete_type = type_map.get(&generic).ok_or(format!(
                                "No concrete type found for generic type {}",
                                generic.type_name
                            ))?;

                            let concrete_type = check_type_annotation(
                                concrete_type,
                                &vec![],
                                type_environment.clone(),
                            )?;

                            Some(Parameter {
                                identifier: identifier.clone(),
                                type_: Box::new(concrete_type),
                            })
                        } else {
                            None
                        }
                    }
                    None => None,
                };

                let cloned_return_type = if let Type::Generic(generic) = *return_type.clone() {
                    let concrete_type = type_map.get(&generic).ok_or(format!(
                        "No concrete type found for generic type {}",
                        generic.type_name
                    ))?;

                    let concrete_type =
                        check_type_annotation(concrete_type, &vec![], type_environment.clone())?;

                    Box::new(concrete_type)
                } else {
                    return_type.clone()
                };

                Ok(Type::Function(Function {
                    identifier: if identifier.is_some() {
                        Some(TypeIdentifier::ConcreteType(
                            name.clone(),
                            concrete_types.clone(),
                        ))
                    } else {
                        None
                    },
                    param: cloned_param,
                    return_type: cloned_return_type,
                }))
            }
            _ => Err(format!(
                "Cannot clone concrete types for type {}",
                self.full_name()
            )),
        }
    }
}

impl FullName for Type {
    fn full_name(&self) -> String {
        match self {
            Type::Substitution { actual_type, .. } => actual_type.full_name(),
            Type::Unknown => "{unknown}".to_string(),
            Type::Generic(GenericType { type_name }) => type_name.to_string(),
            Type::Void => "Void".to_string(),
            Type::Unit => "Unit".to_string(),
            Type::Int => "Int".to_string(),
            Type::UInt => "UInt".to_string(),
            Type::Float => "Float".to_string(),
            Type::String => "String".to_string(),
            Type::Char => "Char".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::Array(t) => format!("[{}]", t.full_name()),
            Type::Struct(s) => s.full_name(),
            Type::Enum(u) => u.full_name(),
            Type::Union(u) => u.full_name(),
            Type::TypeAlias(t) => t.full_name(),
            Type::Function(f) => f.full_name(),
            Type::Literal { name, type_ } => format!("#{}: {}", type_.full_name(), name),
            Type::Tuple(e) => format!(
                "({})",
                e.iter()
                    .map(|t| t.full_name())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Type::Protocol(t) => t.full_name(),
        }
    }
}

impl From<Type> for parser::Literal {
    fn from(val: Type) -> Self {
        match val {
            Type::Void => parser::Literal::Unit,
            Type::Unit => parser::Literal::Unit,
            Type::Int => parser::Literal::Int(0),
            Type::UInt => parser::Literal::UInt(0),
            Type::Float => parser::Literal::Float(0.0),
            Type::String => parser::Literal::String("".to_string()),
            Type::Char => parser::Literal::Char(' '),
            Type::Bool => parser::Literal::Bool(false),
            _ => panic!("Cannot convert type {} to literal", val.full_name()),
        }
    }
}

impl FromStr for Type {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Void" => Ok(Type::Void),
            "Unit" => Ok(Type::Unit),
            "Int" => Ok(Type::Int),
            "UInt" => Ok(Type::UInt),
            "Float" => Ok(Type::Float),
            "String" => Ok(Type::String),
            "Char" => Ok(Type::Char),
            "Bool" => Ok(Type::Bool),
            _ => Err(format!("Cannot convert string {} to type", s)),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.full_name().fmt(f)
    }
}

impl ToKey for Type {
    fn to_key(&self) -> String {
        self.type_annotation().to_key()
    }
}

/// Check if two types are equal
/// The logic is as follows:
/// - The 'left' type can be made from the 'right' type
/// - The 'right' type cannot be made from the 'left' type
///
/// # Examples
/// ```
/// use crate::types::{Type, type_equals};
///
/// let literal_int = Type::Literal(Literal::Int(0));
/// let int = Type::Int;
///
/// // Left type is stricter than right type.
/// // This causes the function to return false as they are not equal
/// assert_eq!(type_equals(&literal_int, &int), false);
///
/// // Left type is less strict than right type.
/// // This causes the function to return true as the left type can be made from the right type
/// assert_eq!(type_equals(&int, &literal_int), true);
pub fn type_equals(left: &Type, right: &Type) -> bool {
    match (left, right) {
        (Type::Substitution { actual_type, .. }, right) => type_equals(actual_type, right),
        (left, Type::Substitution { actual_type, .. }) => type_equals(left, actual_type),
        (Type::UInt, Type::Literal { name, type_ }) if matches!(**type_, Type::Int) => {
            name.parse::<u64>().is_ok()
        }
        (Type::Int, Type::Literal { name, type_ }) if matches!(**type_, Type::UInt) => {
            name.parse::<i64>().is_ok()
        }
        (Type::Union(Union { literals, .. }), Type::Literal { .. }) => literals.contains(right),
        (other, Type::Union(Union { literal_type, .. })) => type_equals(other, literal_type),
        (
            Type::Literal { name, type_ },
            Type::Literal {
                name: name_2,
                type_: type_2,
            },
        ) => name == name_2 && type_equals(type_, type_2),
        (
            Type::TypeAlias(TypeAlias { types: left, .. }),
            Type::TypeAlias(TypeAlias { types: right, .. }),
        ) => {
            for r in right {
                if !left.iter().any(|l| type_equals(l, r)) {
                    return false;
                }
            }

            true
        }
        (Type::TypeAlias(TypeAlias { types, .. }), other) => {
            types.iter().any(|t| type_equals(t, other))
        }
        (other, Type::TypeAlias(TypeAlias { types, .. })) => {
            types.iter().all(|t| type_equals(other, t))
        }
        (other, Type::Literal { type_, .. }) => type_equals(other, type_),
        (Type::Function(fl), Type::Function(fr)) => {
            type_equals(fl.return_type.as_ref(), fr.return_type.as_ref())
                && fl
                    .param
                    .as_ref()
                    .zip(fr.param.as_ref())
                    .is_none_or(|(p, p2)| type_equals(&p.type_, &p2.type_))
        }
        (
            Type::Enum(Enum {
                type_identifier,
                members,
                ..
            }),
            Type::Struct(Struct {
                type_identifier: TypeIdentifier::MemberType(enum_name, discriminant_name),
                ..
            }),
        ) => *type_identifier == **enum_name && members.contains_key(discriminant_name),
        (
            Type::Struct(Struct {
                type_identifier: left_type_identifier,
                ..
            }),
            Type::Struct(Struct {
                type_identifier: right_type_identifier,
                embedded_structs,
                ..
            }),
        ) => {
            let e = embedded_structs
                .iter()
                .map(|s| s.to_key())
                .chain(std::iter::once(right_type_identifier.to_key()))
                .any(|k| k == left_type_identifier.to_key());

            e
        }
        _ => left.to_key() == right.to_key(),
    }
}

pub fn type_equals_coerce(left: &Type, right: &Type) -> bool {
    match (left, right) {
        (Type::UInt, Type::Literal { name, type_ }) if matches!(**type_, Type::Int) => {
            name.parse::<u64>().is_ok()
        }
        (Type::Int, Type::Literal { name, type_ }) if matches!(**type_, Type::UInt) => {
            name.parse::<i64>().is_ok()
        }
        (Type::Literal { type_, .. }, Type::Literal { type_: type_2, .. }) => {
            type_equals(type_, type_2)
        }
        _ => type_equals(left, right),
    }
}

pub fn type_annotation_equals(left: &TypeAnnotation, right: &TypeAnnotation) -> bool {
    match (left, right) {
        (TypeAnnotation::Literal(l), TypeAnnotation::Literal(r)) => l == r,
        (TypeAnnotation::Array(l), TypeAnnotation::Array(r)) => l == r,
        (TypeAnnotation::Function(lp, lr), TypeAnnotation::Function(rp, rr)) => {
            let p = match (lp.as_ref(), rp.as_ref()) {
                (Some(lp), Some(rp)) => type_annotation_equals(lp.as_ref(), rp.as_ref()),
                (None, None) => true,
                _ => false,
            };

            if !p {
                return false;
            }

            let r = match (lr.as_ref(), rr.as_ref()) {
                (Some(lr), Some(rr)) => type_annotation_equals(lr.as_ref(), rr.as_ref()),
                (None, None) => true,
                _ => false,
            };

            r
        }
        (TypeAnnotation::Type(l), TypeAnnotation::Type(r)) => l == r,
        _ => false,
    }
}
