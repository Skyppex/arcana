pub mod type_checker;
pub mod type_environment;
pub mod ast;
pub mod full_name;

mod statements;
mod expressions;
mod scope;

pub use type_checker::*;
pub use type_environment::*;
pub use full_name::*;

use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{parser, types::{GenericType, TypeAnnotation, TypeIdentifier}};

use self::statements::check_type_annotation;

#[derive(Debug, Clone)]
pub struct Struct {
    pub type_identifier: TypeIdentifier,
    pub fields: HashMap<String, Type>,
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
    pub field_type: Box<Type>,
}

impl FullName for StructField {
    fn full_name(&self) -> String {
        format!("{}.{}: {}", self.struct_name, self.field_name, self.field_type)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub type_identifier: TypeIdentifier,
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
pub struct EnumMember {
    pub enum_name: TypeIdentifier,
    pub discriminant_name: String,
    pub fields: HashMap<String, Type>,
}

impl FullName for EnumMember {
    fn full_name(&self) -> String {
        format!("{}::{}", self.enum_name, self.discriminant_name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumMemberField {
    pub enum_name: TypeIdentifier,
    pub discriminant_name: String,
    pub field_name: String,
    pub field_type: Box<Type>,
}

impl FullName for EnumMemberField {
    fn full_name(&self) -> String {
        format!("{}::{}.{}", self.enum_name, self.discriminant_name, self.field_name)
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
        format!("{} {{ {} }}",
            self.type_identifier.to_string(),
            self.literals.iter().map(|l| l.to_string()).collect::<Vec<String>>().join(" | "))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Trait {
    pub type_identifier: TypeIdentifier,
    pub associated_types: HashMap<String, Type>,
    pub functions: HashMap<String, Function>,
}

impl FullName for Trait {
    fn full_name(&self) -> String {
        self.type_identifier.to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub identifier: TypeIdentifier,
    pub parameters: HashMap<String, Type>,
    pub return_type: Box<Type>,
}

impl Function {
    pub fn type_annotation(&self) -> TypeAnnotation {
        TypeAnnotation::from(self.identifier.clone())
    }
}

impl FullName for Function {
    fn full_name(&self) -> String {
        self.identifier.to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Generic(GenericType),
    Void,
    Unit,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    String,
    Char,
    Bool,
    Array(Box<Type>),
    Struct(Struct),
    StructField(StructField),
    Enum(Enum),
    EnumMember(EnumMember),
    EnumMemberField(EnumMemberField),
    Union(Union),
    Trait(Trait),
    Function(Function),
    Literal {
        name: String, // String representation of the literal
        type_: Box<Type>,
    }
}

impl Type {
    pub fn option() -> Type {
        let option_name = "Option".to_string();
        
        let option_ident = TypeIdentifier::GenericType(
            option_name,
            vec![GenericType { type_name: "T".to_string() }]);

        let some_member_ident = TypeIdentifier::MemberType(
            Box::new(option_ident.clone()),
            "Some".to_string());

        let some_member_field_ident = TypeIdentifier::MemberType(
            Box::new(some_member_ident.clone()),
            "f0".to_string());

        let none_member_ident = TypeIdentifier::MemberType(
            Box::new(option_ident.clone()),
            "None".to_string());

        Type::Enum(Enum {
            type_identifier: option_ident,
            members: vec![
                ("Some".to_string(), Type::EnumMember(EnumMember {
                    enum_name: some_member_ident,
                    discriminant_name: "Some".to_string(),
                    fields: vec![("f0".to_string(), Type::EnumMemberField(EnumMemberField {
                        enum_name: some_member_field_ident,
                        discriminant_name: "Some".to_string(),
                        field_name: "f0".to_string(),
                        field_type: Box::new(Type::Generic(GenericType { type_name: "T".to_string() })),
                    }))].into_iter().collect(),
                })),
                ("None".to_string(), Type::EnumMember(EnumMember {
                    enum_name: none_member_ident,
                    discriminant_name: "None".to_string(),
                    fields: HashMap::new(),
                })),
            ].into_iter().collect(),
        })
    }

    pub fn option_of(concrete: Type) -> Type {
        Type::Enum(Enum {
            type_identifier: TypeIdentifier::ConcreteType(
                "Option".to_string(),
                vec![concrete.type_annotation()]),
            members: vec![
                ("Some".to_string(), Type::EnumMember(EnumMember {
                    enum_name: TypeIdentifier::ConcreteType(
                        "Option".to_string(),
                        vec![concrete.type_annotation()]),
                    discriminant_name: "Some".to_string(),
                    fields: vec![("f0".to_string(), concrete.clone())].into_iter().collect(),
                })),
                ("None".to_string(), Type::EnumMember(EnumMember {
                    enum_name: TypeIdentifier::ConcreteType(
                        "Option".to_string(),
                        vec![concrete.type_annotation()]),
                    discriminant_name: "None".to_string(),
                    fields: HashMap::new(),
                })),
            ].into_iter().collect(),
        })
    }

    pub fn from_string(type_name: &str) -> Option<Type> {
        match type_name {
            "void" => Some(Type::Void),
            "unit" => Some(Type::Unit),
            "i8" => Some(Type::I8),
            "i16" => Some(Type::I16),
            "i32" => Some(Type::I32),
            "i64" => Some(Type::I64),
            "i128" => Some(Type::I128),
            "u8" => Some(Type::U8),
            "u16" => Some(Type::U16),
            "u32" => Some(Type::U32),
            "u64" => Some(Type::U64),
            "u128" => Some(Type::U128),
            "f32" => Some(Type::F32),
            "f64" => Some(Type::F64),
            "string" => Some(Type::String),
            "char" => Some(Type::Char),
            "bool" => Some(Type::Bool),
            _ => None,
        }
    }

    pub fn to_string(&self) -> String {
        self.full_name()
    }

    pub fn from_literal(literal: &parser::Literal) -> Result<Type, String> {
        match literal {
            parser::Literal::Unit => Ok(Type::Literal { name: Type::Unit.to_string(), type_: Box::new(Type::Unit) }),
            parser::Literal::I8(v) => Ok(Type::Literal { name: v.to_string(), type_: Box::new(Type::I8) }),
            parser::Literal::I16(v) => Ok(Type::Literal { name: v.to_string(), type_: Box::new(Type::I16) }),
            parser::Literal::I32(v) => Ok(Type::Literal { name: v.to_string(), type_: Box::new(Type::I32) }),
            parser::Literal::I64(v) => Ok(Type::Literal { name: v.to_string(), type_: Box::new(Type::I64) }),
            parser::Literal::I128(v) => Ok(Type::Literal { name: v.to_string(), type_: Box::new(Type::I128) }),
            parser::Literal::U8(v) => Ok(Type::Literal { name: v.to_string(), type_: Box::new(Type::U8) }),
            parser::Literal::U16(v) => Ok(Type::Literal { name: v.to_string(), type_: Box::new(Type::U16) }),
            parser::Literal::U32(v) => Ok(Type::Literal { name: v.to_string(), type_: Box::new(Type::U32) }),
            parser::Literal::U64(v) => Ok(Type::Literal { name: v.to_string(), type_: Box::new(Type::U64) }),
            parser::Literal::U128(v) => Ok(Type::Literal { name: v.to_string(), type_: Box::new(Type::U128) }),
            parser::Literal::F32(v) => Ok(Type::Literal { name: v.to_string(), type_: Box::new(Type::F32) }),
            parser::Literal::F64(v) => Ok(Type::Literal { name: v.to_string(), type_: Box::new(Type::F64) }),
            parser::Literal::Char(v) => Ok(Type::Literal { name: format!("'{}'", v), type_: Box::new(Type::Char) }),
            parser::Literal::String(v) => Ok(Type::Literal { name: format!("\"{}\"", v), type_: Box::new(Type::String) }),
            parser::Literal::Bool(v) => Ok(Type::Literal { name: v.to_string(), type_: Box::new(Type::Bool) }),
            _ => Err(format!("Cannot convert literal {:?} to type", literal)),
        }
    }

    pub fn type_identifier(&self) -> TypeIdentifier {
        match self {
            Type::Generic(name) => TypeIdentifier::Type(name.type_name.clone()),
            Type::Void => TypeIdentifier::Type("void".to_string()),
            Type::Unit => TypeIdentifier::Type("unit".to_string()),
            Type::I8 => TypeIdentifier::Type("i8".to_string()),
            Type::I16 => TypeIdentifier::Type("i16".to_string()),
            Type::I32 => TypeIdentifier::Type("i32".to_string()),
            Type::I64 => TypeIdentifier::Type("i64".to_string()),
            Type::I128 => TypeIdentifier::Type("i128".to_string()),
            Type::U8 => TypeIdentifier::Type("u8".to_string()),
            Type::U16 => TypeIdentifier::Type("u16".to_string()),
            Type::U32 => TypeIdentifier::Type("u32".to_string()),
            Type::U64 => TypeIdentifier::Type("u64".to_string()),
            Type::U128 => TypeIdentifier::Type("u128".to_string()),
            Type::F32 => TypeIdentifier::Type("f32".to_string()),
            Type::F64 => TypeIdentifier::Type("f64".to_string()),
            Type::String => TypeIdentifier::Type("string".to_string()),
            Type::Char => TypeIdentifier::Type("char".to_string()),
            Type::Bool => TypeIdentifier::Type("bool".to_string()),
            Type::Struct(s) => s.type_identifier.clone(),
            Type::StructField(sf) => TypeIdentifier::MemberType(
                Box::new(sf.struct_name.clone()),
                sf.field_name.clone()),
            Type::Enum(u) => u.type_identifier.clone(),
            Type::EnumMember(um) => TypeIdentifier::MemberType(
                Box::new(um.enum_name.clone()),
                um.discriminant_name.clone()),
            Type::EnumMemberField(umf) => TypeIdentifier::MemberType(
                Box::new(TypeIdentifier::MemberType(
                    Box::new(umf.enum_name.clone()),
                    umf.discriminant_name.clone())),
                umf.field_name.clone()),
            Type::Union(u) => u.type_identifier.clone(),
            Type::Function(f) => f.identifier.clone(),
            _ => panic!("Cannot get type identifier for type {}", self.full_name()),
        }
    }

    pub fn type_annotation(&self) -> TypeAnnotation {
        match self {
            Type::Generic(name) => TypeAnnotation::Type(name.type_name.clone()),
            Type::Void => TypeAnnotation::Type("void".to_string()),
            Type::Unit => TypeAnnotation::Type("unit".to_string()),
            Type::I8 => TypeAnnotation::Type("i8".to_string()),
            Type::I16 => TypeAnnotation::Type("i16".to_string()),
            Type::I32 => TypeAnnotation::Type("i32".to_string()),
            Type::I64 => TypeAnnotation::Type("i64".to_string()),
            Type::I128 => TypeAnnotation::Type("i128".to_string()),
            Type::U8 => TypeAnnotation::Type("u8".to_string()),
            Type::U16 => TypeAnnotation::Type("u16".to_string()),
            Type::U32 => TypeAnnotation::Type("u32".to_string()),
            Type::U64 => TypeAnnotation::Type("u64".to_string()),
            Type::U128 => TypeAnnotation::Type("u128".to_string()),
            Type::F32 => TypeAnnotation::Type("f32".to_string()),
            Type::F64 => TypeAnnotation::Type("f64".to_string()),
            Type::String => TypeAnnotation::Type("string".to_string()),
            Type::Char => TypeAnnotation::Type("char".to_string()),
            Type::Bool => TypeAnnotation::Type("bool".to_string()),
            Type::Array(type_) => TypeAnnotation::Array(Box::new(type_.type_annotation())),
            Type::Struct(s) => s.type_annotation(),
            Type::Enum(e) => e.type_annotation(),
            Type::Union(u) => u.type_annotation(),
            Type::Function(f) => f.type_annotation(),
            Type::Literal { type_, .. } => TypeAnnotation::Literal(Box::new((*type_.clone()).into())),
            _ => panic!("Cannot get type annotation for type {}", self.full_name()),
        }
    }

    pub fn clone_with_concrete_types(&self, concrete_types: Vec<TypeAnnotation>, type_environment: Rc<RefCell<TypeEnvironment>>) -> Result<Type, String> {
        match self {
            Type::Struct(s) => {
                let TypeIdentifier::GenericType(name, generics) = s.type_identifier.clone() else {
                    return Err(format!("Cannot clone concrete types for struct {}", self.full_name()));
                };

                let mut type_map = HashMap::new();

                for (ta, gt) in concrete_types.iter().zip(generics) {
                    type_map.insert(gt, ta);
                }

                let mut fields = HashMap::new();

                for (_, type_) in s.fields.iter() {
                    let Type::StructField(StructField {
                        struct_name: _,
                        field_name,
                        field_type }) = type_ else {
                        return Err(format!("Struct fields are not of type StructField {}", type_.full_name()));
                    };

                    let Type::Generic( generic ) = *field_type.clone() else {
                        fields.insert(field_name.clone(), type_.clone());
                        continue;
                    };
                    
                    let concrete_type = type_map.get(&generic)
                        .ok_or(format!("No concrete type found for generic type {}", generic.type_name))?;

                    let concrete_type = check_type_annotation(&concrete_type, &vec![], type_environment.clone())?;

                    fields.insert(field_name.clone(), concrete_type);
                }

                Ok(Type::Struct(Struct {
                    type_identifier: TypeIdentifier::ConcreteType(name, concrete_types),
                    fields,
                }))
            },
            Type::Enum(u) => {
                let TypeIdentifier::GenericType(name, generics) = u.type_identifier.clone() else {
                    return Err(format!("Cannot clone concrete types for enum {}", self.full_name()));
                };

                let mut type_map = HashMap::new();

                for (ta, gt) in concrete_types.iter().zip(generics) {
                    type_map.insert(gt, ta);
                }

                let type_identifier = TypeIdentifier::ConcreteType(name.clone(), concrete_types.clone());

                let mut members = HashMap::new();

                for (member_name, type_) in u.members.iter() {
                    let Type::EnumMember(u) = type_ else {
                        return Err(format!("Enum members are not of type EnumMember {}", type_.full_name()));
                    };

                    let mut fields = HashMap::new();

                    for (_, type_) in u.fields.iter() {
                        let Type::EnumMemberField(EnumMemberField {
                            enum_name: _,
                            discriminant_name: _,
                            field_name,
                            field_type }) = type_ else {
                            return Err(format!("Enum member fields are not of type EnumMemberField {}", type_.full_name()));
                        };
                        
                        let Type::Generic( generic ) = *field_type.clone() else {
                            fields.insert(field_name.clone(), type_.clone());
                            continue;
                        };
                        
                        let concrete_type = type_map.get(&generic)
                            .ok_or(format!("No concrete type found for generic type {}", generic.type_name))?;
    
                        let concrete_type = check_type_annotation(&concrete_type, &vec![], type_environment.clone())?;

                        fields.insert(
                            field_name.clone(),
                            Type::EnumMemberField(EnumMemberField {
                                enum_name: type_identifier.clone(),
                                discriminant_name: u.discriminant_name.clone(),
                                field_name: field_name.clone(),
                                field_type: Box::new(concrete_type)
                            }));
                    }

                    let member_type = Type::EnumMember(EnumMember {
                        enum_name: TypeIdentifier::ConcreteType(name.clone(), concrete_types.clone()),
                        discriminant_name: member_name.clone(),
                        fields,
                    });

                    members.insert(member_name.clone(), member_type);
                }

                let enum_ = Type::Enum(Enum {
                    type_identifier,
                    members,
                });

                Ok(enum_)
            }
            _ => Err(format!("Cannot clone concrete types for type {}", self.full_name())),
        }
    }
}

impl FullName for Type {
    fn full_name(&self) -> String {
        match self {
            Type::Generic(GenericType { type_name }) => type_name.to_string(),
            Type::Void => "void".to_string(),
            Type::Unit => "unit".to_string(),
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::I128 => "i128".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::U128 => "u128".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::String => "string".to_string(),
            Type::Char => "char".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Array(t) => format!("[{}]", t.full_name()),
            Type::Struct(s) => s.full_name(),
            Type::StructField(sf) => sf.full_name(),
            Type::Enum(u) => u.full_name(),
            Type::EnumMember(um) => um.full_name(),
            Type::EnumMemberField(umf) => umf.full_name(),
            Type::Union(u) => u.full_name(),
            Type::Function(f) => f.full_name(),
            Type::Literal { name, type_ } => format!("#{}: {}", type_.full_name(), name),
        }
    }
}

impl Into<parser::Literal> for Type {
    fn into(self) -> parser::Literal {
        match self {
            Type::Void => parser::Literal::Unit,
            Type::Unit => parser::Literal::Unit,
            Type::I8 => parser::Literal::I8(0),
            Type::I16 => parser::Literal::I16(0),
            Type::I32 => parser::Literal::I32(0),
            Type::I64 => parser::Literal::I64(0),
            Type::I128 => parser::Literal::I128(0),
            Type::U8 => parser::Literal::U8(0),
            Type::U16 => parser::Literal::U16(0),
            Type::U32 => parser::Literal::U32(0),
            Type::U64 => parser::Literal::U64(0),
            Type::U128 => parser::Literal::U128(0),
            Type::F32 => parser::Literal::F32(0.0),
            Type::F64 => parser::Literal::F64(0.0),
            Type::String => parser::Literal::String("".to_string()),
            Type::Char => parser::Literal::Char(' '),
            Type::Bool => parser::Literal::Bool(false),
            _ => panic!("Cannot convert type {} to literal", self.full_name()),
        }
    
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_string().fmt(f)
    }
}
