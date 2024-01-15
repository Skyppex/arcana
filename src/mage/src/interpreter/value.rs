use std::fmt::Display;

pub enum Value {
    Void,
    Unassigned,
    Unit,
    Number(Number),
    String(String),
    Boolean(bool),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Void => write!(f, "void"),
            Value::Unassigned => write!(f, "unassigned"),
            Value::Unit => write!(f, "()"),
            Value::Number(number) => write!(f, "{}", number),
            Value::String(string) => write!(f, "{}", string),
            Value::Boolean(boolean) => write!(f, "{}", boolean),
        }
    }
}

pub enum Number {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    F32(f32),
    F64(f64),
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::I8(number) => write!(f, "{}", number),
            Number::I16(number) => write!(f, "{}", number),
            Number::I32(number) => write!(f, "{}", number),
            Number::I64(number) => write!(f, "{}", number),
            Number::I128(number) => write!(f, "{}", number),
            Number::U8(number) => write!(f, "{}", number),
            Number::U16(number) => write!(f, "{}", number),
            Number::U32(number) => write!(f, "{}", number),
            Number::U64(number) => write!(f, "{}", number),
            Number::U128(number) => write!(f, "{}", number),
            Number::F32(number) => write!(f, "{}", number),
            Number::F64(number) => write!(f, "{}", number),
        }
    }
}

pub struct Variable {
    pub identifier: String,
    pub value: Value,
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.identifier, self.value)
    }
}