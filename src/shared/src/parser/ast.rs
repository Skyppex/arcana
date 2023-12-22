#[derive(Debug, Clone)]
pub enum Statement {
    Program { statements: Vec<Result<Statement, String>> },
    StructDeclaration {
        access_modifier: Option<AccessModifier>,
        type_name: String,
        fields: Vec<StructField>,
    },
    UnionDeclaration {
        access_modifier: Option<AccessModifier>,
        type_name: String,
        fields: Vec<UnionMember>,
    },
    FunctionDeclaration {
        access_modifier: Option<AccessModifier>,
        identifier: String,
        parameters: Vec<Parameter>,
        return_type: Option<String>,
        body: Expression,
    },
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub enum Expression {
    VariableDeclaration {
        mutable: bool,
        type_name: String,
        identifier: String,
        initializer: Option<Box<Expression>>,
    },
    If {
        r#if: ConditionBlock,
        else_ifs: Option<Vec<ConditionBlock>>,
        r#else: Option<Box<Expression>>,
    },
    Assignment {
        member: Box<Expression>,
        initializer: Box<Expression>,
    },
    Member(Member),
    Literal(Literal),
    Call {
        caller: Box<Expression>,
        arguments: Vec<Expression>,
    },
    Unary {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    Ternary {
        condition: Box<Expression>,
        true_expression: Box<Expression>,
        false_expression: Box<Expression>,
    },
    Block {
        statements: Vec<Statement>,
    },
    Drop {
        identifier: String,
    },
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Int128(i128),
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    UInt128(u128),
    Float32(f32),
    Float64(f64),
    String(String),
    Char(char),
    Bool(bool),
    Struct {
        type_name: String,
        field_initializers: Option<Vec<FieldInitializer>>,
    },
    Union {
        type_name: String,
        member: String,
        field_initializers: Option<Vec<FieldInitializer>>,
    },
}

#[derive(Debug, Clone)]
pub struct ConditionBlock {
    pub condition: Box<Expression>,
    pub block: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub access_modifier: Option<AccessModifier>,
    pub mutable: bool,
    pub identifier: String,
    pub type_name: String,
}

#[derive(Debug, Clone)]
pub struct FieldInitializer {
    pub identifier: Option<String>,
    pub initializer: Expression,
}

#[derive(Debug, Clone)]
pub struct UnionMember {
    pub identifier: String,
    pub fields: Vec<UnionMemberField>,
}

#[derive(Debug, Clone)]
pub struct UnionMemberField {
    pub identifier: Option<String>,
    pub type_name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessModifier {
    Public,
    Internal,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub identifier: String,
    pub type_name: String,
}

#[derive(Debug, Clone)]
pub enum Member {
    Identifier { symbol: String },
    MemberAccess {
        object: Box<Expression>,
        member: Box<Member>,
        symbol: String,
    },
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Negation,
    LogicalNot,
    BitwiseNot,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift,
    LogicalAnd,
    LogicalOr,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}
