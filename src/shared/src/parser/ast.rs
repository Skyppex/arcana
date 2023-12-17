

pub enum Statement {
    Program { statements: Vec<Result<Statement, String>> },
    VariableDeclaration {
        identifier: String,
        mutable: bool,
        type_name: String,
    },
    IfStatement {
        r#if: ConditionBlock,
        else_ifs: Option<Vec<ConditionBlock>>,
        r#else: Option<Expression>,
    },
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
        return_type: String,
        body: Expression,
    },
    Expression(Expression),
}

pub enum Expression {
    VariableDeclaration {
        type_name: String,
        mutable: bool,
        identifier: String,
        initializer: Box<Expression>,
    },
    Assignment {
        member: Member,

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
        field_initializers: Vec<FieldInitializer>,
    },
    Union {
        type_name: String,
        member: String,
        field_initializers: Vec<FieldInitializer>,
    },
}

pub struct ConditionBlock {
    pub condition: Expression,
    pub block: Expression,
}

pub struct StructField {
    pub access_modifier: Option<AccessModifier>,
    pub mutable: bool,
    pub identifier: String,
    pub type_name: String,
}

pub struct FieldInitializer {
    pub identifier: String,
    pub initializer: Expression,
}

pub struct UnionMember {
    pub identifier: String,
    pub fields: Vec<UnionMemberField>,
}

pub struct UnionMemberField {
    pub identifier: String,
    pub type_name: String,
}

pub enum AccessModifier {
    Public,
    Internal,
}

pub struct Parameter {
    pub identifier: String,
    pub type_name: String,
}

pub enum Member {
    Identifier { symbol: String },
    MemberAccess {
        object: Box<Expression>,
        member: Box<Member>,
        symbol: String,
    },
}

pub enum UnaryOperator {
    Negation,
    LogicalNot,
    BitwiseNot,
}

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
