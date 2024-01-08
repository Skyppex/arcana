
#[derive(Debug, Clone)]
pub enum Statement {
    Program { statements: Vec<Statement> },
    StructDeclaration(StructDeclaration),
    UnionDeclaration(UnionDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub enum Expression {
    None, // For testing purposes

    VariableDeclaration(VariableDeclaration),
    If(If),
    Assignment(Assignment),
    Member(Member),
    Literal(Literal),
    Call(Call),
    Unary(Unary),
    Binary(Binary),
    Ternary(Ternary),
    Block(Vec<Statement>),
    Drop(String),
}

#[derive(Debug, Clone)]
pub struct StructDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub type_name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct UnionDeclaration{
    pub access_modifier: Option<AccessModifier>,
    pub type_name: String,
    pub members: Vec<UnionMember>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub access_modifier: Option<AccessModifier>,
    pub identifier: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<String>,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub enum Literal {
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
    pub field_position: usize,
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
pub struct VariableDeclaration{
    pub mutable: bool,
    pub type_name: String,
    pub identifier: String,
    pub initializer: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct If{
    pub r#if: ConditionBlock,
    pub else_ifs: Option<Vec<ConditionBlock>>,
    pub r#else: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct Assignment{
    pub member: Box<Expression>,
    pub initializer: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Call{
    pub caller: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Unary{
    pub operator: UnaryOperator,
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Binary{
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Ternary{
    pub condition: Box<Expression>,
    pub true_expression: Box<Expression>,
    pub false_expression: Box<Expression>,
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
    BooleanLogicalAnd,
    BooleanLogicalOr,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}
