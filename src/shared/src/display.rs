use crate::{parser::{
    Statement,
    Expression,
    Member,
    Literal,
    StructField,
    UnionMember,
    AccessModifier,
    UnionMemberField,
    FieldInitializer,
    UnaryOperator,
    BinaryOperator,
    Parameter,
    ConditionBlock,
    StructDeclaration,
    UnionDeclaration,
    FunctionDeclaration,
    VariableDeclaration,
    If,
    Assignment,
    Call,
    Unary,
    Binary,
    Ternary
}, type_checker::{ast::{TypedStatement, TypedExpression}, self}};

pub struct Indent {
    level: usize,
}

impl Indent {
    pub fn new() -> Indent {
        Indent {
            level: 0,
        }
    }

    fn increase(&mut self) {
        self.level += 1;
    }

    fn decrease(&mut self) {
        self.level -= 1;
    }

    fn dash(&self) -> String {
        let mut result = String::new();
        for _ in 0..self.level - 1 {
            result.push_str("┆ ");
        }
        result.push_str("├─");
        result
    }

    fn dash_end(&self) -> String {
        let mut result = String::new();
        for _ in 0..self.level - 1 {
            result.push_str("┆ ");
        }
        result.push_str("╰─");
        result
    }
}

pub trait IndentDisplay {
    fn indent_display(&self, indent: &mut Indent) -> String;
}

impl IndentDisplay for Statement {
    fn indent_display(&self, indent: &mut Indent) -> String {
        match self {
            Statement::Program { statements } => {
                let mut result = String::new();
                for statement in statements {
                    result.push_str(format!("{}\n┆\n", &statement.indent_display(indent)).as_str());
                }
                result
            },
            Statement::StructDeclaration(StructDeclaration {
                access_modifier,
                type_name,
                fields 
            }) => {
                let mut result = String::new();
                result.push_str(format!("<struct declaration> {}\n", type_name).as_str());
                indent.increase();
                if let Some(access_modifier) = access_modifier {
                    result.push_str(format!("{}access_modifier: {}", indent.dash(), access_modifier.indent_display(indent)).as_str());
                } else {
                    result.push_str(format!("{}access_modifier: None", indent.dash()).as_str());
                }
                for field in fields {
                    result.push_str(format!("\n{}{}", indent.dash_end(), field.indent_display(indent)).as_str());
                }
                indent.decrease();
                result
            },
            Statement::UnionDeclaration(UnionDeclaration {
                access_modifier,
                type_name,
                members
            }) => {
                let mut result = String::new();
                result.push_str(format!("<union declaration> {}\n", type_name).as_str());
                indent.increase();
                if let Some(access_modifier) = access_modifier {
                    result.push_str(format!("{}access_modifier: {}\n", indent.dash(), access_modifier.indent_display(indent)).as_str());
                } else {
                    result.push_str(format!("{}access_modifier: None", indent.dash()).as_str());
                }
                for member in members {
                    result.push_str(format!("\n{}{}", indent.dash_end(), member.indent_display(indent)).as_str());
                }
                indent.decrease();
                result
            },
            Statement::FunctionDeclaration(FunctionDeclaration {
                access_modifier,
                identifier,
                parameters,
                return_type,
                body
            }) => {
                let mut result = String::new();
                result.push_str(format!("<function declaration> {}\n", identifier).as_str());
                indent.increase();
                if let Some(access_modifier) = access_modifier {
                    result.push_str(format!("{}access_modifier: {}", indent.dash(), access_modifier.indent_display(indent)).as_str());
                } else {
                    result.push_str(format!("{}access_modifier: None", indent.dash()).as_str());
                }
                for parameter in parameters {
                    result.push_str(format!("\n{}{}\n", indent.dash(), parameter.indent_display(indent)).as_str());
                }
                if let Some(return_type) = return_type {
                    result.push_str(format!("{}return_type: {}\n", indent.dash(), return_type).as_str());
                } else {
                    result.push_str(format!("{}return_type: None\n", indent.dash()).as_str());
                }
                result.push_str(format!("{}body: {}", indent.dash_end(), body.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            Statement::Expression(e) => e.indent_display(indent),
        }
    }
}

impl IndentDisplay for Expression {
    fn indent_display(&self, indent: &mut Indent) -> String {
        match self {
            Expression::None => {
                String::new()
            },
            Expression::VariableDeclaration(VariableDeclaration {
                mutable,
                type_name,
                identifier,
                initializer
            }) => {
                let mut result = String::new();
                result.push_str(format!("<variable declaration> {}\n", identifier).as_str());
                indent.increase();
                result.push_str(format!("{}mutable: {}\n", indent.dash(), mutable).as_str());
                result.push_str(format!("{}type_name: {}\n", indent.dash(), type_name).as_str());
                if let Some(initializer) = initializer {
                    result.push_str(format!("{}initializer: {}", indent.dash_end(), initializer.indent_display(indent)).as_str());
                } else {
                    result.push_str(format!("{}initializer: None", indent.dash_end()).as_str());
                }
                indent.decrease();
                result
            },
            Expression::If(If {
                r#if,
                else_ifs,
                r#else
            }) => {
                let mut result = String::new();
                result.push_str("<if>\n");
                indent.increase();
                result.push_str(format!("{}condition:{}", indent.dash(), r#if.condition.indent_display(indent)).as_str());
                if let Some(else_ifs) = else_ifs {
                    for else_if in else_ifs {
                        result.push_str(format!("\n{}{}\n", indent.dash(), else_if.indent_display(indent)).as_str());
                        result.push_str(format!("\n{}condition: {}\n", indent.dash(), else_if.condition.indent_display(indent)).as_str());
                    }
                } else {
                    result.push_str(format!("\n{}else_ifs: None\n", indent.dash()).as_str());
                }
                if let Some(r#else) = r#else {
                    result.push_str(format!("{}else block: {}", indent.dash_end(), r#else.indent_display(indent)).as_str());
                } else {
                    result.push_str(format!("{}else block: None", indent.dash_end()).as_str());
                }
                indent.decrease();
                result
            },
            Expression::Assignment(Assignment {
                member,
                initializer
            }) => {
                let mut result = String::new();
                result.push_str("<assignment>\n");
                indent.increase();
                result.push_str(format!("{}member: {}\n", indent.dash(), member.indent_display(indent)).as_str());
                result.push_str(format!("{}initializer: {}", indent.dash_end(), initializer.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            Expression::Member(m) => m.indent_display(indent),
            Expression::Literal(l) => l.indent_display(indent),
            Expression::Call(Call {
                caller,
                arguments
            }) => {
                let mut result = String::new();
                result.push_str("<call>\n");
                indent.increase();
                result.push_str(format!("{}caller: {}", indent.dash(), caller.indent_display(indent)).as_str());
                let mut i = 0;
                for argument in arguments {
                    if i < arguments.len() - 1 {
                        result.push_str(format!("\n{}argument: {},", indent.dash(), argument.indent_display(indent)).as_str());
                    } else {
                        result.push_str(format!("\n{}argument: {}", indent.dash_end(), argument.indent_display(indent)).as_str());
                    }
                    i += 1;
                }
                indent.decrease();
                result
            },
            Expression::Unary(Unary {
                operator,
                expression
            }) => {
                let mut result = String::new();
                result.push_str("<unary>\n");
                indent.increase();
                result.push_str(format!("{}operator: {}\n", indent.dash(), operator.indent_display(indent)).as_str());
                result.push_str(format!("{}expression: {}", indent.dash_end(), expression.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            Expression::Binary(Binary {
                left,
                operator,
                right
            }) => {
                let mut result = String::new();
                result.push_str("<binary>\n");
                indent.increase();
                result.push_str(format!("{}left: {}\n", indent.dash(), left.indent_display(indent)).as_str());
                result.push_str(format!("{}operator: {}\n", indent.dash(), operator.indent_display(indent)).as_str());
                result.push_str(format!("{}right: {}", indent.dash_end(), right.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            Expression::Ternary(Ternary {
                condition,
                true_expression,
                false_expression
            }) => {
                let mut result = String::new();
                result.push_str("<ternary>\n");
                indent.increase();
                result.push_str(format!("{}condition: {}\n", indent.dash(), condition.indent_display(indent)).as_str());
                result.push_str(format!("{}true_expression: {}\n", indent.dash(), true_expression.indent_display(indent)).as_str());
                result.push_str(format!("{}false_expression: {}", indent.dash_end(), false_expression.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            Expression::Block(statements) => {
                let mut result = String::new();
                result.push_str("<block>");
                indent.increase();
                let mut i = 0;
                for statement in statements {
                    if i < statements.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), statement.indent_display(indent)).as_str());
                    } else {
                        result.push_str(format!("\n{}{}", indent.dash_end(), statement.indent_display(indent)).as_str());
                    }
                    i += 1;
                }
                indent.decrease();
                result
            },
            Expression::Drop(identifier) => {
                let mut result = String::new();
                result.push_str(format!("<drop> {}", identifier).as_str());
                result
            },
        }
    }
}

impl IndentDisplay for Member {
    fn indent_display(&self, indent: &mut Indent) -> String {
        match self {
            Member::Identifier {
                symbol
            } => {
                let mut result = String::new();
                result.push_str(format!("<identifier> {}", symbol).as_str());
                result
            },
            Member::MemberAccess {
                object,
                member,
                symbol
            } => {
                let mut result = String::new();
                result.push_str("<member access>");
                indent.increase();
                result.push_str(format!("{}object: {}\n", indent.dash(), object.indent_display(indent)).as_str());
                result.push_str(format!("{}member: {}\n", indent.dash(), member.indent_display(indent)).as_str());
                result.push_str(format!("{}symbol: {}", indent.dash_end(), symbol).as_str());
                indent.decrease();
                result
            },
        }
    }
}

impl IndentDisplay for Literal {
    fn indent_display(&self, indent: &mut Indent) -> String {
        match self {
            Literal::I8(v) => v.to_string(),
            Literal::I16(v) => v.to_string(),
            Literal::I32(v) => v.to_string(),
            Literal::I64(v) => v.to_string(),
            Literal::I128(v) => v.to_string(),
            Literal::U8(v) => v.to_string(),
            Literal::U16(v) => v.to_string(),
            Literal::U32(v) => v.to_string(),
            Literal::U64(v) => v.to_string(),
            Literal::U128(v) => v.to_string(),
            Literal::F32(v) => v.to_string(),
            Literal::F64(v) => v.to_string(),
            Literal::String(s) => s.to_string(),
            Literal::Char(c) => c.to_string(),
            Literal::Bool(b) => b.to_string(),
            Literal::Struct {
                type_name,
                field_initializers
            } => {
                let mut result = String::new();
                result.push_str("<struct literal>\n");
                indent.increase();
                result.push_str(format!("{}type_name: {}", indent.dash(), type_name).as_str());
                if let Some(field_initializers) = field_initializers {
                    let mut i = 0;
                    for field in field_initializers {
                        if i < field_initializers.len() - 1 {
                            result.push_str(format!("\n{}{},", indent.dash(), field.indent_display(indent)).as_str());
                        } else {
                            result.push_str(format!("\n{}{}", indent.dash_end(), field.indent_display(indent)).as_str());
                        }
                        i += 1;
                    }
                } else {
                    result.push_str(format!("\n{}field_initializers: None", indent.dash_end()).as_str());
                }
                indent.decrease();
                result
            },
            Literal::Union {
                type_name,
                member,
                field_initializers
            } => {
                let mut result = String::new();
                result.push_str("<union literal>\n");
                indent.increase();
                result.push_str(format!("{}type_name: {}\n", indent.dash(), type_name).as_str());
                result.push_str(format!("{}member: {}", indent.dash(), member).as_str());
                if let Some(field_initializers) = field_initializers {
                    let mut i = 0;
                    for field in field_initializers {
                        if i < field_initializers.len() - 1 {
                            result.push_str(format!("\n{}{},", indent.dash(), field.indent_display(indent)).as_str());
                        } else {
                            result.push_str(format!("\n{}{}", indent.dash_end(), field.indent_display(indent)).as_str());
                        }
                        i += 1;
                    }
                } else {
                    result.push_str(format!("\n{}field_initializers: None", indent.dash_end()).as_str());
                }
                indent.decrease();
                result
            },
        }
    }
}

impl IndentDisplay for StructField {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str(format!("<struct field> {}\n", self.identifier).as_str());
        indent.increase();
        if let Some(access_modifier) = &self.access_modifier {
            result.push_str(format!("{}access_modifier: {}\n", indent.dash(), access_modifier.indent_display(indent)).as_str());
        } else {
            result.push_str(format!("{}access_modifier: None\n", indent.dash()).as_str());
        }
        result.push_str(format!("{}type_name: {}\n", indent.dash(), self.type_name).as_str());
        result.push_str(format!("{}mutable: {}", indent.dash_end(), self.mutable).as_str());
        indent.decrease();
        result
    }
}

impl IndentDisplay for UnionMember {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str(format!("<union member> {}", self.identifier).as_str());
        indent.increase();
        let mut i = 0;
        for field in &self.fields {
            if i == 0 {
                result.push_str(format!("\n{}{}", indent.dash(), field.indent_display(indent)).as_str());
            } else {
                if i < self.fields.len() - 1 {
                    result.push_str(format!("\n{}{},", indent.dash(), field.indent_display(indent)).as_str());
                } else {
                    result.push_str(format!("\n{}{}", indent.dash_end(), field.indent_display(indent)).as_str());
                }
            }

            i += 1;
        }
        indent.decrease();
        result
    }
}

impl IndentDisplay for UnionMemberField {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str("<union member field>\n");
        indent.increase();
        if let Some(identifier) = &self.identifier {
            result.push_str(format!("{}identifier: {}\n", indent.dash(), identifier).as_str());
        } else {
            result.push_str(format!("{}identifier: None\n", indent.dash()).as_str());
        }
        result.push_str(format!("{}type_name: {}", indent.dash_end(), self.type_name).as_str());
        indent.decrease();
        result
    }
}

impl IndentDisplay for AccessModifier {
    fn indent_display(&self, _indent: &mut Indent) -> String {
        match self {
            AccessModifier::Public => "public".to_string(),
            AccessModifier::Internal => "internal".to_string(),
        }
    }
}

impl IndentDisplay for FieldInitializer {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str("<field initializer>\n");
        if let Some(identifier) = &self.identifier {
            result.push_str(format!("{}field initializer: {}\n", indent.dash(), identifier).as_str());
        } else {
            result.push_str(format!("{}field initializer: None\n", indent.dash()).as_str());
        }
        result.push_str(format!("{}initializer: {}", indent.dash_end(), self.initializer.indent_display(indent)).as_str());
        result
    }
}

impl IndentDisplay for UnaryOperator {
    fn indent_display(&self, _indent: &mut Indent) -> String {
        match self {
            UnaryOperator::Negate => "-".to_string(),
            UnaryOperator::LogicalNot => "!".to_string(),
            UnaryOperator::BitwiseNot => "~".to_string(),
        }
    }
}

impl IndentDisplay for BinaryOperator {
    fn indent_display(&self, _indent: &mut Indent) -> String {
        match self {
            BinaryOperator::Add => "+".to_string(),
            BinaryOperator::Subtract => "-".to_string(),
            BinaryOperator::Multiply => "*".to_string(),
            BinaryOperator::Divide => "/".to_string(),
            BinaryOperator::Modulo => "%".to_string(),
            BinaryOperator::BitwiseAnd => "&".to_string(),
            BinaryOperator::BitwiseOr => "|".to_string(),
            BinaryOperator::BitwiseXor => "^".to_string(),
            BinaryOperator::BitwiseLeftShift => "<<".to_string(),
            BinaryOperator::BitwiseRightShift => ">>".to_string(),
            BinaryOperator::BooleanLogicalAnd => "&&".to_string(),
            BinaryOperator::BooleanLogicalOr => "||".to_string(),
            BinaryOperator::Equal => "==".to_string(),
            BinaryOperator::NotEqual => "!=".to_string(),
            BinaryOperator::LessThan => "<".to_string(),
            BinaryOperator::LessThanOrEqual => "<=".to_string(),
            BinaryOperator::GreaterThan => ">".to_string(),
            BinaryOperator::GreaterThanOrEqual => ">=".to_string(),
        }
    }
}

impl IndentDisplay for Parameter {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str("<parameter>\n");
        result.push_str(format!("{}parameter: {}\n", indent.dash(), self.identifier).as_str());
        result.push_str(format!("{}type_name: {}", indent.dash_end(), self.type_name).as_str());
        result
    }
}

impl IndentDisplay for ConditionBlock {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str("<condition block>\n");
        indent.increase();
        result.push_str(format!("{}condition: {}\n", indent.dash(), self.condition.indent_display(indent)).as_str());
        result.push_str(format!("{}body: {}", indent.dash_end(), self.block.indent_display(indent)).as_str());
        indent.decrease();
        result
    }
}

impl IndentDisplay for TypedStatement {
    fn indent_display(&self, indent: &mut Indent) -> String {
        match self {
            TypedStatement::None => String::new(),
            TypedStatement::Program {
                statements
            } => {
                let mut result = String::new();
                for statement in statements {
                    result.push_str(&statement.indent_display(indent));
                }
                result
            },
            TypedStatement::StructDeclaration {
                type_name,
                fields,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<struct declaration> {}: {}", type_name, type_.to_string()).as_str());
                indent.increase();
                for field in fields {
                    result.push_str(format!("\n{}{}", indent.dash_end(), field.indent_display(indent)).as_str());
                }
                indent.decrease();
                result
            },
            TypedStatement::UnionDeclaration {
                type_name,
                members,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<union declaration> {}: {}", type_name, type_.to_string()).as_str());
                indent.increase();
                for member in members {
                    result.push_str(format!("\n{}{}", indent.dash_end(), member.indent_display(indent)).as_str());
                }
                indent.decrease();
                result
            },
            TypedStatement::FunctionDeclaration {
                identifier,
                parameters,
                return_type,
                body,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<function declaration> {}: {}\n", identifier, type_.to_string()).as_str());
                indent.increase();
                for parameter in parameters {
                    result.push_str(format!("{}{}\n", indent.dash(), parameter.indent_display(indent)).as_str());
                }
                result.push_str(format!("{}return_type: {}\n", indent.dash(), return_type).as_str());
                result.push_str(format!("{}body: {}", indent.dash_end(), body.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            TypedStatement::Expression(e) => e.indent_display(indent),
        }
    }
}

impl IndentDisplay for TypedExpression {
    fn indent_display(&self, indent: &mut Indent) -> String {
        match self {
            TypedExpression::None => {
                String::new()
            },
            TypedExpression::VariableDeclaration {
                mutable,
                identifier,
                initializer,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<variable declaration> {}: {}\n", identifier, type_.to_string()).as_str());
                indent.increase();
                result.push_str(format!("{}mutable: {}\n", indent.dash(), mutable).as_str());
                if let Some(initializer) = initializer {
                    result.push_str(format!("{}initializer: {}", indent.dash_end(), initializer.indent_display(indent)).as_str());
                } else {
                    result.push_str(format!("{}initializer: None", indent.dash_end()).as_str());
                }
                indent.decrease();
                result
            },
            TypedExpression::If {
                r#if,
                else_ifs,
                r#else,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<if>: {}\n", type_.to_string()).as_str());
                indent.increase();
                result.push_str(format!("{}condition:{}", indent.dash(), r#if.condition.indent_display(indent)).as_str());
                if let Some(else_ifs) = else_ifs {
                    for else_if in else_ifs {
                        result.push_str(format!("\n{}{}\n", indent.dash(), else_if.indent_display(indent)).as_str());
                        result.push_str(format!("\n{}condition: {}\n", indent.dash(), else_if.condition.indent_display(indent)).as_str());
                    }
                } else {
                    result.push_str(format!("\n{}else_ifs: None\n", indent.dash()).as_str());
                }
                if let Some(r#else) = r#else {
                    result.push_str(format!("{}else block: {}", indent.dash_end(), r#else.indent_display(indent)).as_str());
                } else {
                    result.push_str(format!("{}else block: None", indent.dash_end()).as_str());
                }
                indent.decrease();
                result
            },
            TypedExpression::Assignment {
                member,
                initializer,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<assignment>: {}\n", type_.to_string()).as_str());
                indent.increase();
                result.push_str(format!("{}member: {}\n", indent.dash(), member.indent_display(indent)).as_str());
                result.push_str(format!("{}initializer: {}", indent.dash_end(), initializer.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            TypedExpression::Member(m) => m.indent_display(indent),
            TypedExpression::Literal(l) => l.indent_display(indent),
            TypedExpression::Call {
                caller,
                arguments,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<call>: {}\n", type_.to_string()).as_str());
                indent.increase();
                result.push_str(format!("{}caller: {}", indent.dash(), caller.indent_display(indent)).as_str());
                let mut i = 0;
                for argument in arguments {
                    if i < arguments.len() - 1 {
                        result.push_str(format!("\n{}argument: {},", indent.dash(), argument.indent_display(indent)).as_str());
                    } else {
                        result.push_str(format!("\n{}argument: {}", indent.dash_end(), argument.indent_display(indent)).as_str());
                    }
                    i += 1;
                }
                indent.decrease();
                result
            },
            TypedExpression::Unary {
                operator,
                expression,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<unary>: {}\n", type_.to_string()).as_str());
                indent.increase();
                result.push_str(format!("{}operator: {}\n", indent.dash(), operator.indent_display(indent)).as_str());
                result.push_str(format!("{}expression: {}", indent.dash_end(), expression.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            TypedExpression::Binary {
                left,
                operator,
                right,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<binary>: {}\n", type_.to_string()).as_str());
                indent.increase();
                result.push_str(format!("{}left: {}\n", indent.dash(), left.indent_display(indent)).as_str());
                result.push_str(format!("{}operator: {}\n", indent.dash(), operator.indent_display(indent)).as_str());
                result.push_str(format!("{}right: {}", indent.dash_end(), right.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            TypedExpression::Ternary {
                condition,
                true_expression,
                false_expression,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<ternary>: {}\n", type_.to_string()).as_str());
                indent.increase();
                result.push_str(format!("{}condition: {}\n", indent.dash(), condition.indent_display(indent)).as_str());
                result.push_str(format!("{}true_expression: {}\n", indent.dash(), true_expression.indent_display(indent)).as_str());
                result.push_str(format!("{}false_expression: {}", indent.dash_end(), false_expression.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            TypedExpression::Block {
                statements,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<block>: {}", type_.to_string()).as_str());
                indent.increase();
                let mut i = 0;
                for statement in statements {
                    if i < statements.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), statement.indent_display(indent)).as_str());
                    } else {
                        result.push_str(format!("\n{}{}", indent.dash_end(), statement.indent_display(indent)).as_str());
                    }
                    i += 1;
                }
                indent.decrease();
                result
            },
            TypedExpression::Drop {
                identifier,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<drop> {}: {}", identifier, type_.to_string()).as_str());
                result
            },
        }
    }
}

impl IndentDisplay for type_checker::ast::Member {
    fn indent_display(&self, indent: &mut Indent) -> String {
        match self {
            type_checker::ast::Member::Identifier {
                symbol,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<identifier> {}: {}", symbol, type_.to_string()).as_str());
                result
            },
            type_checker::ast::Member::MemberAccess {
                object,
                member,
                symbol,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<member access>: {}", type_.to_string()).as_str());
                indent.increase();
                result.push_str(format!("{}object: {}\n", indent.dash(), object.indent_display(indent)).as_str());
                result.push_str(format!("{}member: {}\n", indent.dash(), member.indent_display(indent)).as_str());
                result.push_str(format!("{}symbol: {}", indent.dash_end(), symbol).as_str());
                indent.decrease();
                result
            },
        }
    }
}

impl IndentDisplay for type_checker::ast::Literal {
    fn indent_display(&self, indent: &mut Indent) -> String {
        match self {
            type_checker::ast::Literal::I8(v) => v.to_string(),
            type_checker::ast::Literal::I16(v) => v.to_string(),
            type_checker::ast::Literal::I32(v) => v.to_string(),
            type_checker::ast::Literal::I64(v) => v.to_string(),
            type_checker::ast::Literal::I128(v) => v.to_string(),
            type_checker::ast::Literal::U8(v) => v.to_string(),
            type_checker::ast::Literal::U16(v) => v.to_string(),
            type_checker::ast::Literal::U32(v) => v.to_string(),
            type_checker::ast::Literal::U64(v) => v.to_string(),
            type_checker::ast::Literal::U128(v) => v.to_string(),
            type_checker::ast::Literal::F32(v) => v.to_string(),
            type_checker::ast::Literal::F64(v) => v.to_string(),
            type_checker::ast::Literal::String(s) => s.to_string(),
            type_checker::ast::Literal::Char(c) => c.to_string(),
            type_checker::ast::Literal::Bool(b) => b.to_string(),
            type_checker::ast::Literal::Struct {
                type_name,
                field_initializers,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<struct literal>: {}\n", type_.to_string()).as_str());
                indent.increase();
                result.push_str(format!("{}type_name: {}", indent.dash(), type_name).as_str());
                if let Some(field_initializers) = field_initializers {
                    let mut i = 0;
                    for field in field_initializers {
                        if i < field_initializers.len() - 1 {
                            result.push_str(format!("\n{}{},", indent.dash(), field.indent_display(indent)).as_str());
                        } else {
                            result.push_str(format!("\n{}{}", indent.dash_end(), field.indent_display(indent)).as_str());
                        }
                        i += 1;
                    }
                } else {
                    result.push_str(format!("\n{}field_initializers: None", indent.dash_end()).as_str());
                }
                indent.decrease();
                result
            },
            type_checker::ast::Literal::Union {
                type_name,
                member,
                field_initializers,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<union literal>: {}\n", type_.to_string()).as_str());
                indent.increase();
                result.push_str(format!("{}type_name: {}\n", indent.dash(), type_name).as_str());
                result.push_str(format!("{}member: {}", indent.dash(), member).as_str());
                if let Some(field_initializers) = field_initializers {
                    let mut i = 0;
                    for field in field_initializers {
                        if i < field_initializers.len() - 1 {
                            result.push_str(format!("\n{}{},", indent.dash(), field.indent_display(indent)).as_str());
                        } else {
                            result.push_str(format!("\n{}{}", indent.dash_end(), field.indent_display(indent)).as_str());
                        }
                        i += 1;
                    }
                } else {
                    result.push_str(format!("\n{}field_initializers: None", indent.dash_end()).as_str());
                }
                indent.decrease();
                result
            },
        }
    }
}

impl IndentDisplay for type_checker::ast::StructField {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str(format!("<struct field> {}: {}\n", self.identifier, self.type_.to_string()).as_str());
        indent.increase();
        result.push_str(format!("{}mutable: {}", indent.dash(), self.mutable).as_str());
        indent.decrease();
        result
    }
}

impl IndentDisplay for type_checker::ast::Parameter {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str("<parameter>\n");
        result.push_str(format!("{}parameter: {}\n", indent.dash(), self.identifier).as_str());
        result.push_str(format!("{}type_name: {}", indent.dash_end(), self.type_.to_string()).as_str());
        result
    }
}

impl IndentDisplay for type_checker::ast::UnionMember {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str(format!("<union member> {}: {}", self.discriminant_name, self.type_.to_string()).as_str());
        indent.increase();
        let mut i = 0;
        for field in &self.fields {
            if i == 0 {
                result.push_str(format!("\n{}{}", indent.dash(), field.indent_display(indent)).as_str());
            } else {
                if i < self.fields.len() - 1 {
                    result.push_str(format!("\n{}{},", indent.dash(), field.indent_display(indent)).as_str());
                } else {
                    result.push_str(format!("\n{}{}", indent.dash_end(), field.indent_display(indent)).as_str());
                }
            }

            i += 1;
        }
        indent.decrease();
        result
    }
}

impl IndentDisplay for type_checker::ast::UnionMemberField {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str(format!("<union member field>: {}\n", self.type_.to_string()).as_str());
        indent.increase();
        if let Some(identifier) = &self.identifier {
            result.push_str(format!("{}identifier: {}\n", indent.dash(), identifier).as_str());
        } else {
            result.push_str(format!("{}identifier: None\n", indent.dash()).as_str());
        }
        result.push_str(format!("{}type_name: {}", indent.dash_end(), self.type_.to_string()).as_str());
        indent.decrease();
        result
    }
}

impl IndentDisplay for type_checker::ast::ConditionBlock {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str("<condition block>\n");
        indent.increase();
        result.push_str(format!("{}condition: {}\n", indent.dash(), self.condition.indent_display(indent)).as_str());
        result.push_str(format!("{}body: {}", indent.dash_end(), self.block.indent_display(indent)).as_str());
        indent.decrease();
        result
    }
}

impl IndentDisplay for type_checker::ast::UnaryOperator {
    fn indent_display(&self, _indent: &mut Indent) -> String {
        match self {
            type_checker::ast::UnaryOperator::Identity => "+".to_string(),
            type_checker::ast::UnaryOperator::Negate => "-".to_string(),
            type_checker::ast::UnaryOperator::LogicalNot => "!".to_string(),
            type_checker::ast::UnaryOperator::BitwiseNot => "~".to_string(),
        }
    }
}

impl IndentDisplay for type_checker::ast::BinaryOperator {
    fn indent_display(&self, _indent: &mut Indent) -> String {
        match self {
            type_checker::ast::BinaryOperator::Add => "+".to_string(),
            type_checker::ast::BinaryOperator::Subtract => "-".to_string(),
            type_checker::ast::BinaryOperator::Multiply => "*".to_string(),
            type_checker::ast::BinaryOperator::Divide => "/".to_string(),
            type_checker::ast::BinaryOperator::Modulo => "%".to_string(),
            type_checker::ast::BinaryOperator::BitwiseAnd => "&".to_string(),
            type_checker::ast::BinaryOperator::BitwiseOr => "|".to_string(),
            type_checker::ast::BinaryOperator::BitwiseXor => "^".to_string(),
            type_checker::ast::BinaryOperator::BitwiseLeftShift => "<<".to_string(),
            type_checker::ast::BinaryOperator::BitwiseRightShift => ">>".to_string(),
            type_checker::ast::BinaryOperator::BooleanLogicalAnd => "&&".to_string(),
            type_checker::ast::BinaryOperator::BooleanLogicalOr => "||".to_string(),
            type_checker::ast::BinaryOperator::Equal => "==".to_string(),
            type_checker::ast::BinaryOperator::NotEqual => "!=".to_string(),
            type_checker::ast::BinaryOperator::LessThan => "<".to_string(),
            type_checker::ast::BinaryOperator::LessThanOrEqual => "<=".to_string(),
            type_checker::ast::BinaryOperator::GreaterThan => ">".to_string(),
            type_checker::ast::BinaryOperator::GreaterThanOrEqual => ">=".to_string(),
        }
    }
}

impl IndentDisplay for type_checker::ast::FieldInitializer {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str("<field initializer>\n");
        if let Some(identifier) = &self.identifier {
            result.push_str(format!("{}field initializer: {}\n", indent.dash(), identifier).as_str());
        } else {
            result.push_str(format!("{}field initializer: None\n", indent.dash()).as_str());
        }
        result.push_str(format!("{}initializer: {}", indent.dash_end(), self.initializer.indent_display(indent)).as_str());
        result
    }
}