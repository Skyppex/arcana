use crate::{parser::{
    AccessModifier, Assignment, Binary, BinaryOperator, Call, ConditionBlock, Expression, FieldInitializer, FlagsMember, FunctionDeclaration, If, Index, Literal, Member, Parameter, Statement, StructDeclaration, StructField, Ternary, Unary, UnaryOperator, UnionDeclaration, UnionMember, UnionMemberField, UnionMemberFieldInitializers, VariableDeclaration, While
}, type_checker::{self, ast::{
    Block, TypedExpression, TypedStatement
}, Type}, types::{GenericType, TypeAnnotation, TypeName}};

pub struct Indent {
    levels: Vec<bool>,
}

impl Indent {
    pub fn new() -> Indent {
        Indent {
            levels: vec![],
        }
    }

    fn increase(&mut self) {
        self.levels.push(false);
    }

    fn increase_leaf(&mut self) {
        self.levels.push(true);
    }

    fn decrease(&mut self) {
        self.levels.pop();
    }

    fn end_current(&mut self) {
        let len = self.levels.len();
        if len == 0 {
            return;
        }
        self.levels[len - 1] = true;
    }

    fn dash(&self) -> String {
        let mut result = String::new();
        for is_end in self.levels.iter().rev().skip(1).rev() {
            result.push_str(if *is_end { "  " } else { "┆ " });
        }
        result.push_str("├─");
        result
    }

    fn dash_end(&self) -> String {
        let mut result = String::new();
        for is_end in self.levels.iter().rev().skip(1).rev() {
            result.push_str(if *is_end { "  " } else { "┆ " });
        }

        self.levels.last().map(|is_end| {
            result.push_str(if *is_end { "╰─" } else { "├─" });
        });

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

                for (i, statement) in statements.iter().enumerate() {
                    result.push_str(&statement.indent_display(indent));
                    if i < statements.len() - 1 {
                        result.push_str("\n\n");
                    }
                }
                
                result
            },
            Statement::StructDeclaration(StructDeclaration {
                access_modifier,
                type_name,
                fields 
            }) => {
                let mut result = String::new();
                result.push_str("<struct declaration>\n");
                indent.increase();

                result.push_str(format!("{}type_name: {}\n", indent.dash(), type_name.indent_display(indent)).as_str());
                result.push_str(format!("{}access_modifier: {}", indent.dash(), access_modifier.indent_display(indent)).as_str());

                for (i, field) in fields.iter().enumerate() {
                    let is_end = i == fields.len() - 1;
                    indent.end_current();
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
                result.push_str("<union declaration>\n");
                indent.increase();

                result.push_str(format!("{}type_name: {}\n", indent.dash(), type_name.indent_display(indent)).as_str());
                result.push_str(format!("{}access_modifier: {}", indent.dash(), access_modifier.indent_display(indent)).as_str());

                for (i, member) in members.iter().enumerate() {
                    if i < members.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), member.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}", indent.dash_end(), member.indent_display(indent)).as_str());
                    }                    
                }

                indent.decrease();
                result
            },
            // Statement::FlagsDeclaration(FlagsDeclaration {
            //     access_modifier,
            //     type_name,
            //     members
            // }) => {
            //     let mut result = String::new();
            //     result.push_str(format!("<flags declaration> {}\n", type_name).as_str());
            //     indent.increase();
            //     result.push_str(format!("{}access_modifier: {}\n", indent.dash(), access_modifier.indent_display(indent)).as_str());
                
            //     for (i, member) in members.iter().enumerate() {
            //         let is_end = i == members.len() - 1;
            //         indent.current(is_end);
            //         result.push_str(format!("\n{}{}", indent.dash_end(), member.indent_display(indent)).as_str());
            //     }

            //     indent.decrease();
            //     result
            // },
            Statement::Impl(impl_) => {
                let mut result = String::new();
                result.push_str("<impl>\n");
                indent.increase();
                result.push_str(format!("{}type_annotation: {}", indent.dash(), impl_.type_annotation.indent_display(indent)).as_str());

                for (i, function) in impl_.functions.iter().enumerate() {
                    let is_end = i == impl_.functions.len() - 1;
                    indent.end_current();
                    result.push_str(format!("\n{}{}", indent.dash_end(), function.indent_display(indent)).as_str());
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
                result.push_str("<function declaration>\n");
                indent.increase();

                result.push_str(format!("{}identifier: {}\n", indent.dash(), identifier).as_str());
                result.push_str(format!("{}access_modifier: {}", indent.dash(), access_modifier.indent_display(indent)).as_str());

                for parameter in parameters {
                    result.push_str(format!("\n{}{}", indent.dash(), parameter.indent_display(indent)).as_str());
                }

                result.push_str(format!("\n{}return_type: {}\n", indent.dash(), return_type.indent_display(indent)).as_str());
                
                indent.end_current();
                result.push_str(format!("{}body: <block>", indent.dash_end()).as_str());
                indent.increase();

                for (i, statement) in body.iter().enumerate() {
                    if i < body.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), statement.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}", indent.dash_end(), statement.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                indent.decrease();
                result
            },
            Statement::Semi(statement) => {
                let mut result = String::new();
                result.push_str("<semi>\n");
                indent.increase();
                indent.end_current();
                result.push_str(format!("{}statement: {}", indent.dash_end(), statement.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            Statement::Break(e) => {
                let mut result = String::new();
                result.push_str("<break>\n");
                indent.increase();
                indent.end_current();
                result.push_str(format!("{}expression: {}", indent.dash_end(), e.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            Statement::Continue => {
                let mut result = String::new();
                result.push_str("<continue>");
                result
            },
            Statement::Return(e) => {
                let mut result = String::new();
                result.push_str("<return>\n");
                indent.increase();
                indent.end_current();
                result.push_str(format!("{}expression: {}", indent.dash_end(), e.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            Statement::Expression(e) => e.indent_display(indent),
            Statement::Print(e) => e.indent_display(indent),
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
                type_annotation,
                identifier,
                initializer
            }) => {
                let mut result = String::new();
                result.push_str(format!("<variable declaration> {}\n", identifier).as_str());
                indent.increase();
                result.push_str(format!("{}mutable: {}\n", indent.dash(), mutable).as_str());
                result.push_str(format!("{}type_annotation: {}\n", indent.dash(), type_annotation.indent_display(indent)).as_str());
                indent.end_current();
                result.push_str(format!("{}initializer: {}", indent.dash_end(), initializer.indent_display(indent)).as_str());
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
                result.push_str(format!("\n{}body: {}", indent.dash(), r#if.block.indent_display(indent)).as_str());
                
                if let Some(else_ifs) = else_ifs {
                    for (i, else_if) in else_ifs.iter().enumerate() {
                        let is_end = i == else_ifs.len() - 1;
                        indent.end_current();
                        result.push_str(format!("\n{}{}\n", indent.dash(), else_if.indent_display(indent)).as_str());
                        result.push_str(format!("\n{}condition: {}\n", indent.dash(), else_if.condition.indent_display(indent)).as_str());
                    }
                } else {
                    result.push_str(format!("\n{}else_ifs: None\n", indent.dash()).as_str());
                }

                indent.end_current();
                result.push_str(format!("{}else block: {}", indent.dash_end(), r#else.indent_display(indent)).as_str());
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
                indent.end_current();
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
                
                for (i, argument) in arguments.iter().enumerate() {
                    if i < arguments.len() - 1 {
                        result.push_str(format!("\n{}argument: {},", indent.dash(), argument.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}argument: {}", indent.dash_end(), argument.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                result
            },
            Expression::Index(Index {
                caller,
                index
            }) => {
                let mut result = String::new();
                result.push_str("<index>\n");
                indent.increase_leaf();
                result.push_str(format!("{}caller: {}\n", indent.dash(), caller.indent_display(indent)).as_str());
                result.push_str(format!("{}index: {}", indent.dash_end(), index.indent_display(indent)).as_str());
                indent.decrease();
                result
            }
            Expression::Unary(Unary {
                operator,
                expression
            }) => {
                let mut result = String::new();
                result.push_str("<unary>\n");
                indent.increase();
                result.push_str(format!("{}operator: {}\n", indent.dash(), operator.indent_display(indent)).as_str());
                indent.end_current();
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
                indent.end_current();
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
                indent.end_current();
                result.push_str(format!("{}false_expression: {}", indent.dash_end(), false_expression.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            Expression::Block(statements) => {
                let mut result = String::new();
                result.push_str("<block>");
                indent.increase();
                
                for (i, statement) in statements.iter().enumerate() {
                    if i < statements.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), statement.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}", indent.dash_end(), statement.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                result
            },
            Expression::Drop(identifier) => {
                let mut result = String::new();
                result.push_str(format!("<drop> {}", identifier).as_str());
                result
            },
            Expression::Loop(statements) => {
                let mut result = String::new();
                result.push_str("<loop>\n");
                indent.increase();
                indent.end_current();
                result.push_str(format!("{}<block>", indent.dash_end()).as_str());
                indent.increase();
                indent.end_current();
                
                for (i, statement) in statements.iter().enumerate() {
                    if i < statements.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), statement.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}", indent.dash_end(), statement.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                indent.decrease();
                result
            },
            Expression::While(While {
                condition,
                statements,
                else_statements
            }) => {
                let mut result = String::new();
                result.push_str("<while>\n");
                indent.increase();
                result.push_str(format!("{}condition: {}\n", indent.dash(), condition.indent_display(indent)).as_str());
                indent.end_current();
                result.push_str(format!("{}<block>", indent.dash()).as_str());
                indent.increase();
                
                for (i, statement) in statements.iter().enumerate() {
                    if i < statements.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), statement.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}", indent.dash_end(), statement.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                
                if let Some(else_statements) = else_statements {
                    result.push_str(format!("\n{}else: <block>", indent.dash()).as_str());
                    indent.increase();
                    
                    for (i, statement) in else_statements.iter().enumerate() {
                        if i < else_statements.len() - 1 {
                            result.push_str(format!("\n{}{},", indent.dash(), statement.indent_display(indent)).as_str());
                        } else {
                            indent.end_current();
                            result.push_str(format!("\n{}{}", indent.dash_end(), statement.indent_display(indent)).as_str());
                        }
                    }

                    indent.decrease();
                } else {
                    result.push_str(format!("\n{}else: <block> None", indent.dash_end()).as_str());
                }

                indent.decrease();
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
                result.push_str("<member access>\n");
                indent.increase();
                result.push_str(format!("{}object: {}\n", indent.dash(), object.indent_display(indent)).as_str());
                result.push_str(format!("{}member: {}\n", indent.dash(), member.indent_display(indent)).as_str());
                indent.end_current();
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
            Literal::Unit => "unit".to_string(),
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
            Literal::Array(expressions) => {
                let mut result = String::new();
                result.push_str("<array>");
                indent.increase();

                for (i, expression) in expressions.iter().enumerate() {
                    if i < expressions.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), expression.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}", indent.dash_end(), expression.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                result
            },
            Literal::Struct {
                type_name,
                field_initializers
            } => {
                let mut result = String::new();
                result.push_str("<struct literal>\n");
                indent.increase();
                result.push_str(format!("{}type_name: {}", indent.dash(), type_name.indent_display(indent)).as_str());
                
                for (i, field) in field_initializers.iter().enumerate() {
                    if i < field_initializers.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), field.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}", indent.dash_end(), field.indent_display(indent)).as_str());
                    }
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
                indent.increase_leaf();
                result.push_str(format!("{}type_name: {}\n", indent.dash(), type_name.indent_display(indent)).as_str());
                result.push_str(format!("{}member: {}\n", indent.dash_end(), member).as_str());
                indent.increase_leaf();
                result.push_str(format!("{}{}", indent.dash_end(), field_initializers.indent_display(indent)).as_str());
                indent.decrease();
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

        result.push_str(format!("{}type_annotation: {}\n", indent.dash(), self.type_annotation.indent_display(indent)).as_str());
        indent.end_current();
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
        
        for (i, field) in self.fields.iter().enumerate() {
            if i < self.fields.len() - 1 {
                result.push_str(format!("\n{}{},", indent.dash(), field.indent_display(indent)).as_str());
            } else {
                indent.end_current();
                result.push_str(format!("\n{}{}", indent.dash_end(), field.indent_display(indent)).as_str());
            }
        }

        indent.decrease();
        result
    }
}

impl IndentDisplay for UnionMemberField {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str("<union member field>\n");
        indent.increase_leaf();
        result.push_str(format!("{}identifier: {}\n", indent.dash(), &self.identifier).as_str());
        result.push_str(format!("{}type_annotation: {}", indent.dash_end(), self.type_annotation.indent_display(indent)).as_str());
        indent.decrease();
        result
    }
}

impl IndentDisplay for FlagsMember {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str("<flags member>\n");
        indent.increase_leaf();
        result.push_str(format!("{}identifier: {}\n", indent.dash(), &self.identifier).as_str());
        result.push_str(format!("{}value: {}", indent.dash_end(), self.value).as_str());
        indent.decrease();
        result
    }
}

impl IndentDisplay for AccessModifier {
    fn indent_display(&self, _indent: &mut Indent) -> String {
        match self {
            AccessModifier::Public => "public".to_string(),
            AccessModifier::Internal => "internal".to_string(),
            AccessModifier::Super => "super".to_string(),
        }
    }
}

impl IndentDisplay for FieldInitializer {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str("<field initializer>\n");
        indent.increase();
        
        if let Some(identifier) = &self.identifier {
            result.push_str(format!("{}field initializer: {}\n", indent.dash(), identifier).as_str());
        } else {
            result.push_str(format!("{}field initializer: None\n", indent.dash()).as_str());
        }

        indent.end_current();
        result.push_str(format!("{}initializer: {}", indent.dash_end(), self.initializer.indent_display(indent)).as_str());
        indent.decrease();
        result
    }
}

impl IndentDisplay for UnionMemberFieldInitializers {
    fn indent_display(&self, indent: &mut Indent) -> String {
        match self {
            UnionMemberFieldInitializers::None => "".to_string(),
            UnionMemberFieldInitializers::Named(field_initializers) => {
                let mut result = String::new();
                result.push_str("<named field initializers>");
                indent.increase();
                
                for (i, (identifier, initializer)) in field_initializers.iter().enumerate() {
                    if i < field_initializers.len() - 1 {
                        result.push_str(format!("\n{}{}: {},", indent.dash(), identifier, initializer.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}: {}", indent.dash_end(), identifier, initializer.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                result
            },
            UnionMemberFieldInitializers::Unnamed(field_initializers) => {
                let mut result = String::new();
                result.push_str("<unnamed field initializers>");
                indent.increase();
                
                for (i, initializer) in field_initializers.iter().enumerate() {
                    if i < field_initializers.len() - 1 {
                        result.push_str(format!("\n{}f{}: {},", indent.dash(), i, initializer.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}f{}: {}", indent.dash_end(), i, initializer.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                result
            },
        }
    }
}

impl IndentDisplay for UnaryOperator {
    fn indent_display(&self, _indent: &mut Indent) -> String {
        match self {
            UnaryOperator::Identity => "+".to_string(),
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
        indent.increase_leaf();
        result.push_str(format!("{}parameter: {}\n", indent.dash(), self.identifier).as_str());
        result.push_str(format!("{}type_annotation: {}", indent.dash_end(), self.type_annotation.indent_display(indent)).as_str());
        indent.decrease();
        result
    }
}

impl IndentDisplay for ConditionBlock {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str("<condition block>\n");
        indent.increase();
        result.push_str(format!("{}condition: {}\n", indent.dash(), self.condition.indent_display(indent)).as_str());
        indent.end_current();
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
                
                for (i, statement) in statements.iter().enumerate() {
                    result.push_str(&statement.indent_display(indent));
                    if i < statements.len() - 1 {
                        result.push_str("\n\n");
                    }
                }

                result
            },
            TypedStatement::StructDeclaration {
                type_name,
                fields,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<struct declaration> {}\n", type_).as_str());
                indent.increase();

                result.push_str(format!("{}type_name: {}", indent.dash(), type_name.indent_display(indent)).as_str());

                for (i, field) in fields.iter().enumerate() {
                    let is_end = i == fields.len() - 1;
                    indent.end_current();
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
                result.push_str(format!("<union declaration> {}\n", type_).as_str());
                indent.increase();

                result.push_str(format!("{}type_name: {}", indent.dash(), type_name.indent_display(indent)).as_str());
                
                for (i, member) in members.iter().enumerate() {
                    if i < members.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), member.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}", indent.dash_end(), member.indent_display(indent)).as_str());
                    }
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
                result.push_str(format!("<function declaration> {}\n", type_).as_str());
                indent.increase();
                
                result.push_str(format!("{}identifier: {}", indent.dash(), identifier).as_str());

                for parameter in parameters {
                    result.push_str(format!("{}{}\n", indent.dash(), parameter.indent_display(indent)).as_str());
                }

                result.push_str(format!("{}return_type: {}\n", indent.dash(), return_type).as_str());
                
                indent.end_current();
                result.push_str(format!("{}body: <block>", indent.dash_end()).as_str());
                indent.increase();

                for (i, statement) in body.iter().enumerate() {
                    if i < body.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), statement.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}", indent.dash_end(), statement.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                indent.decrease();
                result
            },
            TypedStatement::Impl {
                type_annotation,
                functions
            } => {
                let mut result = String::new();
                result.push_str("<impl>\n");
                indent.increase();
                result.push_str(format!("{}type_annotation: {}", indent.dash(), type_annotation.indent_display(indent)).as_str());

                for (i, function) in functions.iter().enumerate() {
                    let is_end = i == functions.len() - 1;
                    indent.end_current();
                    result.push_str(format!("\n{}{}", indent.dash_end(), function.indent_display(indent)).as_str());
                }

                indent.decrease();
                result
            },
            TypedStatement::Semi(e) => {
                let mut result = String::new();
                result.push_str(format!("<semi>: {}\n", Type::Void).as_str());
                indent.increase();
                indent.end_current();
                result.push_str(format!("{}statement: {}", indent.dash_end(), e.indent_display(indent)).as_str());
                indent.decrease();
                result
            
            },
            TypedStatement::Break(e) => {
                let mut result = String::new();
                result.push_str(format!("<break>: {}\n", Type::Void).as_str());
                indent.increase();
                indent.end_current();
                result.push_str(format!("{}expression: {}", indent.dash_end(), e.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            TypedStatement::Continue => {
                let mut result = String::new();
                result.push_str(format!("<continue>: {}\n", Type::Void).as_str());
                result
            },
            TypedStatement::Return(e) => {
                let mut result = String::new();
                result.push_str(format!("<return>: {}\n", Type::Void).as_str());
                indent.increase();
                indent.end_current();
                result.push_str(format!("{}expression: {}", indent.dash_end(), e.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            TypedStatement::Expression(e) => e.indent_display(indent),
            TypedStatement::Print(e) => e.indent_display(indent),
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
                result.push_str(format!("<variable declaration> {}: {}\n", identifier, type_).as_str());
                indent.increase();
                result.push_str(format!("{}mutable: {}\n", indent.dash(), mutable).as_str());
                indent.end_current();
                
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
                result.push_str(format!("<if>: {}\n", type_).as_str());
                indent.increase();
                result.push_str(format!("{}condition:{}", indent.dash(), r#if.condition.indent_display(indent)).as_str());
                
                for else_if in else_ifs {
                    result.push_str(format!("\n{}{}\n", indent.dash(), else_if.indent_display(indent)).as_str());
                    result.push_str(format!("\n{}condition: {}", indent.dash(), else_if.condition.indent_display(indent)).as_str());
                }

                indent.end_current();
                
                if let Some(r#else) = r#else {
                    result.push_str(format!("\n{}else block: {}", indent.dash_end(), r#else.indent_display(indent)).as_str());
                } else {
                    result.push_str(format!("\n{}else block: None", indent.dash_end()).as_str());
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
                result.push_str(format!("<assignment>: {}\n", type_).as_str());
                indent.increase();
                result.push_str(format!("{}member: {}\n", indent.dash(), member.indent_display(indent)).as_str());
                indent.end_current();
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
                result.push_str(format!("<call>: {}\n", type_).as_str());
                indent.increase();
                result.push_str(format!("{}caller: {}", indent.dash(), caller.indent_display(indent)).as_str());
                
                for (i, argument) in arguments.iter().enumerate() {
                    if i < arguments.len() - 1 {
                        result.push_str(format!("\n{}argument: {},", indent.dash(), argument.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}argument: {}", indent.dash_end(), argument.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                result
            },
            TypedExpression::Index {
                caller,
                argument,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<index>: {}\n", type_).as_str());
                indent.increase_leaf();
                result.push_str(format!("{}caller: {}\n", indent.dash(), caller.indent_display(indent)).as_str());
                result.push_str(format!("{}index: {}", indent.dash_end(), argument.indent_display(indent)).as_str());
                indent.decrease();
                result
            }
            TypedExpression::Unary {
                operator,
                expression,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<unary>: {}\n", type_).as_str());
                indent.increase();
                result.push_str(format!("{}operator: {}\n", indent.dash(), operator.indent_display(indent)).as_str());
                indent.end_current();
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
                result.push_str(format!("<binary>: {}\n", type_).as_str());
                indent.increase();
                result.push_str(format!("{}left: {}\n", indent.dash(), left.indent_display(indent)).as_str());
                result.push_str(format!("{}operator: {}\n", indent.dash(), operator.indent_display(indent)).as_str());
                indent.end_current();
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
                result.push_str(format!("<ternary>: {}\n", type_).as_str());
                indent.increase();
                result.push_str(format!("{}condition: {}\n", indent.dash(), condition.indent_display(indent)).as_str());
                result.push_str(format!("{}true_expression: {}\n", indent.dash(), true_expression.indent_display(indent)).as_str());
                indent.end_current();
                result.push_str(format!("{}false_expression: {}", indent.dash_end(), false_expression.indent_display(indent)).as_str());
                indent.decrease();
                result
            },
            TypedExpression::Block(Block {
                statements,
                type_
            }) => {
                let mut result = String::new();
                result.push_str(format!("<block>: {}", type_).as_str());
                indent.increase();
                
                for (i, statement) in statements.iter().enumerate() {
                    if i < statements.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), statement.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}", indent.dash_end(), statement.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                result
            },
            TypedExpression::Drop {
                identifier,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<drop> {}: {}", identifier, type_).as_str());
                result
            },
            TypedExpression::Loop(Block {
                statements,
                type_
            }) => {
                let mut result = String::new();
                result.push_str(format!("<loop>: {}", type_).as_str());
                indent.increase();
                indent.end_current();
                
                for (i, statement) in statements.iter().enumerate() {
                    if i < statements.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), statement.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}", indent.dash_end(), statement.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                result
            },
            TypedExpression::While {
                condition,
                block,
                else_block,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<while>: {}\n", type_).as_str());
                indent.increase();
                result.push_str(format!("{}condition: {}\n", indent.dash(), condition.indent_display(indent)).as_str());
                indent.end_current();
                result.push_str(format!("{}<block>", indent.dash()).as_str());
                
                for (i, statement) in block.iter().enumerate() {
                    if i < block.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), statement.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}", indent.dash_end(), statement.indent_display(indent)).as_str());
                    }
                }

                if let Some(else_block) = else_block {
                    result.push_str(format!("\n{}else:", indent.dash()).as_str());
                    
                    for (i, statement) in else_block.iter().enumerate() {
                        if i < else_block.len() - 1 {
                            result.push_str(format!("\n{}{},", indent.dash(), statement.indent_display(indent)).as_str());
                        } else {
                            indent.end_current();
                            result.push_str(format!("\n{}{}", indent.dash_end(), statement.indent_display(indent)).as_str());
                        }
                    }
                } else {
                    result.push_str(format!("\n{}else: None", indent.dash_end()).as_str());
                }

                indent.decrease();
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
                result.push_str(format!("<identifier> {}: {}", symbol, type_).as_str());
                result
            },
            type_checker::ast::Member::MemberAccess {
                object,
                member,
                symbol,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<member access>: {}\n", type_).as_str());
                indent.increase();
                result.push_str(format!("{}object: {}\n", indent.dash(), object.indent_display(indent)).as_str());
                result.push_str(format!("{}member: {}\n", indent.dash(), member.indent_display(indent)).as_str());
                indent.end_current();
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
            type_checker::ast::Literal::Unit => "unit".to_string(),
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
            type_checker::ast::Literal::Array {
                values,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<array>: {}", type_).as_str());
                indent.increase();

                for (i, expression) in values.iter().enumerate() {
                    if i < values.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), expression.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}", indent.dash_end(), expression.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                result
            },
            type_checker::ast::Literal::Struct {
                type_name,
                field_initializers,
                type_
            } => {
                let mut result = String::new();
                result.push_str(format!("<struct literal>: {}\n", type_).as_str());
                indent.increase();
                result.push_str(format!("{}type_name: {}", indent.dash(), type_name.indent_display(indent)).as_str());

                for (i, field) in field_initializers.iter().enumerate() {
                    if i < field_initializers.len() - 1 {
                        result.push_str(format!("\n{}{},", indent.dash(), field.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}", indent.dash_end(), field.indent_display(indent)).as_str());
                    }
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
                result.push_str(format!("<union literal>: {}\n", type_).as_str());
                indent.increase_leaf();
                result.push_str(format!("{}type_name: {}\n", indent.dash(), type_name.indent_display(indent)).as_str());
                result.push_str(format!("{}member: {}\n", indent.dash_end(), member).as_str());
                indent.increase_leaf();
                result.push_str(format!("{}{}", indent.dash_end(), field_initializers.indent_display(indent)).as_str());
                indent.decrease();
                indent.decrease();
                result
            },
        }
    }
}

impl IndentDisplay for type_checker::ast::StructField {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str(format!("<struct field> {}: {}\n", self.identifier, self.type_).as_str());
        indent.increase_leaf();
        result.push_str(format!("{}mutable: {}", indent.dash_end(), self.mutable).as_str());
        indent.decrease();
        result
    }
}

impl IndentDisplay for type_checker::ast::Parameter {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str("<parameter>\n");
        result.push_str(format!("{}parameter: {}\n", indent.dash(), self.identifier).as_str());
        result.push_str(format!("{}type_annotation: {}", indent.dash_end(), self.type_annotation.indent_display(indent)).as_str());
        result
    }
}

impl IndentDisplay for type_checker::ast::UnionMember {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str(format!("<union member> {}: {}", self.discriminant_name, self.type_).as_str());
        indent.increase();
        
        for (i, field) in self.fields.iter().enumerate() {
            if i < self.fields.len() - 1 {
                result.push_str(format!("\n{}{},", indent.dash(), field.indent_display(indent)).as_str());
            } else {
                indent.end_current();
                result.push_str(format!("\n{}{}", indent.dash_end(), field.indent_display(indent)).as_str());
            }
        }

        indent.decrease();
        result
    }
}

impl IndentDisplay for type_checker::ast::UnionMemberField {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str(format!("<union member field>: {}\n", self.type_).as_str());
        indent.increase_leaf();
        result.push_str(format!("{}identifier: {}\n", indent.dash(), self.identifier).as_str());
        result.push_str(format!("{}type: {}", indent.dash_end(), self.type_).as_str());
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
        indent.end_current();
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
        indent.increase();
        
        if let Some(identifier) = &self.identifier {
            result.push_str(format!("{}field initializer: {}\n", indent.dash(), identifier).as_str());
        } else {
            result.push_str(format!("{}field initializer: None\n", indent.dash()).as_str());
        }

        indent.end_current();
        result.push_str(format!("{}initializer: {}", indent.dash_end(), self.initializer.indent_display(indent)).as_str());
        indent.decrease();
        result
    }
}

impl IndentDisplay for type_checker::ast::UnionMemberFieldInitializers {
    fn indent_display(&self, indent: &mut Indent) -> String {
        match self {
            type_checker::ast::UnionMemberFieldInitializers::None => "".to_string(),
            type_checker::ast::UnionMemberFieldInitializers::Named(field_initializers) => {
                let mut result = String::new();
                result.push_str("<named field initializer>");
                indent.increase();
                
                for (i, (identifier, initializer)) in field_initializers.iter().enumerate() {
                    if i < field_initializers.len() - 1 {
                        result.push_str(format!("\n{}{}: {},", indent.dash(), identifier, initializer.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}{}: {}", indent.dash_end(), identifier, initializer.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                result
            },
            type_checker::ast::UnionMemberFieldInitializers::Unnamed(field_initializers) => {
                let mut result = String::new();
                result.push_str("<unnamed field initializer>");
                indent.increase();
                
                for (i, initializer) in field_initializers.iter().enumerate() {
                    if i < field_initializers.len() - 1 {
                        result.push_str(format!("\n{}f{}: {},", indent.dash(), i, initializer.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}f{}: {}", indent.dash_end(), i, initializer.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                result
            },
        }
    }
}

impl IndentDisplay for TypeName {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str(format!("<type name>: {}\n", self.to_string()).as_str());

        match self {
            TypeName::Type(type_name) => {
                indent.increase_leaf();
                result.push_str(format!("{}type: {}", indent.dash_end(), type_name).as_str());
                indent.decrease();
                result
            },
            TypeName::GenericType(type_name, generics) => {
                indent.increase();
                result.push_str(format!("{}type: {}", indent.dash(), type_name).as_str());
                indent.end_current();
                result.push_str(format!("\n{}generics:", indent.dash_end()).as_str());
                indent.increase();

                for (i, generic) in generics.iter().enumerate() {
                    if i < generics.len() - 1 {
                        result.push_str(format!("\n{}generic: {},", indent.dash(), generic.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}generic: {}", indent.dash_end(), generic.indent_display(indent)).as_str());
                    }
                }

                indent.decrease();
                indent.decrease();
                result
            },
        }
    }
}

impl IndentDisplay for GenericType {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str("<generic type>\n");
        indent.increase_leaf();
        result.push_str(format!("{}type: {}", indent.dash_end(), self.type_name).as_str());
        indent.decrease();
        result
    }
}

impl IndentDisplay for TypeAnnotation {
    fn indent_display(&self, indent: &mut Indent) -> String {
        let mut result = String::new();
        result.push_str("<type annotation>\n");
        indent.increase_leaf();

        match self {
            TypeAnnotation::Type(type_name) => {
                result.push_str(format!("{}type: {}", indent.dash_end(), type_name).as_str());
            },
            TypeAnnotation::GenericType(type_name, generics) => {
                result.push_str(format!("{}generic_type: {}", indent.dash(), type_name).as_str());
                
                for (i, generic) in generics.iter().enumerate() {
                    if i < generics.len() - 1 {
                        result.push_str(format!("\n{}generic: {},", indent.dash(), generic.indent_display(indent)).as_str());
                    } else {
                        indent.end_current();
                        result.push_str(format!("\n{}generic: {}", indent.dash_end(), generic.indent_display(indent)).as_str());
                    }
                }
            },
            TypeAnnotation::Array(type_annotation) => {
                result.push_str(format!("{}slice_type: {}", indent.dash_end(), type_annotation.indent_display(indent)).as_str());
            },
        }

        indent.decrease();
        result
    }
}

impl<T: IndentDisplay> IndentDisplay for Option<T> {
    fn indent_display(&self, indent: &mut Indent) -> String {
        match self {
            Some(v) => v.indent_display(indent),
            None => "None".to_string(),
        }
    }
}

impl<T: IndentDisplay> IndentDisplay for Box<T> {
    fn indent_display(&self, indent: &mut Indent) -> String {
        self.as_ref().indent_display(indent)
    }
}

impl IndentDisplay for String {
    fn indent_display(&self, _indent: &mut Indent) -> String {
        self.clone()
    }
}