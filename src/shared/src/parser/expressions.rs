use std::collections::HashMap;

use crate::{lexer::token::{self, Keyword, TokenKind}, types::TypeAnnotation};

use super::{cursor::Cursor, statements::parse_statement, Assignment, Binary, BinaryOperator, Call, ConditionBlock, Expression, FieldInitializer, If, Index, Literal, Member, Statement, Ternary, Unary, UnaryOperator, EnumMemberFieldInitializers, VariableDeclaration, While};
use crate::types::{can_be_type_annotation, parse_type_annotation};

pub fn parse_expression(cursor: &mut Cursor) -> Result<Expression, String> {
    #[cfg(feature = "interpreter")]
    let expression = parse_drop(cursor);

    #[cfg(not(feature = "interpreter"))]
    let expression = parse_loop(cursor);

    return expression;
}

#[cfg(feature = "interpreter")]
fn parse_drop(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Drop) {
        return parse_loop(cursor);
    }

    cursor.bump()?; // Consume the drop

    let TokenKind::OpenParen = cursor.bump()?.kind else {
        return Err(format!("Expected ( but found {:?}", cursor.first().kind));
    };

    let TokenKind::Identifier(identifier) = cursor.bump()?.kind else {
        return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
    };

    let TokenKind::CloseParen = cursor.bump()?.kind else {
        return Err(format!("Expected ) but found {:?}", cursor.first().kind));
    };

    Ok(Expression::Drop(identifier))
}

pub fn parse_loop(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Loop) {
        return parse_while(cursor);
    }

    cursor.bump()?; // Consume the loop

    let block = parse_block_statements(cursor)?;

    Ok(Expression::Loop(block))
}

pub fn parse_while(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::While) {
        return parse_block(cursor);
    }

    cursor.bump()?; // Consume the while

    let condition = parse_expression(cursor)?;
    let block = parse_block_statements(cursor)?;

    if cursor.first().kind != TokenKind::Keyword(Keyword::Else) {
        return Ok(Expression::While(While {
            condition: Box::new(condition),
            statements: block,
            else_statements: None
        }));
    }

    cursor.bump()?; // Consume the else

    let else_block = parse_block_statements(cursor)?;

    Ok(Expression::While(While {
        condition: Box::new(condition),
        statements: block,
        else_statements: Some(else_block)
    }))
}

pub fn parse_block(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::OpenBrace {
        return parse_type_literal(cursor);
    }

    parse_block_statements(cursor)
        .map(|statements| Expression::Block(statements))
}

pub fn parse_block_statements(cursor: &mut Cursor) -> Result<Vec<Statement>, String> {
    if cursor.first().kind != TokenKind::OpenBrace {
        return Err(format!("Expected {{ but found {:?}", cursor.first().kind));
    }

    cursor.bump()?; // Consume the {
    
    let mut statements = vec![];

    while cursor.first().kind != TokenKind::CloseBrace {
        statements.push(parse_statement(cursor)?);
    }

    cursor.bump()?; // Consume the }

    Ok(statements)
}

fn parse_type_literal(cursor: &mut Cursor) -> Result<Expression, String> {
    let TokenKind::Identifier(_) = cursor.first().kind else {
        return parse_assignment(cursor);
    };

    if !matches!(cursor.second().kind, TokenKind::OpenBrace | TokenKind::DoubleColon) {
        return parse_assignment(cursor);
    };

    let type_annotation = parse_type_annotation(cursor, true)?;

    if cursor.first().kind == TokenKind::OpenBrace {
        parse_struct_literal(cursor, type_annotation)
    } else {
        parse_enum_literal(cursor, type_annotation)
    }
}

fn parse_struct_literal(cursor: &mut Cursor, type_annotation: TypeAnnotation) -> Result<Expression, String> {
    if cursor.bump()?.kind != TokenKind::OpenBrace {
        return Err(format!("Expected {{ but found {:?}", cursor.first().kind));
    }

    let mut field_initializers = vec![];
    let mut has_comma = true;

    while cursor.first().kind != TokenKind::CloseBrace {
        if !has_comma {
            return Err(format!("Expected , but found {:?}", cursor.first().kind));
        }

        has_comma = true;
        field_initializers.push(parse_field_initializer(cursor)?);

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        } else {
            has_comma = false;
        }
    }

    cursor.bump()?; // Consume the }
    Ok(Expression::Literal(Literal::Struct {
        type_annotation,
        field_initializers 
    }))
}

fn parse_field_initializer(cursor: &mut Cursor) -> Result<FieldInitializer, String> {
    let TokenKind::Identifier(identifier) = cursor.first().kind else {
        return Ok(FieldInitializer { identifier: None, initializer: parse_expression(cursor)? });
    };

    let TokenKind::Colon = cursor.second().kind else {
        return Ok(FieldInitializer { identifier: None, initializer: parse_expression(cursor)? });
    };

    cursor.bump()?; // Consume the identifier
    cursor.bump()?; // Consume the :

    let initializer = parse_expression(cursor)?;

    Ok(FieldInitializer { identifier: Some(identifier), initializer })
}

fn parse_enum_literal(cursor: &mut Cursor, type_annotation: TypeAnnotation) -> Result<Expression, String> {
    let TokenKind::DoubleColon = cursor.first().kind else {
        return Err(format!("Expected :: but found {:?}", cursor.first().kind));
    };

    cursor.bump()?; // Consume the ::
    
    let TokenKind::Identifier(member) = cursor.first().kind else {
        return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
    };

    cursor.bump()?; // Consume the identifier

    let field_initializers = {
        if cursor.first().kind == TokenKind::OpenParen {
            cursor.bump()?; // Consume the (
            if matches!(cursor.second().kind, TokenKind::Colon) {
                parse_named_enum_member_field_initializers(cursor)?
            } else {
                parse_unnamed_enum_member_field_initializers(cursor)?
            }
        } else {
            EnumMemberFieldInitializers::None
        }
    };

    cursor.bump()?; // Consume the )
    Ok(Expression::Literal(Literal::Enum {
        type_annotation,
        member,
        field_initializers
    }))
}

fn parse_unnamed_enum_member_field_initializers(cursor: &mut Cursor) -> Result<EnumMemberFieldInitializers, String> {
    let mut field_initializers = vec![];
    
    while cursor.first().kind != TokenKind::CloseParen {
        let initializer = parse_expression(cursor)?;

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        }

        field_initializers.push(initializer);
    }

    Ok(EnumMemberFieldInitializers::Unnamed(field_initializers))
}

fn parse_named_enum_member_field_initializers(cursor: &mut Cursor) -> Result<EnumMemberFieldInitializers, String> {
    let mut field_initializers = HashMap::new();
    
    while cursor.first().kind != TokenKind::CloseParen {
        let TokenKind::Identifier(identifier) = cursor.first().kind else {
            return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
        };

        let TokenKind::Colon = cursor.second().kind else {
            return Err(format!("Expected : but found {:?}", cursor.first().kind));
        };

        cursor.bump()?; // Consume the identifier
        cursor.bump()?; // Consume the :

        let initializer = parse_expression(cursor)?;

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        }

        field_initializers.insert(identifier, initializer);
    }

    Ok(EnumMemberFieldInitializers::Named(field_initializers))
}

fn parse_assignment(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut expression = parse_compound_assignment(cursor)?;

    while matches!(cursor.first().kind, TokenKind::Equal) {
        cursor.bump()?; // Consume the =
        let initializer = parse_expression(cursor)?;

        let Expression::Member(member) = expression else {
            return Err(format!("Expected member but found {:?}", expression));
        };

        expression = Expression::Assignment(Assignment {
            member: Box::new(member),
            initializer: Box::new(initializer),
        });
    }

    Ok(expression)
}

fn parse_compound_assignment(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut expression = parse_ternary(cursor)?;

    while matches!(cursor.first().kind,
        TokenKind::PlusEqual
        | TokenKind::MinusEqual
        | TokenKind::StarEqual
        | TokenKind::SlashEqual
        | TokenKind::PercentEqual
        | TokenKind::AmpersandEqual
        | TokenKind::PipeEqual
        | TokenKind::CaretEqual) {
        let operator = cursor.bump()?.kind; // Consume the +=, -=, *=, /=, %=, &=, |=, ^=, >>=, or <<=
        let initializer = parse_expression(cursor)?;

        let Expression::Member(ref member) = expression else {
            return Err(format!("Expected member but found {:?}", expression));
        };

        expression = Expression::Assignment(Assignment {
            member: Box::new(member.clone()),
            initializer: Box::new(Expression::Binary(Binary {
                left: Box::new(expression),
                right: Box::new(initializer),
                operator: match operator {
                    TokenKind::PlusEqual => BinaryOperator::Add,
                    TokenKind::MinusEqual => BinaryOperator::Subtract,
                    TokenKind::StarEqual => BinaryOperator::Multiply,
                    TokenKind::SlashEqual => BinaryOperator::Divide,
                    TokenKind::PercentEqual => BinaryOperator::Modulo,
                    TokenKind::AmpersandEqual => BinaryOperator::BitwiseAnd,
                    TokenKind::PipeEqual => BinaryOperator::BitwiseOr,
                    TokenKind::CaretEqual => BinaryOperator::BitwiseXor,
                    _ => unreachable!("Expected +=, -=, *=, /=, %=, &=, |=, or ^=, but found {:?}", operator),
                },
            })),
        });
    }

    Ok(expression)
}

fn parse_ternary(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut expression = parse_boolean_logical(cursor)?;

    while let TokenKind::QuestionMark = cursor.first().kind {
        cursor.bump()?; // Consume the ?

        let true_expression = parse_expression(cursor)?;

        expression = match cursor.first().kind {
            TokenKind::Colon => {
                cursor.bump()?; // Consume the :

                let false_expression = parse_expression(cursor)?;

                Expression::Ternary(Ternary {
                    condition: Box::new(expression),
                    true_expression: Box::new(true_expression),
                    false_expression: Box::new(false_expression)
                })
            },
            _ => return Err(format!("Expected : but found {:?}", cursor.first().kind)),
        };
    }
    
    Ok(expression)
}

fn parse_boolean_logical(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut expression = parse_comparison(cursor)?;

    while matches!(cursor.first().kind, TokenKind::DoubleAmpersand | TokenKind::DoublePipe) {
        let operator = cursor.bump()?.kind; // Consume the && or ||
        let right = parse_comparison(cursor)?;

        expression = Expression::Binary(Binary {
            left: Box::new(expression),
            right: Box::new(right),
            operator: match operator {
                TokenKind::DoubleAmpersand => BinaryOperator::BooleanLogicalAnd,
                TokenKind::DoublePipe => BinaryOperator::BooleanLogicalOr,
                _ => unreachable!("Expected && or || but found {:?}", operator),
            },
        });
    }

    Ok(expression)
}

fn parse_comparison(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut expression = parse_bitwise_logical(cursor)?;

    while matches!(cursor.first().kind, 
        TokenKind::DoubleEqual
        | TokenKind::BangEqual
        | TokenKind::Greater
        | TokenKind::Less
        | TokenKind::GreaterEqual
        | TokenKind::LessEqual) {
        let operator = cursor.bump()?.kind; // Consume the ==, !=, >, <, >=, or <=
        let right = parse_bitwise_logical(cursor)?;

        expression = Expression::Binary(Binary {
            left: Box::new(expression),
            right: Box::new(right),
            operator: match operator {
                TokenKind::DoubleEqual => BinaryOperator::Equal,
                TokenKind::BangEqual => BinaryOperator::NotEqual,
                TokenKind::Greater => BinaryOperator::GreaterThan,
                TokenKind::Less => BinaryOperator::LessThan,
                TokenKind::GreaterEqual => BinaryOperator::GreaterThanOrEqual,
                TokenKind::LessEqual => BinaryOperator::LessThanOrEqual,
                _ => unreachable!("Expected ==, !=, >, <, >=, or <= but found {:?}", operator),
            },
        });
    }

    Ok(expression)
}

fn parse_bitwise_logical(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut expression = parse_additive(cursor)?;

    while matches!(cursor.first().kind,
          TokenKind::Caret
        | TokenKind::Ampersand
        | TokenKind::Pipe) ||
        matches!((cursor.first().kind, cursor.second().kind),
          (TokenKind::Greater, TokenKind::Greater)
        | (TokenKind::Less   , TokenKind::Less   )) {
        let operator = cursor.bump()?.kind; // Consume the >>, <<, ^, &, or |
        let right = parse_additive(cursor)?;

        expression = Expression::Binary(Binary {
            left: Box::new(expression),
            right: Box::new(right),
            operator: match operator {
                TokenKind::Greater => {
                    cursor.bump()?; // Consume the >
                    BinaryOperator::BitwiseRightShift
                },
                TokenKind::Less => {
                    cursor.bump()?; // Consume the <
                    BinaryOperator::BitwiseLeftShift
                },
                TokenKind::Caret => BinaryOperator::BitwiseXor,
                TokenKind::Ampersand => BinaryOperator::BitwiseAnd,
                TokenKind::Pipe => BinaryOperator::BitwiseOr,
                _ => unreachable!("Expected >>, <<, ^, &, or | but found {:?}", operator),
            },
        });
    }
    
    Ok(expression)
}

fn parse_additive(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut expression = parse_multiplicative(cursor)?;

    while matches!(cursor.first().kind, TokenKind::Plus | TokenKind::Minus) {
        let operator = cursor.bump()?.kind; // Consume the + or -
        let right = parse_multiplicative(cursor)?;

        expression = Expression::Binary(Binary {
            left: Box::new(expression),
            right: Box::new(right),
            operator: match operator {
                TokenKind::Plus => BinaryOperator::Add,
                TokenKind::Minus => BinaryOperator::Subtract,
                _ => unreachable!("Expected + or - but found {:?}", operator),
            },
        });
    }
    
    Ok(expression)
}

fn parse_multiplicative(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut expression = parse_variable_declaration(cursor)?;

    while matches!(cursor.first().kind, TokenKind::Star | TokenKind::Slash | TokenKind::Percent) {
        let operator = cursor.bump()?.kind; // Consume the *, /, or %
        let right = parse_variable_declaration(cursor)?;

        expression = Expression::Binary(Binary {
            left: Box::new(expression),
            right: Box::new(right),
            operator: match operator {
                TokenKind::Star => BinaryOperator::Multiply,
                TokenKind::Slash => BinaryOperator::Divide,
                TokenKind::Percent => BinaryOperator::Modulo,
                _ => unreachable!("Expected *, /, or % but found {:?}", operator),
            },
        });
    }

    Ok(expression)
}

fn parse_variable_declaration(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Let) {
        return parse_if(cursor);
    }

    cursor.bump()?; // Consume the let

    let mutable = matches!(cursor.first().kind, TokenKind::Keyword(Keyword::Mut));

    if mutable {
        cursor.bump()?; // Consume the mutable
    }
    
    let TokenKind::Identifier(identifier) = cursor.bump()?.kind else {
        return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
    };
    
    if cursor.first().kind != TokenKind::Colon {
        return Err(format!("Expected : but found {:?}", cursor.first().kind));
    }

    cursor.bump()?; // Consume the :
    
    if !can_be_type_annotation(cursor) {
        return Err(format!("Expected type annotation but found {:?}", cursor.first().kind));
    }

    let type_annotation = parse_type_annotation(cursor, false)?;

    match cursor.first().kind {
        TokenKind::Equal => {
            cursor.bump()?; // Consume the =

            let initializer = parse_expression(cursor)?;

            Ok(Expression::VariableDeclaration(VariableDeclaration {
                mutable,
                type_annotation,
                identifier,
                initializer: Some(Box::new(initializer)),
            }))
        },
        TokenKind::Semicolon => {
            Ok(Expression::VariableDeclaration(VariableDeclaration {
                mutable,
                type_annotation,
                identifier,
                initializer: None,
            }))
        },
        _ => Err(format!("Expected = or ; but found {:?}", cursor.first().kind)),
    }
}
 
fn parse_if(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::If) {
        return parse_unary(cursor);
    }

    cursor.bump()?; // Consume the if

    let if_condition = parse_expression(cursor)?;
    let if_block = parse_block(cursor)?;

    let mut else_ifs = vec![];
    let mut r#else = None;

    while cursor.first().kind == TokenKind::Keyword(Keyword::Else) {
        cursor.bump()?; // Consume the else

        if cursor.first().kind == TokenKind::Keyword(Keyword::If) {
            cursor.bump()?; // Consume the if

            let condition = parse_expression(cursor)?;
            let block = parse_block(cursor)?;

            else_ifs.push(ConditionBlock { condition: Box::new(condition), block: Box::new(block) });
        } else {
            let block = parse_block(cursor)?;
            r#else = Some(Box::new(block));
        }
    }

    return Ok(Expression::If(If
    {
        r#if: ConditionBlock { condition: Box::new(if_condition), block: Box::new(if_block) },
        else_ifs: if else_ifs.len() > 0 { Some(else_ifs) } else { None },
        r#else
    }))
}

fn parse_unary(cursor: &mut Cursor) -> Result<Expression, String> {
    if matches!(cursor.first().kind, TokenKind::Plus | TokenKind::Minus | TokenKind::Bang | TokenKind::Tilde) {
        let operator = cursor.bump()?.kind; // Consume the +, -, !, or ~
        let right = parse_unary(cursor)?;

        return Ok(Expression::Unary(Unary {
            operator: match operator {
                TokenKind::Plus => UnaryOperator::Identity,
                TokenKind::Minus => UnaryOperator::Negate,
                TokenKind::Bang => UnaryOperator::LogicalNot,
                TokenKind::Tilde => UnaryOperator::BitwiseNot,
                _ => unreachable!("Expected +, -, !, or ~ but found {:?}", operator),
            },
            expression: Box::new(right),
        }));
    }

    parse_call_member(cursor)
}

fn parse_call_member(cursor: &mut Cursor) -> Result<Expression, String> {
    let member = parse_member_access(cursor)?;

    if cursor.first().kind == TokenKind::OpenParen {
        return parse_call_expression(member, cursor);
    }

    if cursor.first().kind == TokenKind::OpenBracket {
        return parse_index_expression(member, cursor);
    }

    return Ok(member);
}

fn parse_call_expression(caller: Expression, cursor: &mut Cursor) -> Result<Expression, String> {
    let arguments = parse_args(cursor)?;
    cursor.bump()?; // Consume the )
    
    let mut call = Expression::Call(Call { caller: Box::new(caller.clone()), arguments });

    if let TokenKind::OpenParen = cursor.first().kind {
        call = parse_call_expression(call, cursor)?;
    }

    return Ok(call);
}

fn parse_args(cursor: &mut Cursor) -> Result<Vec<Expression>, String> {
    let TokenKind::OpenParen = cursor.bump()?.kind else {
        return Err(format!("Expected ( but found {:?}", cursor.first().kind));
    };

    if let TokenKind::CloseParen = cursor.first().kind {
        Ok(vec![])
    } else {
        parse_args_list(cursor)
    }
}

fn parse_args_list(cursor: &mut Cursor) -> Result<Vec<Expression>, String> {
    let mut args = parse_expression(cursor)
        .map(|e| vec![e])?;

    while let TokenKind::Comma = cursor.first().kind {
        cursor.bump()?; // Consume the ,
        let expression = parse_expression(cursor)?;
        args.push(expression);
    }

    Ok(args)
}

fn parse_index_expression(caller: Expression, cursor: &mut Cursor) -> Result<Expression, String> {
    cursor.bump()?; // Consume the [
    let argument = parse_expression(cursor)?;
    cursor.bump()?; // Consume the ]
    
    let mut index = Expression::Index(Index {
        caller: Box::new(caller.clone()),
        index: Box::new(argument)
    });

    if let TokenKind::OpenBracket = cursor.first().kind {
        index = parse_index_expression(index, cursor)?;
    }

    return Ok(index);
}

fn parse_member_access(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut object = parse_literal(cursor)?;

    while let TokenKind::DoubleColon = cursor.first().kind {
        cursor.bump()?; // Consume the .

        let TokenKind::Identifier(identifier) = cursor.first().kind else {
            return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
        };

        let Expression::Member(member) = parse_literal(cursor)? else {
            return Err(format!("Expected member but found {:?}", cursor.first().kind));
        };

        object = Expression::Member(Member::MemberAccess {
            object: Box::new(object),
            member: Box::new(member),
            symbol: identifier,
        });
    }

    while let TokenKind::Dot = cursor.first().kind {
        cursor.bump()?; // Consume the .

        let TokenKind::Identifier(identifier) = cursor.first().kind else {
            return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
        };

        let Expression::Member(member) = parse_literal(cursor)? else {
            return Err(format!("Expected member but found {:?}", cursor.first().kind));
        };

        object = Expression::Member(Member::MemberAccess {
            object: Box::new(object),
            member: Box::new(member),
            symbol: identifier,
        });
    }

    Ok(object)
}

pub fn parse_literal(cursor: &mut Cursor) -> Result<Expression, String> {
    let TokenKind::Literal(literal) = cursor.first().kind else {
        return parse_primary(cursor);
    };
    
    cursor.bump()?; // Consume the literal
    to_expression_literal(literal)
}

fn parse_primary(cursor: &mut Cursor) -> Result<Expression, String> {
    match cursor.first().kind {
        TokenKind::Identifier(identifier) => {
            cursor.bump()?; // Consume the identifier
            Ok(Expression::Member(Member::Identifier { symbol: identifier }))
        },
        TokenKind::OpenParen => {
            cursor.bump()?; // Consume the (

            let expression = parse_expression(cursor)?;

            match cursor.first().kind {
                TokenKind::CloseParen => {
                    cursor.bump()?; // Consume the )
                    Ok(expression)
                },
                _ => Err(format!("Expected ) but found {:?}", cursor.first().kind)),
            }
        },
        TokenKind::OpenBracket => {
            cursor.bump()?; // Consume the [

            let mut elements = vec![];

            while cursor.first().kind != TokenKind::CloseBracket {
                elements.push(parse_expression(cursor)?);

                if cursor.first().kind == TokenKind::Comma {
                    cursor.bump()?; // Consume the ,
                }
            }

            cursor.bump()?; // Consume the ]

            Ok(Expression::Literal(Literal::Array(elements)))
        }
        _ => Err(format!("Expected primary expression but found {:?}", cursor.first().kind)),
    }
}

fn to_expression_literal(literal: token::Literal) -> Result<Expression, String> {
    match literal {
        token::Literal::Unit => Ok(Expression::Literal(Literal::Unit)),
        token::Literal::I8(literal) => Ok(Expression::Literal(Literal::I8(literal.value))),
        token::Literal::I16(literal) => Ok(Expression::Literal(Literal::I16(literal.value))),
        token::Literal::I32(literal) => Ok(Expression::Literal(Literal::I32(literal.value))),
        token::Literal::I64(literal) => Ok(Expression::Literal(Literal::I64(literal.value))),
        token::Literal::I128(literal) => Ok(Expression::Literal(Literal::I128(literal.value))),
        token::Literal::U8(literal) => Ok(Expression::Literal(Literal::U8(literal.value))),
        token::Literal::U16(literal) => Ok(Expression::Literal(Literal::U16(literal.value))),
        token::Literal::U32(literal) => Ok(Expression::Literal(Literal::U32(literal.value))),
        token::Literal::U64(literal) => Ok(Expression::Literal(Literal::U64(literal.value))),
        token::Literal::U128(literal) => Ok(Expression::Literal(Literal::U128(literal.value))),
        token::Literal::F32(value) => Ok(Expression::Literal(Literal::F32(value))),
        token::Literal::F64(value) => Ok(Expression::Literal(Literal::F64(value))),
        token::Literal::String(value) => Ok(Expression::Literal(Literal::String(value))),
        token::Literal::Char(value) => Ok(Expression::Literal(Literal::Char(value.parse::<char>().unwrap()))),
        token::Literal::Bool(value) => Ok(Expression::Literal(Literal::Bool(value))),
    }
}
