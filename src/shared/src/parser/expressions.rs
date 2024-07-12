use std::collections::HashMap;

use crate::{
    lexer::token::{self, Keyword, TokenKind},
    types::{parse_optional_type_annotation, TypeAnnotation},
};

use super::{
    cursor::Cursor, statements::parse_statement, Assignment, Binary, BinaryOperator, Call, Closure,
    ClosureParameter, EnumMemberFieldInitializers, Expression, FieldInitializer, If, Literal,
    Member, Statement, Unary, UnaryOperator, VariableDeclaration, While,
};
use crate::types::parse_type_annotation;

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
        return Err(format!(
            "Expected identifier but found {:?}",
            cursor.first().kind
        ));
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
            else_statements: None,
        }));
    }

    cursor.bump()?; // Consume the else

    let else_block = parse_block_statements(cursor)?;

    Ok(Expression::While(While {
        condition: Box::new(condition),
        statements: block,
        else_statements: Some(else_block),
    }))
}

pub fn parse_block(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::OpenBrace {
        return parse_type_literal(cursor);
    }

    parse_block_statements(cursor).map(|statements| Expression::Block(statements))
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

    if !matches!(
        cursor.second().kind,
        TokenKind::OpenBrace | TokenKind::DoubleColon
    ) {
        return parse_assignment(cursor);
    };

    let type_annotation = parse_type_annotation(cursor, false)?;

    if cursor.first().kind == TokenKind::OpenBrace {
        parse_struct_literal(cursor, type_annotation)
    } else {
        parse_enum_literal(cursor, type_annotation)
    }
}

fn parse_struct_literal(
    cursor: &mut Cursor,
    type_annotation: TypeAnnotation,
) -> Result<Expression, String> {
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
        field_initializers,
    }))
}

fn parse_field_initializer(cursor: &mut Cursor) -> Result<FieldInitializer, String> {
    let TokenKind::Identifier(identifier) = cursor.first().kind else {
        return Ok(FieldInitializer {
            identifier: None,
            initializer: parse_expression(cursor)?,
        });
    };

    let TokenKind::Colon = cursor.second().kind else {
        return Ok(FieldInitializer {
            identifier: None,
            initializer: parse_expression(cursor)?,
        });
    };

    cursor.bump()?; // Consume the identifier
    cursor.bump()?; // Consume the :

    let initializer = parse_expression(cursor)?;

    Ok(FieldInitializer {
        identifier: Some(identifier),
        initializer,
    })
}

fn parse_enum_literal(
    cursor: &mut Cursor,
    type_annotation: TypeAnnotation,
) -> Result<Expression, String> {
    let field_initializers = {
        if cursor.first().kind == TokenKind::OpenParen {
            cursor.bump()?; // Consume the (
            parse_named_enum_member_field_initializers(cursor)?
        } else {
            EnumMemberFieldInitializers::None
        }
    };

    cursor.bump()?; // Consume the )
    Ok(Expression::Literal(Literal::Enum {
        type_annotation: type_annotation.clone(),
        member: type_annotation
            .to_string()
            .split("::")
            .last()
            .unwrap()
            .to_string(),
        field_initializers,
    }))
}

fn parse_named_enum_member_field_initializers(
    cursor: &mut Cursor,
) -> Result<EnumMemberFieldInitializers, String> {
    let mut field_initializers = HashMap::new();

    while cursor.first().kind != TokenKind::CloseParen {
        let TokenKind::Identifier(identifier) = cursor.first().kind else {
            return Err(format!(
                "Expected identifier but found {:?}",
                cursor.first().kind
            ));
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
    let mut expression = parse_closure(cursor)?;

    while matches!(
        cursor.first().kind,
        TokenKind::PlusEqual
            | TokenKind::MinusEqual
            | TokenKind::StarEqual
            | TokenKind::SlashEqual
            | TokenKind::PercentEqual
            | TokenKind::AmpersandEqual
            | TokenKind::PipeEqual
            | TokenKind::CaretEqual
    ) {
        let operator = cursor.bump()?.kind; // Consume the +=, -=, *=, /=, %=, &=, |=, ^=
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
                    _ => unreachable!(
                        "Expected +=, -=, *=, /=, %=, &=, |=, or ^=, but found {:?}",
                        operator
                    ),
                },
            })),
        });
    }

    Ok(expression)
}

fn parse_closure(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Pipe {
        return parse_boolean_logical(cursor);
    }

    cursor.bump()?; // Consume the |

    let mut params = vec![];

    while cursor.first().kind != TokenKind::Pipe {
        let TokenKind::Identifier(identifier) = cursor.bump()?.kind else {
            return Err(format!(
                "Expected identifier but found {:?}",
                cursor.first().kind
            ));
        };

        let type_annotation = parse_optional_type_annotation(cursor, false)?;

        params.push(ClosureParameter {
            identifier,
            type_annotation,
        });

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        }
    }

    cursor.bump()?; // Consume the |

    let return_type_annotation = parse_optional_type_annotation(cursor, true)?;

    // Optional fat arrow
    if cursor.first().kind == TokenKind::FatArrow {
        cursor.bump()?; // Consume the =>
    }

    let body = parse_expression(cursor)?;

    unwrap_arguments(params, return_type_annotation, body)
}

fn unwrap_arguments(
    params: Vec<ClosureParameter>,
    return_type_annotation: Option<TypeAnnotation>,
    body: Expression,
) -> Result<Expression, String> {
    match params.first().cloned() {
        None => Ok(Expression::Closure(Closure {
            param: None,
            return_type_annotation,
            body: Box::new(body),
        })),
        Some(first) => {
            let (new_body, new_return_type_annotation) = unwrap_arguments_recurse(
                params.into_iter().skip(1).collect(),
                return_type_annotation,
                body,
            )?;

            Ok(Expression::Closure(Closure {
                param: Some(first),
                return_type_annotation: new_return_type_annotation,
                body: Box::new(new_body),
            }))
        }
    }
}

fn unwrap_arguments_recurse(
    params: Vec<ClosureParameter>,
    return_type_annotation: Option<TypeAnnotation>,
    body: Expression,
) -> Result<(Expression, Option<TypeAnnotation>), String> {
    match params.last().cloned() {
        None => Ok((body, return_type_annotation)),
        Some(last) => {
            let new_body = Expression::Closure(Closure {
                param: Some(last.clone()),
                return_type_annotation: return_type_annotation.clone(),
                body: Box::new(body),
            });

            let new_return_type_annotation = Some(TypeAnnotation::Function(
                last.type_annotation.map(Box::new),
                return_type_annotation.clone().map(Box::new),
            ));

            unwrap_arguments_recurse(
                params.into_iter().rev().skip(1).rev().collect(),
                new_return_type_annotation,
                new_body,
            )
        }
    }
}

fn parse_boolean_logical(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut expression = parse_comparison(cursor)?;

    while matches!(
        (cursor.first().kind, cursor.second().kind),
        (TokenKind::DoubleAmpersand, _) | (TokenKind::Pipe, TokenKind::Pipe)
    ) {
        let operator = cursor.bump()?.kind; // Consume the && or |

        if matches!(operator, TokenKind::Pipe) {
            cursor.bump()?; // Consume the second |
        }

        let right = parse_comparison(cursor)?;

        expression = Expression::Binary(Binary {
            left: Box::new(expression),
            right: Box::new(right),
            operator: match operator {
                TokenKind::DoubleAmpersand => BinaryOperator::LogicalAnd,
                TokenKind::Pipe => BinaryOperator::LogicalOr,
                _ => unreachable!("Expected && or || but found {:?}", operator),
            },
        });
    }

    Ok(expression)
}

fn parse_comparison(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut expression = parse_bitwise_logical(cursor)?;

    while matches!(
        cursor.first().kind,
        TokenKind::DoubleEqual
            | TokenKind::BangEqual
            | TokenKind::Greater
            | TokenKind::Less
            | TokenKind::GreaterEqual
            | TokenKind::LessEqual
    ) {
        let operator = cursor.bump()?.kind; // Consume the ==, !=, >, <, >=, or <=
        let right = parse_additive(cursor)?;

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

    while matches!(
        (cursor.first().kind, cursor.second().kind),
        (TokenKind::Caret, _)
            | (TokenKind::Ampersand, _)
            | (TokenKind::Greater, TokenKind::Greater)
            | (TokenKind::Less, TokenKind::Less)
    ) || matches!(
        (cursor.first().kind, cursor.second().kind),
        (TokenKind::Pipe, t) if t != TokenKind::Pipe)
    {
        let operator = cursor.bump()?.kind; // Consume the (first >), (first <), ^, &, or |

        if matches!(operator, TokenKind::Greater | TokenKind::Less) {
            cursor.bump()?; // Consume the second > or <
        }

        let right = parse_boolean_logical(cursor)?;

        expression = Expression::Binary(Binary {
            left: Box::new(expression),
            right: Box::new(right),
            operator: match operator {
                TokenKind::Greater => BinaryOperator::BitwiseRightShift,
                TokenKind::Less => BinaryOperator::BitwiseLeftShift,
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

    while matches!(
        cursor.first().kind,
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent
    ) {
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
        return Err(format!(
            "Expected identifier but found {:?}",
            cursor.first().kind
        ));
    };

    let type_annotation = parse_optional_type_annotation(cursor, false)?;

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
        }
        TokenKind::Semicolon => Ok(Expression::VariableDeclaration(VariableDeclaration {
            mutable,
            type_annotation,
            identifier,
            initializer: None,
        })),
        _ => Err(format!(
            "Expected = or ; but found {:?}",
            cursor.first().kind
        )),
    }
}

fn parse_if(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::If) {
        return parse_unary(cursor);
    }

    cursor.bump()?; // Consume the if

    let if_condition = parse_expression(cursor)?;
    let if_block = parse_expression(cursor)?;

    let mut r#else = None;

    if cursor.first().kind == TokenKind::Keyword(Keyword::Else) {
        cursor.bump()?; // Consume the else

        if cursor.first().kind == TokenKind::Keyword(Keyword::If) {
            r#else = Some(Box::new(parse_if(cursor)?));
        } else {
            let block = parse_block(cursor)?;
            r#else = Some(Box::new(block));
        }
    }

    return Ok(Expression::If(If {
        condition: Box::new(if_condition),
        true_expression: Box::new(if_block),
        false_expression: r#else,
    }));
}

fn parse_unary(cursor: &mut Cursor) -> Result<Expression, String> {
    if matches!(
        cursor.first().kind,
        TokenKind::Plus | TokenKind::Minus | TokenKind::Bang | TokenKind::Tilde
    ) {
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

    parse_trailing_closure(cursor)
}

fn parse_trailing_closure(cursor: &mut Cursor) -> Result<Expression, String> {
    let call = parse_call_or_param_propagation(cursor)?;

    if cursor.first().kind != TokenKind::FatArrow {
        return Ok(call);
    }

    cursor.bump()?; // Consume the =>

    let return_type_annotation = if cursor.first().kind == TokenKind::Colon {
        cursor.bump()?; // Consume the :
        Some(parse_type_annotation(cursor, true)?)
    } else {
        None
    };

    let body = parse_expression(cursor)?;

    Ok(Expression::Call(Call {
        callee: Box::new(call),
        argument: Some(Box::new(Expression::Closure(Closure {
            param: None,
            return_type_annotation,
            body: Box::new(body),
        }))),
    }))
}

fn parse_call_or_param_propagation(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut expression = parse_member_access(cursor)?;

    loop {
        match cursor.first().kind {
            // Call expression
            TokenKind::OpenParen => {
                expression = parse_call_expression(expression, cursor)?;
            }
            // Param propagation
            TokenKind::Colon => {
                cursor.bump()?; // Consume the :

                let TokenKind::Identifier(identifier) = cursor.first().kind else {
                    return Err(format!(
                        "Expected identifier but found {:?}",
                        cursor.first().kind
                    ));
                };

                let Expression::Member(member) = parse_primary(cursor)? else {
                    return Err(format!(
                        "Expected member but found {:?}",
                        cursor.first().kind
                    ));
                };

                expression = Expression::Member(Member::ParamPropagation {
                    object: Box::new(expression),
                    member: Box::new(member),
                    symbol: identifier,
                });
            }
            _ => break,
        }
    }

    return Ok(expression);
}

fn parse_call_expression(callee: Expression, cursor: &mut Cursor) -> Result<Expression, String> {
    let arguments = parse_args(cursor)?;
    cursor.bump()?; // Consume the )

    let mut call = Expression::Call(Call {
        callee: Box::new(callee.clone()),
        argument: arguments.first().map(|a| Box::new(a.clone())),
    });

    for arg in arguments.into_iter().skip(1) {
        call = Expression::Call(Call {
            callee: Box::new(call),
            argument: Some(Box::new(arg)),
        })
    }

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
    let mut args = parse_expression(cursor).map(|e| vec![e])?;

    while let TokenKind::Comma = cursor.first().kind {
        cursor.bump()?; // Consume the ,
        let expression = parse_expression(cursor)?;
        args.push(expression);
    }

    Ok(args)
}

fn parse_member_access(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut object = parse_literal(cursor)?;

    while let TokenKind::DoubleColon = cursor.first().kind {
        cursor.bump()?; // Consume the ::

        let TokenKind::Identifier(identifier) = cursor.first().kind else {
            return Err(format!(
                "Expected identifier but found {:?}",
                cursor.first().kind
            ));
        };

        let Expression::Member(member) = parse_literal(cursor)? else {
            return Err(format!(
                "Expected member but found {:?}",
                cursor.first().kind
            ));
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
            return Err(format!(
                "Expected identifier but found {:?}",
                cursor.first().kind
            ));
        };

        let Expression::Member(member) = parse_literal(cursor)? else {
            return Err(format!(
                "Expected member but found {:?}",
                cursor.first().kind
            ));
        };

        object = Expression::Member(Member::MemberAccess {
            object: Box::new(object),
            member: Box::new(member),
            symbol: identifier,
        });
    }

    if let TokenKind::Colon = cursor.first().kind {
        cursor.bump()?; // Consume the :

        let TokenKind::Identifier(identifier) = cursor.first().kind else {
            return Err(format!(
                "Expected identifier but found {:?}",
                cursor.first().kind
            ));
        };

        let Expression::Member(member) = parse_literal(cursor)? else {
            return Err(format!(
                "Expected member but found {:?}",
                cursor.first().kind
            ));
        };

        object = Expression::Member(Member::ParamPropagation {
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
            Ok(Expression::Member(Member::Identifier {
                symbol: identifier,
            }))
        }
        TokenKind::OpenParen => {
            cursor.bump()?; // Consume the (

            let expression = parse_expression(cursor)?;

            match cursor.first().kind {
                TokenKind::CloseParen => {
                    cursor.bump()?; // Consume the )
                    Ok(expression)
                }
                _ => Err(format!("Expected ) but found {:?}", cursor.first().kind)),
            }
        }
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
        _ => Err(format!(
            "Expected primary expression but found {:?}",
            cursor.first().kind
        )),
    }
}

fn to_expression_literal(literal: token::Literal) -> Result<Expression, String> {
    match literal {
        token::Literal::Void => Err("Void literals are not allowed".to_string()),
        token::Literal::Unit => Ok(Expression::Literal(Literal::Unit)),
        token::Literal::Int(literal) => Ok(Expression::Literal(Literal::Int(literal.value))),
        token::Literal::UInt(literal) => Ok(Expression::Literal(Literal::UInt(literal.value))),
        token::Literal::Float(value) => Ok(Expression::Literal(Literal::Float(value))),
        token::Literal::String(value) => Ok(Expression::Literal(Literal::String(value))),
        token::Literal::Char(value) => Ok(Expression::Literal(Literal::Char(
            value.parse::<char>().unwrap(),
        ))),
        token::Literal::Bool(value) => Ok(Expression::Literal(Literal::Bool(value))),
    }
}
