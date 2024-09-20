use std::collections::HashMap;

use crate::{
    lexer::token::{self, IdentifierType, Keyword, TokenKind},
    type_checker::decision_tree::{Constructor, FieldPattern, Pattern},
    types::{parse_generics_in_type_name, parse_optional_type_annotation, TypeAnnotation},
};

use super::{
    cursor::Cursor, statements::parse_statement, Assignment, Binary, BinaryOperator, Call, Closure,
    ClosureParameter, EnumMemberFieldInitializers, Expression, FieldInitializer, For, If, Literal,
    Match, MatchArm, Member, Statement, Unary, UnaryOperator, VariableDeclaration, While,
};

use crate::types::parse_type_annotation;

pub fn parse_expression(cursor: &mut Cursor) -> Result<Expression, String> {
    #[cfg(feature = "interpreter")]
    let expression = parse_print(cursor);

    #[cfg(not(feature = "interpreter"))]
    let expression = parse_break(cursor);

    return expression;
}

#[cfg(feature = "interpreter")]
fn parse_print(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Print) {
        return parse_drop(cursor);
    }

    cursor.bump()?; // Consume the drop

    let TokenKind::OpenParen = cursor.bump()?.kind else {
        return Err(format!("Expected ( but found {:?}", cursor.first().kind));
    };

    let expression = parse_expression(cursor)?;

    let TokenKind::CloseParen = cursor.bump()?.kind else {
        return Err(format!("Expected ) but found {:?}", cursor.first().kind));
    };

    Ok(Expression::Print(Box::new(expression)))
}

#[cfg(feature = "interpreter")]
fn parse_drop(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Drop) {
        return parse_break(cursor);
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

fn parse_break(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Break) {
        return parse_continue(cursor);
    }

    cursor.bump()?; // Consume the break

    let expression = if cursor.first().kind == TokenKind::Semicolon {
        cursor.bump()?;
        None
    } else {
        Some(parse_expression(cursor)?)
    };

    Ok(Expression::Break(expression.map(Box::new)))
}

fn parse_continue(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Continue) {
        return parse_return(cursor);
    }

    cursor.bump()?; // Consume the continue
    Ok(Expression::Continue)
}

fn parse_return(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Return) {
        return parse_trailing_closure(cursor);
    }

    cursor.bump()?; // Consume the return

    let expression = if cursor.first().kind == TokenKind::Semicolon {
        cursor.bump()?; // Consume the ;
        None
    } else {
        Some(parse_expression(cursor)?)
    };

    Ok(Expression::Return(expression.map(Box::new)))
}

fn parse_trailing_closure(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut expression = parse_loop(cursor)?;

    while cursor.first().kind == TokenKind::Arrow {
        cursor.bump()?; // Consume the ->

        let mut params = None;

        if cursor.first().kind == TokenKind::Pipe {
            cursor.bump()?; // Consume the |
            params = Some(parse_comma_separated_closure_params(cursor)?);
            cursor.expect(TokenKind::Pipe)?;
        }

        cursor.optional_bump(TokenKind::FatArrow)?;

        let return_type_annotation = if cursor.first().kind == TokenKind::Colon {
            cursor.bump()?; // Consume the :
            Some(parse_type_annotation(cursor, true)?)
        } else {
            None
        };

        let body = parse_loop(cursor)?;

        let Some(params) = params else {
            expression = Expression::Call(Call {
                callee: Box::new(expression),
                argument: Some(Box::new(Expression::Closure(Closure {
                    param: None,
                    return_type_annotation,
                    body: Box::new(body),
                }))),
            });
            continue;
        };

        expression = Expression::Call(Call {
            callee: Box::new(expression),
            argument: Some(Box::new(unwrap_arguments(
                params,
                return_type_annotation,
                body,
            )?)),
        });
    }

    Ok(expression)
}

pub fn parse_loop(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Loop) {
        return parse_while(cursor);
    }

    cursor.bump()?; // Consume the loop
    cursor.optional_bump(TokenKind::FatArrow)?;

    let body = parse_expression(cursor)?;

    Ok(Expression::Loop(Box::new(body)))
}

pub fn parse_while(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::While) {
        return parse_for(cursor);
    }

    cursor.bump()?; // Consume the while

    let condition = parse_expression(cursor)?;
    cursor.expect(TokenKind::FatArrow)?;
    let body = parse_expression(cursor)?;

    if cursor.first().kind != TokenKind::Keyword(Keyword::Else) {
        return Ok(Expression::While(While {
            condition: Box::new(condition),
            body: Box::new(body),
            else_body: None,
        }));
    }

    cursor.bump()?; // Consume the else
    cursor.optional_bump(TokenKind::FatArrow)?;

    let else_body = parse_expression(cursor)?;

    Ok(Expression::While(While {
        condition: Box::new(condition),
        body: Box::new(body),
        else_body: Some(Box::new(else_body)),
    }))
}

pub fn parse_for(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::For) {
        return parse_block(cursor);
    }

    cursor.bump()?; // Consume the for

    let TokenKind::Identifier(identifier) = cursor.first().kind else {
        return Err(format!(
            "Expected identifier but found {:?}",
            cursor.first().kind
        ));
    };

    cursor.bump()?; // Consume the identifier

    cursor.expect(TokenKind::Keyword(Keyword::In))?;

    let iterable = parse_expression(cursor)?;
    cursor.expect(TokenKind::FatArrow)?;
    let body = parse_expression(cursor)?;

    if cursor.first().kind != TokenKind::Keyword(Keyword::Else) {
        return Ok(Expression::For(For {
            identifier,
            iterable: Box::new(iterable),
            body: Box::new(body),
            else_body: None,
        }));
    }

    cursor.bump()?; // Consume the else
    cursor.optional_bump(TokenKind::FatArrow)?;

    let else_block = parse_expression(cursor)?;

    Ok(Expression::For(For {
        identifier,
        iterable: Box::new(iterable),
        body: Box::new(body),
        else_body: Some(Box::new(else_block)),
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
    println!("1");
    let TokenKind::Identifier(_) = cursor.first().kind else {
        return parse_range(cursor);
    };

    println!("2");
    if !matches!(
        cursor.second().kind,
        TokenKind::OpenBrace | TokenKind::DoubleColon | TokenKind::Less
    ) {
        return parse_range(cursor);
    };

    println!("3");
    if let TokenKind::Identifier(name) = cursor.third().kind {
        if cursor.second().kind == TokenKind::DoubleColon && name.is_function_identifier_name() {
            return parse_member_access(cursor);
        }
    }

    println!("ADSLKH {:?}", cursor.first());
    let type_annotation = parse_type_annotation(cursor, false)?;

    if type_annotation.has_double_colon() {
        parse_enum_literal(cursor, type_annotation)
    } else {
        parse_struct_literal(cursor, type_annotation)
    }
}

fn parse_struct_literal(
    cursor: &mut Cursor,
    type_annotation: TypeAnnotation,
) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::OpenBrace {
        return Ok(Expression::Literal(Literal::Struct {
            type_annotation,
            field_initializers: vec![],
        }));
    }

    cursor.bump()?; // Consume the {

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
        if cursor.first().kind == TokenKind::OpenBrace {
            cursor.bump()?; // Consume the {
            let fields = parse_named_enum_member_field_initializers(cursor)?;
            cursor.bump()?; // Consume the }
            fields
        } else {
            EnumMemberFieldInitializers::None
        }
    };

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

    while cursor.first().kind != TokenKind::CloseBrace {
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

fn parse_range(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut expression = parse_assignment(cursor)?;

    while cursor.first().kind == TokenKind::DoubleDot {
        let operator = cursor.bump()?.kind; // Consume the ..

        let inclusive = cursor.first().kind == TokenKind::Equal;

        if inclusive {
            cursor.bump()?; // Consume the =
        }

        let right = parse_assignment(cursor)?;

        expression = Expression::Binary(Binary {
            left: Box::new(expression),
            right: Box::new(right),
            operator: match (&operator, inclusive) {
                (TokenKind::DoubleDot, false) => BinaryOperator::Range,
                (TokenKind::DoubleDot, true) => BinaryOperator::RangeInclusive,
                _ => unreachable!("Expected .. but found {:?}", operator),
            },
        });
    }

    Ok(expression)
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
        return parse_match(cursor);
    }

    cursor.bump()?; // Consume the |

    let params = parse_comma_separated_closure_params(cursor)?;

    cursor.bump()?; // Consume the |

    let return_type_annotation = parse_optional_type_annotation(cursor, true)?;

    // Optional fat arrow
    if cursor.first().kind == TokenKind::FatArrow {
        cursor.bump()?; // Consume the =>
    }

    let body = parse_expression(cursor)?;

    unwrap_arguments(params, return_type_annotation, body)
}

fn parse_comma_separated_closure_params(
    cursor: &mut Cursor,
) -> Result<Vec<ClosureParameter>, String> {
    let mut params = vec![];

    while cursor.first().kind != TokenKind::Pipe {
        let TokenKind::Identifier(identifier) = cursor.first().kind else {
            return Err(format!(
                "Expected identifier but found {:?}",
                cursor.first().kind
            ));
        };

        cursor.bump()?; // Consume the identifier

        let type_annotation = parse_optional_type_annotation(cursor, false)?;

        params.push(ClosureParameter {
            identifier,
            type_annotation,
        });

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        }
    }

    Ok(params)
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

            let new_return_type_annotation = return_type_annotation.map(|rta| {
                TypeAnnotation::Function(last.type_annotation.map(Box::new), Some(Box::new(rta)))
            });

            unwrap_arguments_recurse(
                params.into_iter().rev().skip(1).rev().collect(),
                new_return_type_annotation,
                new_body,
            )
        }
    }
}

fn parse_match(cursor: &mut Cursor) -> Result<Expression, String> {
    let expression = parse_boolean_logical(cursor)?;

    if cursor.first().kind != TokenKind::Keyword(Keyword::Match) {
        return Ok(expression);
    }

    cursor.bump()?; // Consume the match

    let mut arms = vec![];

    if cursor.first().kind == TokenKind::Pipe {
        cursor.bump()?; // Consume the |
    }

    loop {
        let pattern = parse_pattern(cursor)?;
        cursor.expect(TokenKind::FatArrow)?; // Consume the =>
        let body = parse_expression(cursor)?;

        arms.push(MatchArm {
            pattern,
            expression: Box::new(body),
        });

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        }

        if cursor.first().kind != TokenKind::Pipe {
            break;
        }

        cursor.expect(TokenKind::Pipe)?; // Consume the |
    }
    while cursor.first().kind == TokenKind::Pipe {
        cursor.bump()?; // Consume the arm keyword
        let pattern = parse_pattern(cursor)?;
        cursor.expect(TokenKind::FatArrow)?; // Consume the =>
        let body = parse_expression(cursor)?;

        arms.push(MatchArm {
            pattern,
            expression: Box::new(body),
        });

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        }
    }

    Ok(Expression::Match(Match {
        expression: Box::new(expression),
        arms,
    }))
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

    let pattern = parse_pattern(cursor)?;

    let type_annotation = parse_optional_type_annotation(cursor, false)?;

    match cursor.first().kind {
        TokenKind::Equal => {
            cursor.bump()?; // Consume the =

            let initializer = parse_expression(cursor)?;

            Ok(Expression::VariableDeclaration(VariableDeclaration {
                mutable,
                type_annotation,
                pattern,
                initializer: Some(Box::new(initializer)),
            }))
        }
        TokenKind::Semicolon => Ok(Expression::VariableDeclaration(VariableDeclaration {
            mutable,
            type_annotation,
            pattern,
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
    cursor.expect(TokenKind::FatArrow)?;
    let if_block = parse_expression(cursor)?;

    let mut r#else = None;

    if cursor.first().kind == TokenKind::Keyword(Keyword::Else) {
        cursor.bump()?; // Consume the else

        if cursor.first().kind == TokenKind::Keyword(Keyword::If) {
            r#else = Some(Box::new(parse_if(cursor)?));
        } else {
            cursor.optional_bump(TokenKind::FatArrow)?;
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

        if matches!(operator, TokenKind::Minus)
            && matches!(
                right,
                Expression::Literal(Literal::Int(_)) | Expression::Literal(Literal::Float(_))
            )
        {
            match right {
                Expression::Literal(Literal::Int(value)) => {
                    return Ok(Expression::Literal(Literal::Int(-value)));
                }
                Expression::Literal(Literal::Float(value)) => {
                    return Ok(Expression::Literal(Literal::Float(-value)));
                }
                _ => unreachable!("Checked in previous if"),
            }
        }

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

    parse_call_or_param_propagation(cursor)
}

fn parse_call_or_param_propagation(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut expression = parse_member_access(cursor)?;

    if cursor.first().kind == TokenKind::DoubleColon && cursor.second().kind == TokenKind::Less {
        cursor.bump()?; // Consume the ::
        let generics = parse_generics_in_type_name(cursor)?;

        let Expression::Member(member) = expression.clone() else {
            return Err(format!("Expected member but found {:?}", expression));
        };

        expression = Expression::Member(member.with_generics(generics));
    }

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
                    generics: None,
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
        let Expression::Member(Member::Identifier { symbol, generics }) = &object else {
            break;
        };

        if !symbol.is_type_identifier_name() {
            break;
        }

        let type_annotation = match generics {
            Some(generics) => TypeAnnotation::ConcreteType(
                symbol.clone(),
                generics
                    .clone()
                    .into_iter()
                    .map(|g| g.type_annotation())
                    .collect(),
            ),
            None => TypeAnnotation::Type(symbol.clone()),
        };

        println!("aksdjshalksjhsd");
        println!("1");

        let TokenKind::Identifier(identifier) = cursor.second().kind else {
            return Err(format!(
                "Expected identifier but found {:?}",
                cursor.first().kind
            ));
        };

        println!("{:?}", identifier);
        println!("2");
        if !identifier.is_function_identifier_name() {
            return Ok(object);
        }

        cursor.bump()?; // Consume the ::

        println!("5");
        let Expression::Member(member) = parse_literal(cursor)? else {
            return Err(format!(
                "Expected member but found {:?}",
                cursor.first().kind
            ));
        };

        println!("6");
        object = Expression::Member(Member::StaticMemberAccess {
            type_annotation,
            member: Box::new(member),
            symbol: identifier,
            generics: None,
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
            generics: None,
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
            generics: None,
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
                generics: None,
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

fn parse_pattern(cursor: &mut Cursor) -> Result<Pattern, String> {
    let pattern = parse_single_pattern(cursor)?;

    println!("Parsing pattern");

    if cursor.first().kind == TokenKind::DoubleDot {
        cursor.bump()?; // Consume the ..

        println!("Parsing range");

        let inclusive = cursor.first().kind == TokenKind::Equal;

        if inclusive {
            cursor.bump()?; // Consume the =

            println!("Inclusive range");
        }

        let right = parse_pattern(cursor)?;

        return Ok(Pattern::Range(
            Box::new(pattern),
            Box::new(right),
            inclusive,
        ));
    }

    Ok(pattern)
}

fn parse_single_pattern(cursor: &mut Cursor) -> Result<Pattern, String> {
    match cursor.first().kind {
        TokenKind::Underscore => {
            cursor.bump()?; // Consume the _
            Ok(Pattern::Wildcard)
        }
        TokenKind::Literal(token::Literal::Unit) => {
            cursor.bump()?; // Consume the ()
            Ok(Pattern::Unit)
        }
        TokenKind::Literal(token::Literal::Bool(v)) => {
            cursor.bump()?; // Consume the ()
            Ok(Pattern::Bool(v))
        }
        TokenKind::Literal(token::Literal::Int(v)) => {
            cursor.bump()?; // Consume the literal
            Ok(Pattern::Int(v.value))
        }
        TokenKind::Literal(token::Literal::UInt(v)) => {
            cursor.bump()?; // Consume the literal
            Ok(Pattern::UInt(v.value))
        }
        TokenKind::Literal(token::Literal::Float(v)) => {
            cursor.bump()?; // Consume the literal
            Ok(Pattern::Float(v))
        }
        TokenKind::Literal(token::Literal::Char(v)) => {
            cursor.bump()?; // Consume the literal
            Ok(Pattern::Char(
                v.parse::<char>().expect("Failed to parse char literal"),
            ))
        }
        TokenKind::Literal(token::Literal::String(v)) => {
            cursor.bump()?; // Consume the literal
            Ok(Pattern::String(v))
        }
        TokenKind::Identifier(ident) if ident.is_type_identifier_name() => {
            let type_annotation = parse_type_annotation(cursor, false)?;

            if cursor.first().kind != TokenKind::OpenBrace {
                return Ok(Pattern::Constructor(Constructor::Struct {
                    type_annotation,
                    field_patterns: vec![],
                }));
            }

            cursor.bump()?; // Consume the {

            let mut fields = vec![];

            while cursor.first().kind != TokenKind::CloseBrace {
                let TokenKind::Identifier(identifier) = cursor.first().kind else {
                    return Err(format!(
                        "Expected identifier but found {:?}",
                        cursor.first().kind
                    ));
                };

                cursor.bump()?; // Consume the identifier

                if cursor.first().kind != TokenKind::Colon {
                    fields.push(FieldPattern {
                        identifier: identifier.clone(),
                        pattern: Pattern::Variable(identifier),
                    });

                    if cursor.first().kind == TokenKind::Comma {
                        cursor.bump()?; // Consume the ,
                    }

                    continue;
                }

                cursor.bump()?; // Consume the :

                let pattern = parse_pattern(cursor)?;

                fields.push(FieldPattern {
                    identifier,
                    pattern,
                });

                if cursor.first().kind == TokenKind::Comma {
                    cursor.bump()?; // Consume the ,
                }
            }

            cursor.expect(TokenKind::CloseBrace)?;
            Ok(Pattern::Constructor(Constructor::Struct {
                type_annotation,
                field_patterns: fields,
            }))
        }
        TokenKind::Identifier(identifier) if identifier.is_variable_identifier_name() => {
            cursor.bump()?; // Consume the identifier

            Ok(Pattern::Variable(identifier))
        }
        TokenKind::Less => {
            cursor.bump()?; // Consume the <
            let pattern = parse_pattern(cursor)?;
            Ok(Pattern::LessThan(Box::new(pattern)))
        }
        TokenKind::Greater => {
            cursor.bump()?; // Consume the >
            let pattern = parse_pattern(cursor)?;
            Ok(Pattern::GreaterThan(Box::new(pattern)))
        }
        TokenKind::LessEqual => {
            cursor.bump()?; // Consume the <=
            let pattern = parse_pattern(cursor)?;
            Ok(Pattern::LessThanOrEqual(Box::new(pattern)))
        }
        TokenKind::GreaterEqual => {
            cursor.bump()?; // Consume the >=
            let pattern = parse_pattern(cursor)?;
            Ok(Pattern::GreaterThanOrEqual(Box::new(pattern)))
        }
        _ => Err(format!(
            "Unknown start of pattern: {:?}",
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
            value.parse::<char>().expect("Failed to parse char literal"),
        ))),
        token::Literal::Bool(value) => Ok(Expression::Literal(Literal::Bool(value))),
    }
}
