use crate::ast::pattern::PatternKind;
use crate::ast::ExpressionKind;
use crate::diagnostic::{Diagnostic, Span};
use crate::{
    ast::pattern::{Bound, ComparisonOperator, FieldPattern, Pattern},
    ast::{statements::ParseContext, ArrayItem, Index, UseExpr},
    lexer::token::{self, IdentifierType, Keyword, TokenKind},
    types::{parse_generics_in_type_name, parse_optional_type_annotation, ToKey, TypeAnnotation},
};

use super::{
    cursor::Cursor, fat_arrow_expr_or_block_expr, statements::parse_statement, Assignment, Binary,
    BinaryOperator, Call, Closure, ClosureParameter, Expression, FieldInitializer, For, If, Match,
    MatchArm, Member, Statement, Unary, UnaryOperator, ValueLiteral, VariableDeclaration, While,
};

use crate::types::parse_type_annotation;

pub fn parse_expression(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Expression, Diagnostic> {
    parse_break(cursor, context)
}

fn parse_break(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    if cursor.first().kind != TokenKind::Keyword(Keyword::Break) {
        return parse_continue(cursor, context);
    }

    cursor.bump()?; // Consume the break

    let expression = if cursor.first().kind == TokenKind::Semicolon {
        cursor.bump()?;
        None
    } else {
        Some(parse_expression(cursor, context)?)
    };

    Ok(ExpressionKind::Break(expression.map(Box::new)).at(cursor.span_from(start)))
}

fn parse_continue(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    if cursor.first().kind != TokenKind::Keyword(Keyword::Continue) {
        return parse_return(cursor, context);
    }

    cursor.bump()?; // Consume the continue
    Ok(ExpressionKind::Continue.at(cursor.span_from(start)))
}

fn parse_return(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    if cursor.first().kind != TokenKind::Keyword(Keyword::Return) {
        return parse_use_expression(cursor, context);
    }

    cursor.bump()?; // Consume the return

    let expression = if cursor.first().kind == TokenKind::Semicolon {
        cursor.bump()?; // Consume the ;
        None
    } else {
        Some(parse_expression(cursor, context)?)
    };

    Ok(ExpressionKind::Return(expression.map(Box::new)).at(cursor.span_from(start)))
}

fn parse_use_expression(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    let TokenKind::Keyword(Keyword::Use) = cursor.first().kind else {
        return parse_trailing_closure(cursor, context);
    };

    let args = parse_args_list(cursor, context)?;

    cursor.expect(TokenKind::LeftArrow)?;

    let expr = parse_trailing_closure(cursor, context)?;

    Ok(ExpressionKind::Use(UseExpr {
        args,
        expr: Box::new(expr),
    })
    .at(cursor.span_from(start)))
}

fn parse_trailing_closure(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    let mut expression = parse_loop(cursor, context)?;

    while cursor.first().kind == TokenKind::RightArrow {
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

        let body = parse_loop(cursor, context)?;

        let Some(params) = params else {
            expression = ExpressionKind::Call(Call {
                callee: Box::new(expression),
                argument: Some(Box::new(
                    ExpressionKind::Closure(Closure {
                        param: None,
                        return_type_annotation,
                        body: Box::new(body),
                    })
                    .at(cursor.span_from(start)),
                )),
            })
            .at(cursor.span_from(start));
            continue;
        };

        expression = ExpressionKind::Call(Call {
            callee: Box::new(expression),
            argument: Some(Box::new(unwrap_arguments(
                params,
                return_type_annotation,
                body,
                cursor.span_from(start),
            )?)),
        })
        .at(cursor.span_from(start));
    }

    Ok(expression)
}

pub fn parse_loop(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    if cursor.first().kind != TokenKind::Keyword(Keyword::Loop) {
        return parse_while(cursor, context);
    }

    cursor.bump()?; // Consume the loop

    let body = fat_arrow_expr_or_block_expr(cursor, context)?;

    Ok(ExpressionKind::Loop(Box::new(body)).at(cursor.span_from(start)))
}

pub fn parse_while(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    if cursor.first().kind != TokenKind::Keyword(Keyword::While) {
        return parse_for(cursor, context);
    }

    cursor.bump()?; // Consume the while

    let condition = parse_expression(cursor, context)?;

    let body = fat_arrow_expr_or_block_expr(cursor, context)?;

    if cursor.first().kind != TokenKind::Keyword(Keyword::Else) {
        return Ok(ExpressionKind::While(While {
            condition: Box::new(condition),
            body: Box::new(body),
            else_body: None,
        })
        .at(cursor.span_from(start)));
    }

    cursor.bump()?; // Consume the else

    let else_body = fat_arrow_expr_or_block_expr(cursor, context)?;

    Ok(ExpressionKind::While(While {
        condition: Box::new(condition),
        body: Box::new(body),
        else_body: Some(Box::new(else_body)),
    })
    .at(cursor.span_from(start)))
}

pub fn parse_for(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    if cursor.first().kind != TokenKind::Keyword(Keyword::For) {
        return parse_type_literal(cursor, context);
    }

    cursor.bump()?; // Consume the for

    let pattern = parse_pattern(cursor)?;

    cursor.expect(TokenKind::Keyword(Keyword::In))?;

    let iterable = parse_expression(cursor, context)?;
    let body = fat_arrow_expr_or_block_expr(cursor, context)?;

    if cursor.first().kind != TokenKind::Keyword(Keyword::Else) {
        return Ok(ExpressionKind::For(For {
            pattern,
            iterable: Box::new(iterable),
            body: Box::new(body),
            else_body: None,
        })
        .at(cursor.span_from(start)));
    }

    cursor.bump()?; // Consume the else

    let else_block = fat_arrow_expr_or_block_expr(cursor, context)?;

    Ok(ExpressionKind::For(For {
        pattern,
        iterable: Box::new(iterable),
        body: Box::new(body),
        else_body: Some(Box::new(else_block)),
    })
    .at(cursor.span_from(start)))
}

fn parse_type_literal(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Expression, Diagnostic> {
    let start = cursor.span();

    let TokenKind::Identifier(identifier) = cursor.first().kind else {
        return parse_range(cursor, context);
    };

    if identifier.validate_type_identifier_name().is_err() {
        return parse_range(cursor, context);
    }

    match (cursor.second().kind, cursor.third().kind) {
        (kind, _)
            if !matches!(
                kind,
                TokenKind::OpenBrace | TokenKind::DoubleColon | TokenKind::Less
            ) =>
        {
            return parse_range(cursor, context);
        }
        // `Foo::<Int> { .. }` constructs a generic struct; the same prefix
        // without a brace is something else, so look past the type arguments
        // to decide.
        (TokenKind::DoubleColon, TokenKind::Less) => {
            let mut lookahead = cursor.clone();

            let constructs_a_literal = parse_type_annotation(&mut lookahead, false)
                .is_ok_and(|_| lookahead.first().kind == TokenKind::OpenBrace);

            if !constructs_a_literal {
                return parse_range(cursor, context);
            }
        }
        (_, TokenKind::Less) => {
            return parse_range(cursor, context);
        }
        // `E::foo` is a static member access, parsed further down. `E::First`
        // names a variant, so it is an enum literal and belongs here.
        (TokenKind::DoubleColon, TokenKind::Identifier(identifier))
            if identifier.validate_type_identifier_name().is_err() =>
        {
            return parse_range(cursor, context);
        }
        _ => {}
    }

    let type_annotation = parse_type_annotation(cursor, false)?;

    let literal = if type_annotation.has_double_colon() {
        parse_enum_literal(cursor, type_annotation, context, start)?
    } else {
        parse_struct_literal(cursor, type_annotation, context, start)?
    };

    // A literal is a value like any other, so what follows it applies to it —
    // except a call, since a literal is not a function.
    parse_postfix(literal, cursor, context, false)
}

fn parse_struct_literal(
    cursor: &mut Cursor,
    type_annotation: TypeAnnotation,
    context: &ParseContext,
    start: Span,
) -> Result<Expression, Diagnostic> {
    if cursor.first().kind != TokenKind::OpenBrace {
        return Ok(ExpressionKind::Literal(ValueLiteral::Struct {
            type_annotation,
            field_initializers: vec![],
        })
        .at(cursor.span_from(start)));
    }

    cursor.bump()?; // Consume the {
    let field_initializers = parse_field_initializers(cursor, context)?;
    cursor.bump()?; // Consume the }

    Ok(ExpressionKind::Literal(ValueLiteral::Struct {
        type_annotation,
        field_initializers,
    })
    .at(cursor.span_from(start)))
}

pub fn parse_field_initializers(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Vec<FieldInitializer>, Diagnostic> {
    let mut field_initializers = vec![];
    let mut has_comma = true;

    while cursor.first().kind != TokenKind::CloseBrace {
        if !has_comma {
            return Err(Diagnostic::error(format!(
                "Expected , but found {:?}",
                cursor.first().kind
            ))
            .at(cursor.first().span));
        }

        has_comma = true;
        field_initializers.push(parse_field_initializer(cursor, context)?);

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        } else {
            has_comma = false;
        }
    }

    Ok(field_initializers)
}

fn parse_field_initializer(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<FieldInitializer, Diagnostic> {
    let TokenKind::Identifier(identifier) = cursor.first().kind else {
        return Err(Diagnostic::error(format!(
            "Expected identifier but found {:?}",
            cursor.first().kind
        ))
        .at(cursor.first().span));
    };

    let TokenKind::Colon = cursor.second().kind else {
        return Err(
            Diagnostic::error(format!("Expected : but found {:?}", cursor.first().kind))
                .at(cursor.first().span),
        );
    };

    cursor.bump()?; // Consume the identifier
    cursor.bump()?; // Consume the :

    let initializer = parse_expression(cursor, context)?;

    Ok(FieldInitializer {
        identifier,
        initializer,
    })
}

fn parse_enum_literal(
    cursor: &mut Cursor,
    type_annotation: TypeAnnotation,
    context: &ParseContext,
    start: Span,
) -> Result<Expression, Diagnostic> {
    if cursor.first().kind != TokenKind::OpenBrace {
        return Ok(ExpressionKind::Literal(ValueLiteral::Enum {
            type_annotation: type_annotation.clone(),
            member: type_annotation
                .to_key()
                .split("::")
                .last()
                .unwrap()
                .to_string(),
            field_initializers: vec![],
        })
        .at(cursor.span_from(start)));
    }

    cursor.expect(TokenKind::OpenBrace)?;
    let field_initializers = parse_field_initializers(cursor, context)?;
    cursor.expect(TokenKind::CloseBrace)?;

    Ok(ExpressionKind::Literal(ValueLiteral::Enum {
        type_annotation: type_annotation.clone(),
        member: type_annotation
            .to_key()
            .split("::")
            .last()
            .unwrap()
            .to_string(),
        field_initializers,
    })
    .at(cursor.span_from(start)))
}

fn parse_range(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    let mut expression = parse_assignment(cursor, context)?;

    if context.is_index {
        return Ok(expression);
    }

    while cursor.first().kind == TokenKind::DoubleDot {
        let operator = cursor.bump()?.kind; // Consume the ..

        let inclusive = cursor.first().kind == TokenKind::Equal;

        if inclusive {
            cursor.bump()?; // Consume the =
        }

        let right = parse_assignment(cursor, context)?;

        expression = ExpressionKind::Binary(Binary {
            left: Box::new(expression),
            right: Box::new(right),
            operator: match (&operator, inclusive) {
                (TokenKind::DoubleDot, false) => BinaryOperator::Range,
                (TokenKind::DoubleDot, true) => BinaryOperator::RangeInclusive,
                _ => unreachable!("Expected .. but found {:?}", operator),
            },
        })
        .at(cursor.span_from(start));
    }

    Ok(expression)
}

fn parse_assignment(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    let mut expression = parse_compound_assignment(cursor, context)?;

    while matches!(cursor.first().kind, TokenKind::Equal) {
        cursor.bump()?; // Consume the =
        let initializer = parse_expression(cursor, context)?;

        let ExpressionKind::Member(member) = expression.kind else {
            return Err(
                Diagnostic::error(format!("Expected member but found {:?}", expression))
                    .at(cursor.first().span),
            );
        };

        expression = ExpressionKind::Assignment(Assignment {
            member: Box::new(member),
            initializer: Box::new(initializer),
        })
        .at(cursor.span_from(start));
    }

    Ok(expression)
}

fn parse_compound_assignment(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    let mut expression = parse_closure(cursor, context)?;

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
        let initializer = parse_expression(cursor, context)?;

        let ExpressionKind::Member(ref member) = expression.kind else {
            return Err(
                Diagnostic::error(format!("Expected member but found {:?}", expression))
                    .at(cursor.first().span),
            );
        };

        expression = ExpressionKind::Assignment(Assignment {
            member: Box::new(member.clone()),
            initializer: Box::new(
                ExpressionKind::Binary(Binary {
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
                })
                .at(cursor.span_from(start)),
            ),
        })
        .at(cursor.span_from(start));
    }

    Ok(expression)
}

fn parse_closure(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    if cursor.first().kind != TokenKind::Pipe {
        return parse_match(cursor, context);
    }

    cursor.bump()?; // Consume the |

    let params = parse_comma_separated_closure_params(cursor)?;

    cursor.bump()?; // Consume the |

    let return_type_annotation = parse_optional_type_annotation(cursor, true)?;

    cursor.optional_bump(TokenKind::FatArrow)?;

    let body = parse_expression(cursor, context)?;

    unwrap_arguments(
        params,
        return_type_annotation,
        body,
        cursor.span_from(start),
    )
}

fn parse_comma_separated_closure_params(
    cursor: &mut Cursor,
) -> Result<Vec<ClosureParameter>, Diagnostic> {
    let mut params = vec![];

    while cursor.first().kind != TokenKind::Pipe {
        let TokenKind::Identifier(identifier) = cursor.first().kind else {
            return Err(Diagnostic::error(format!(
                "Expected identifier but found {:?}",
                cursor.first().kind
            ))
            .at(cursor.first().span));
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

/// Rewrites a multi-parameter closure into nested single-parameter ones.
///
/// Every closure this builds is given `span`, the span of the closure as it was
/// actually written: none of them exist in the source on their own.
fn unwrap_arguments(
    params: Vec<ClosureParameter>,
    return_type_annotation: Option<TypeAnnotation>,
    body: Expression,
    span: Span,
) -> Result<Expression, Diagnostic> {
    match params.first().cloned() {
        None => Ok(ExpressionKind::Closure(Closure {
            param: None,
            return_type_annotation,
            body: Box::new(body),
        })
        .at(span)),
        Some(first) => {
            let (new_body, new_return_type_annotation) = unwrap_arguments_recurse(
                params.into_iter().skip(1).collect(),
                return_type_annotation,
                body,
                span,
            )?;

            Ok(ExpressionKind::Closure(Closure {
                param: Some(first),
                return_type_annotation: new_return_type_annotation,
                body: Box::new(new_body),
            })
            .at(span))
        }
    }
}

fn unwrap_arguments_recurse(
    params: Vec<ClosureParameter>,
    return_type_annotation: Option<TypeAnnotation>,
    body: Expression,
    span: Span,
) -> Result<(Expression, Option<TypeAnnotation>), Diagnostic> {
    match params.last().cloned() {
        None => Ok((body, return_type_annotation)),
        Some(last) => {
            let new_body = ExpressionKind::Closure(Closure {
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
                new_body.at(span),
                span,
            )
        }
    }
}

fn parse_match(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    let expression = parse_boolean_logical(cursor, context)?;

    if cursor.first().kind != TokenKind::Keyword(Keyword::Match) {
        return Ok(expression);
    }

    cursor.bump()?; // Consume the match

    let mut arms = vec![];

    cursor.optional_bump(TokenKind::Pipe)?;

    loop {
        let pattern = parse_pattern(cursor)?;
        cursor.expect(TokenKind::FatArrow)?; // Consume the =>
        let body = parse_expression(cursor, context)?;

        arms.push(MatchArm {
            pattern,
            expression: Box::new(body),
        });

        cursor.optional_bump(TokenKind::Comma)?;

        if cursor.first().kind != TokenKind::Pipe {
            break;
        }

        cursor.bump()?; // consume the |
    }

    Ok(ExpressionKind::Match(Match {
        expression: Box::new(expression),
        arms,
    })
    .at(cursor.span_from(start)))
}

fn parse_boolean_logical(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    let mut expression = parse_comparison(cursor, context)?;

    while matches!(
        (cursor.first().kind, cursor.second().kind),
        (TokenKind::DoubleAmpersand, _) | (TokenKind::Pipe, TokenKind::Pipe)
    ) {
        let operator = cursor.bump()?.kind; // Consume the && or |

        if matches!(operator, TokenKind::Pipe) {
            cursor.bump()?; // Consume the second |
        }

        let right = parse_comparison(cursor, context)?;

        expression = ExpressionKind::Binary(Binary {
            left: Box::new(expression),
            right: Box::new(right),
            operator: match operator {
                TokenKind::DoubleAmpersand => BinaryOperator::LogicalAnd,
                TokenKind::Pipe => BinaryOperator::LogicalOr,
                _ => unreachable!("Expected && or || but found {:?}", operator),
            },
        })
        .at(cursor.span_from(start));
    }

    Ok(expression)
}

fn parse_comparison(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    let mut expression = parse_bitwise_logical(cursor, context)?;

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
        let right = parse_additive(cursor, context)?;

        expression = ExpressionKind::Binary(Binary {
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
        })
        .at(cursor.span_from(start));
    }

    Ok(expression)
}

fn parse_bitwise_logical(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    let mut expression = parse_additive(cursor, context)?;

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

        let right = parse_boolean_logical(cursor, context)?;

        expression = ExpressionKind::Binary(Binary {
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
        })
        .at(cursor.span_from(start));
    }

    Ok(expression)
}

fn parse_additive(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    let mut expression = parse_multiplicative(cursor, context)?;

    while matches!(cursor.first().kind, TokenKind::Plus | TokenKind::Minus) {
        let operator = cursor.bump()?.kind; // Consume the + or -
        let right = parse_multiplicative(cursor, context)?;

        expression = ExpressionKind::Binary(Binary {
            left: Box::new(expression),
            right: Box::new(right),
            operator: match operator {
                TokenKind::Plus => BinaryOperator::Add,
                TokenKind::Minus => BinaryOperator::Subtract,
                _ => unreachable!("Expected + or - but found {:?}", operator),
            },
        })
        .at(cursor.span_from(start));
    }

    Ok(expression)
}

fn parse_multiplicative(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    let mut expression = parse_variable_declaration(cursor, context)?;

    while matches!(
        cursor.first().kind,
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent
    ) {
        let operator = cursor.bump()?.kind; // Consume the *, /, or %
        let right = parse_variable_declaration(cursor, context)?;

        expression = ExpressionKind::Binary(Binary {
            left: Box::new(expression),
            right: Box::new(right),
            operator: match operator {
                TokenKind::Star => BinaryOperator::Multiply,
                TokenKind::Slash => BinaryOperator::Divide,
                TokenKind::Percent => BinaryOperator::Modulo,
                _ => unreachable!("Expected *, /, or % but found {:?}", operator),
            },
        })
        .at(cursor.span_from(start));
    }

    Ok(expression)
}

fn parse_variable_declaration(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    if cursor.first().kind != TokenKind::Keyword(Keyword::Let) {
        return parse_if(cursor, context);
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

            let initializer = parse_expression(cursor, context)?;

            Ok(ExpressionKind::VariableDeclaration(VariableDeclaration {
                mutable,
                type_annotation,
                pattern,
                initializer: Some(Box::new(initializer)),
            })
            .at(cursor.span_from(start)))
        }
        TokenKind::Semicolon => Ok(ExpressionKind::VariableDeclaration(VariableDeclaration {
            mutable,
            type_annotation,
            pattern,
            initializer: None,
        })
        .at(cursor.span_from(start))),
        _ => Err(Diagnostic::error(format!(
            "Expected = or ; but found {:?}",
            cursor.first().kind
        ))
        .at(cursor.first().span)),
    }
}

fn parse_if(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    if cursor.first().kind != TokenKind::Keyword(Keyword::If) {
        return parse_unary(cursor, context);
    }

    cursor.bump()?; // Consume the if

    let if_condition = parse_expression(cursor, context)?;
    let if_block = fat_arrow_expr_or_block_expr(cursor, context)?;

    let mut r#else = None;

    if cursor.first().kind == TokenKind::Keyword(Keyword::Else) {
        cursor.bump()?; // Consume the else

        if cursor.first().kind == TokenKind::Keyword(Keyword::If) {
            r#else = Some(Box::new(parse_if(cursor, context)?));
        } else {
            let else_expr = fat_arrow_expr_or_block_expr(cursor, context)?;
            r#else = Some(Box::new(else_expr));
        }
    }

    Ok(ExpressionKind::If(If {
        condition: Box::new(if_condition),
        true_expression: Box::new(if_block),
        false_expression: r#else,
    })
    .at(cursor.span_from(start)))
}

fn parse_unary(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    if matches!(
        cursor.first().kind,
        TokenKind::Plus | TokenKind::Minus | TokenKind::Bang | TokenKind::Tilde
    ) {
        let operator = cursor.bump()?.kind; // Consume the +, -, !, or ~
        let right = parse_unary(cursor, context)?;

        if matches!(operator, TokenKind::Minus)
            && matches!(
                &right.kind,
                ExpressionKind::Literal(ValueLiteral::Int(_))
                    | ExpressionKind::Literal(ValueLiteral::Float(_))
            )
        {
            match right.kind {
                ExpressionKind::Literal(ValueLiteral::Int(value)) => {
                    return Ok(ExpressionKind::Literal(ValueLiteral::Int(-value))
                        .at(cursor.span_from(start)));
                }
                ExpressionKind::Literal(ValueLiteral::Float(value)) => {
                    return Ok(ExpressionKind::Literal(ValueLiteral::Float(-value))
                        .at(cursor.span_from(start)));
                }
                _ => unreachable!("Checked in previous if"),
            }
        }

        if matches!(operator, TokenKind::Plus)
            && matches!(
                &right.kind,
                ExpressionKind::Literal(ValueLiteral::Int(_))
                    | ExpressionKind::Literal(ValueLiteral::UInt(_))
                    | ExpressionKind::Literal(ValueLiteral::Float(_))
            )
        {
            match right.kind {
                ExpressionKind::Literal(ValueLiteral::Int(value)) => {
                    return Ok(ExpressionKind::Literal(ValueLiteral::Int(value))
                        .at(cursor.span_from(start)));
                }
                ExpressionKind::Literal(ValueLiteral::UInt(value)) => {
                    return Ok(ExpressionKind::Literal(ValueLiteral::UInt(value))
                        .at(cursor.span_from(start)));
                }
                ExpressionKind::Literal(ValueLiteral::Float(value)) => {
                    return Ok(ExpressionKind::Literal(ValueLiteral::Float(value))
                        .at(cursor.span_from(start)));
                }
                _ => unreachable!("Checked in previous if"),
            }
        }

        return Ok(ExpressionKind::Unary(Unary {
            operator: match operator {
                TokenKind::Plus => UnaryOperator::Identity,
                TokenKind::Minus => UnaryOperator::Negate,
                TokenKind::Bang => UnaryOperator::LogicalNot,
                TokenKind::Tilde => UnaryOperator::BitwiseNot,
                _ => unreachable!("Expected +, -, !, or ~ but found {:?}", operator),
            },
            expression: Box::new(right),
        })
        .at(cursor.span_from(start)));
    }

    parse_call_or_param_propagation(cursor, context)
}

fn parse_call_or_param_propagation(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    let mut expression = parse_member_access(cursor, context)?;

    if cursor.first().kind == TokenKind::DoubleColon && cursor.second().kind == TokenKind::Less {
        cursor.bump()?; // Consume the ::
        cursor.bump()?; // Consume the <
        let generics = parse_generics_in_type_name(cursor)?;
        cursor.bump()?; // Consume the >

        let ExpressionKind::Member(member) = expression.kind.clone() else {
            return Err(
                Diagnostic::error(format!("Expected member but found {:?}", expression))
                    .at(cursor.first().span),
            );
        };

        expression =
            ExpressionKind::Member(member.with_generics(generics)).at(cursor.span_from(start));
    }

    parse_postfix(expression, cursor, context, true)
}

/// Consumes whatever follows an expression and binds tighter than any operator:
/// calls, field access, indexing and param propagation, in any order.
///
/// `allow_call` is false after something that cannot be called, such as a
/// struct literal. Without that, a literal ending a line would take a
/// parenthesised expression on the next line as its argument list.
fn parse_postfix(
    mut expression: Expression,
    cursor: &mut Cursor,
    context: &ParseContext,
    allow_call: bool,
) -> Result<Expression, Diagnostic> {
    let start = expression.span;
    loop {
        match cursor.first().kind {
            // Call expression
            TokenKind::OpenParen if allow_call => {
                expression = parse_call_expression(expression, cursor, context)?;
            }
            // Field access
            TokenKind::Dot => {
                expression = parse_field_access(expression, cursor, context)?;
            }
            // Param propagation
            TokenKind::Colon => {
                cursor.bump()?; // Consume the :

                if cursor.first().kind == TokenKind::OpenBracket {
                    cursor.bump()?; // Consume the [

                    let index = parse_index(cursor, context)?;
                    cursor.expect(TokenKind::CloseBracket)?;

                    expression = ExpressionKind::Member(Member::Index {
                        object: Box::new(expression),
                        index,
                    })
                    .at(cursor.span_from(start));
                } else {
                    let TokenKind::Identifier(identifier) = cursor.first().kind else {
                        return Err(Diagnostic::error(format!(
                            "Expected identifier but found {:?}",
                            cursor.first().kind
                        ))
                        .at(cursor.first().span));
                    };

                    let ExpressionKind::Member(member) = parse_literal(cursor, context)?.kind
                    else {
                        return Err(Diagnostic::error(format!(
                            "Expected member but found {:?}",
                            cursor.first().kind
                        ))
                        .at(cursor.first().span));
                    };

                    expression = ExpressionKind::Member(Member::ParamPropagation {
                        object: Box::new(expression),
                        member: Box::new(member),
                        symbol: identifier,
                        generics: None,
                    })
                    .at(cursor.span_from(start));
                }
            }
            _ => break,
        }
    }

    Ok(expression)
}

fn parse_call_expression(
    callee: Expression,
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Expression, Diagnostic> {
    let start = callee.span;
    let arguments = parse_args(cursor, context)?;
    cursor.bump()?; // Consume the )

    let mut call = ExpressionKind::Call(Call {
        callee: Box::new(callee.clone()),
        argument: arguments.first().map(|a| Box::new(a.clone())),
    });

    for arg in arguments.into_iter().skip(1) {
        call = ExpressionKind::Call(Call {
            callee: Box::new(call.at(cursor.span_from(start))),
            argument: Some(Box::new(arg)),
        })
    }

    if let TokenKind::OpenParen = cursor.first().kind {
        call = parse_call_expression(call.at(cursor.span_from(start)), cursor, context)?.kind;
    }

    Ok(call.at(cursor.span_from(start)))
}

fn parse_args(cursor: &mut Cursor, context: &ParseContext) -> Result<Vec<Expression>, Diagnostic> {
    let TokenKind::OpenParen = cursor.bump()?.kind else {
        return Err(
            Diagnostic::error(format!("Expected ( but found {:?}", cursor.first().kind))
                .at(cursor.first().span),
        );
    };

    if let TokenKind::CloseParen = cursor.first().kind {
        Ok(vec![])
    } else {
        parse_args_list(cursor, context)
    }
}

fn parse_args_list(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Vec<Expression>, Diagnostic> {
    let mut args = parse_expression(cursor, context).map(|e| vec![e])?;

    while let TokenKind::Comma = cursor.first().kind {
        cursor.bump()?; // Consume the ,
        let expression = parse_expression(cursor, context)?;
        args.push(expression);
    }

    Ok(args)
}

fn parse_member_access(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    let mut object = parse_literal(cursor, context)?;

    while let TokenKind::DoubleColon = cursor.first().kind {
        let ExpressionKind::Member(Member::Identifier { symbol, generics }) = &object.kind else {
            break;
        };

        if symbol.validate_type_identifier_name().is_err() {
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

        let TokenKind::Identifier(identifier) = cursor.second().kind else {
            return Err(Diagnostic::error(format!(
                "Expected identifier but found {:?}",
                cursor.first().kind
            ))
            .at(cursor.first().span));
        };

        if identifier.validate_function_identifier_name().is_err() {
            return Ok(object);
        }

        cursor.bump()?; // Consume the ::

        let ExpressionKind::Member(member) = parse_literal(cursor, context)?.kind else {
            return Err(Diagnostic::error(format!(
                "Expected member but found {:?}",
                cursor.first().kind
            ))
            .at(cursor.first().span));
        };

        object = ExpressionKind::Member(Member::StaticMemberAccess {
            type_annotation,
            member: Box::new(member),
            symbol: identifier,
            generics: None,
        })
        .at(cursor.span_from(start));
    }

    Ok(object)
}

/// Consumes `.field`, wrapping what came before it.
///
/// Field access is postfix, so it belongs in the same loop as calls, indexing
/// and propagation rather than only applying to what a literal produced.
fn parse_field_access(
    object: Expression,
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Expression, Diagnostic> {
    let start = object.span;
    cursor.bump()?; // Consume the .

    let TokenKind::Identifier(identifier) = cursor.first().kind else {
        return Err(Diagnostic::error(format!(
            "Expected identifier but found {:?}",
            cursor.first().kind
        ))
        .at(cursor.first().span));
    };

    let ExpressionKind::Member(member) = parse_literal(cursor, context)?.kind else {
        return Err(Diagnostic::error(format!(
            "Expected member but found {:?}",
            cursor.first().kind
        ))
        .at(cursor.first().span));
    };

    Ok(ExpressionKind::Member(Member::MemberAccess {
        object: Box::new(object),
        member: Box::new(member),
        symbol: identifier,
        generics: None,
    })
    .at(cursor.span_from(start)))
}

pub fn parse_literal(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Expression, Diagnostic> {
    let start = cursor.span();

    let TokenKind::Literal(literal) = cursor.first().kind else {
        return parse_primary(cursor, context);
    };

    cursor.bump()?; // Consume the literal
    to_expression_literal(literal, cursor.span_from(start))
}

fn parse_primary(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();
    match cursor.first().kind {
        TokenKind::Identifier(identifier) => {
            cursor.bump()?; // Consume the identifier

            Ok(ExpressionKind::Member(Member::Identifier {
                symbol: identifier,
                generics: None,
            })
            .at(cursor.span_from(start)))
        }
        TokenKind::OpenBrace => parse_block(cursor, context),
        TokenKind::OpenParen => {
            cursor.bump()?; // Consume the (

            let context = &ParseContext::default();
            let expression = parse_expression(cursor, context)?;

            match cursor.first().kind {
                TokenKind::CloseParen => {
                    cursor.bump()?; // Consume the )
                    Ok(expression)
                }
                TokenKind::Comma => {
                    let mut elements = vec![expression];

                    while cursor.first().kind == TokenKind::Comma {
                        cursor.bump()?; // Consume the ,

                        elements.push(parse_expression(cursor, context)?);
                    }

                    cursor.expect(TokenKind::CloseParen)?;

                    Ok(ExpressionKind::Tuple(elements).at(cursor.span_from(start)))
                }
                _ => Err(Diagnostic::error(format!(
                    "Expected ) or , but found {:?}",
                    cursor.first().kind
                ))
                .at(cursor.first().span)),
            }
        }
        TokenKind::OpenBracket => {
            cursor.bump()?; // Consume the [

            let mut items = vec![];

            while cursor.first().kind != TokenKind::CloseBracket {
                let mut is_spread = false;

                if cursor.first().kind == TokenKind::DoubleDot {
                    cursor.bump()?; // consume the ..
                    is_spread = true;
                }

                let expression = parse_expression(cursor, context)?;

                if is_spread {
                    items.push(ArrayItem::Spread(expression))
                } else {
                    items.push(ArrayItem::Expression(expression))
                }

                if cursor.first().kind == TokenKind::Comma {
                    cursor.bump()?; // consume the ,
                }
            }

            cursor.expect(TokenKind::CloseBracket)?;

            Ok(ExpressionKind::Literal(ValueLiteral::Array(items)).at(cursor.span_from(start)))

            // let first_expression = parse_expression(cursor, context)?;
            //
            // match cursor.first().kind {
            //     TokenKind::CloseBracket => {
            //         cursor.bump()?; // Consume the ]
            //
            //         Ok(ExpressionKind::Literal(ValueLiteral::Array(vec![
            //             first_expression,
            //         ])))
            //     }
            //     TokenKind::Comma => {
            //         cursor.bump()?; // Consume the ,
            //         let mut elements = vec![first_expression];
            //
            //         while cursor.first().kind != TokenKind::CloseBracket {
            //             elements.push(parse_expression(cursor, context)?);
            //
            //             if cursor.first().kind == TokenKind::Comma {
            //                 cursor.bump()?; // Consume the ,
            //             }
            //         }
            //
            //         cursor.bump()?; // Consume the ]
            //         Ok(ExpressionKind::Literal(ValueLiteral::Array(elements)))
            //     }
            //     // TokenKind::Semicolon => {
            //     //     cursor.bump()?; // Consume the ;
            //     //     let index = parse_index(cursor, context)?;
            //     //
            //     //     cursor.expect(TokenKind::CloseBracket)?;
            //     //     Ok(ExpressionKind::Member(Member::Index {
            //     //         object: Box::new(first_expression),
            //     //         index,
            //     //     }))
            //     // }
            //     _ => Err(format!("Unexpected token {:?}", cursor.first().kind)),
            // }
        }
        _ => Err(Diagnostic::error(format!(
            "Expected primary expression but found {:?}",
            cursor.first().kind
        ))
        .at(cursor.first().span)),
    }
}

pub fn parse_block(cursor: &mut Cursor, context: &ParseContext) -> Result<Expression, Diagnostic> {
    let start = cursor.span();

    parse_block_statements(cursor, context)
        .map(|statements| ExpressionKind::Block(statements).at(cursor.span_from(start)))
}

pub fn parse_block_statements(
    cursor: &mut Cursor,
    context: &ParseContext,
) -> Result<Vec<Statement>, Diagnostic> {
    cursor.expect(TokenKind::OpenBrace)?; // Consume the {

    let mut statements = vec![];

    while cursor.first().kind != TokenKind::CloseBrace {
        statements.push(parse_statement(cursor, context)?);
    }

    cursor.expect(TokenKind::CloseBrace)?; // Consume the }

    Ok(statements)
}

fn parse_index(cursor: &mut Cursor, context: &ParseContext) -> Result<Index, Diagnostic> {
    let mut start = None;

    let mut context = context.clone();
    context.is_index = true;

    if cursor.first().kind != TokenKind::DoubleDot {
        let index = parse_expression(cursor, &context)?;

        if cursor.first().kind != TokenKind::DoubleDot {
            return Ok(Index::Value(Box::new(index)));
        }

        start = Some(Box::new(index));
    }

    cursor.expect(TokenKind::DoubleDot)?;

    if cursor.first().kind == TokenKind::CloseBracket {
        return Ok(Index::Range {
            start,
            end: None,
            inclusive: false,
        });
    }

    let inclusive = if cursor.first().kind == TokenKind::Equal {
        cursor.bump()?; // Consume the =
        true
    } else {
        false
    };

    let end = parse_expression(cursor, &context)?;

    Ok(Index::Range {
        start,
        end: Some(Box::new(end)),
        inclusive,
    })
}

fn parse_pattern(cursor: &mut Cursor) -> Result<Pattern, Diagnostic> {
    let start = cursor.span();
    let pattern = parse_pattern_primary(cursor)?;

    if cursor.first().kind != TokenKind::DoubleDot {
        return Ok(pattern);
    }

    cursor.bump()?; // Consume the ..

    let inclusive = cursor.first().kind == TokenKind::Equal;

    if inclusive {
        cursor.bump()?; // Consume the =
    }

    // Ranges don't nest: the endpoints are primaries, so `1..2..3` is an error
    // rather than something with a made-up meaning.
    let upper = parse_pattern_primary(cursor)?;

    Ok(PatternKind::Range {
        lower: pattern_into_bound(pattern)?,
        upper: pattern_into_bound(upper)?,
        inclusive,
    }
    .at(cursor.span_from(start)))
}

fn parse_pattern_primary(cursor: &mut Cursor) -> Result<Pattern, Diagnostic> {
    let start = cursor.span();
    match cursor.first().kind {
        TokenKind::Underscore => {
            cursor.bump()?; // Consume the _
            Ok(PatternKind::Wildcard.at(cursor.span_from(start)))
        }
        TokenKind::Literal(token::Literal::Unit) => {
            cursor.bump()?; // Consume the unit
            Ok(PatternKind::Unit.at(cursor.span_from(start)))
        }
        TokenKind::Literal(token::Literal::Bool(v)) => {
            cursor.bump()?; // Consume the literal
            Ok(PatternKind::Bool(v).at(cursor.span_from(start)))
        }
        TokenKind::Literal(token::Literal::Int(v)) => {
            cursor.bump()?; // Consume the literal
            Ok(PatternKind::Int(v.value).at(cursor.span_from(start)))
        }
        TokenKind::Literal(token::Literal::UInt(v)) => {
            cursor.bump()?; // Consume the literal
            Ok(PatternKind::UInt(v.value).at(cursor.span_from(start)))
        }
        TokenKind::Literal(token::Literal::Float(v)) => {
            cursor.bump()?; // Consume the literal
            Ok(PatternKind::Float(v).at(cursor.span_from(start)))
        }
        TokenKind::Literal(token::Literal::Rune(v)) => {
            cursor.bump()?; // Consume the literal
            Ok(PatternKind::Rune(parse_rune(&v)?).at(cursor.span_from(start)))
        }
        TokenKind::Literal(token::Literal::String(v)) => {
            cursor.bump()?; // Consume the literal
            Ok(PatternKind::String(v).at(cursor.span_from(start)))
        }
        TokenKind::Less => parse_comparison_pattern(cursor, ComparisonOperator::LessThan),
        TokenKind::Greater => parse_comparison_pattern(cursor, ComparisonOperator::GreaterThan),
        TokenKind::LessEqual => {
            parse_comparison_pattern(cursor, ComparisonOperator::LessThanOrEqual)
        }
        TokenKind::GreaterEqual => {
            parse_comparison_pattern(cursor, ComparisonOperator::GreaterThanOrEqual)
        }
        // `::First`, or `::E2::S3` down through nested enums. The path is
        // resolved against the matched value's type.
        TokenKind::DoubleColon => {
            let mut path = vec![];

            while cursor.first().kind == TokenKind::DoubleColon {
                let TokenKind::Identifier(variant) = cursor.second().kind else {
                    return Err(Diagnostic::error(format!(
                        "Expected a variant name after :: but found {:?}",
                        cursor.second().kind
                    ))
                    .at(cursor.first().span));
                };

                variant.validate_type_identifier_name()?;

                cursor.bump()?; // Consume the ::
                cursor.bump()?; // Consume the variant name

                path.push(variant);
            }

            let inner = parse_variant_tail(cursor, &path)?;

            Ok(PatternKind::EnumVariant {
                enum_annotation: None,
                path,
                inner,
            }
            .at(cursor.span_from(start)))
        }
        // `MyEnum::First { .. }` or `Point { .. }`. A `::` anywhere in the name
        // makes it a variant; without one it is always a struct.
        TokenKind::Identifier(identifier) if identifier.validate_type_identifier_name().is_ok() => {
            let type_annotation = parse_type_annotation(cursor, false)?;

            match split_variant_annotation(&type_annotation) {
                Some((enum_annotation, path)) => {
                    let inner = parse_variant_tail(cursor, &path)?;

                    Ok(PatternKind::EnumVariant {
                        enum_annotation: Some(enum_annotation),
                        path,
                        inner,
                    }
                    .at(cursor.span_from(start)))
                }
                None => Ok(PatternKind::Struct {
                    type_annotation: Some(type_annotation),
                    fields: parse_optional_field_patterns(cursor)?,
                }
                .at(cursor.span_from(start))),
            }
        }
        TokenKind::Identifier(identifier)
            if identifier.validate_variable_identifier_name().is_ok() =>
        {
            cursor.bump()?; // Consume the identifier
            parse_optional_bound_pattern(cursor, identifier, start)
        }
        // `{ x, y: 1 }` — the type comes from the matched value.
        TokenKind::OpenBrace => Ok(PatternKind::Struct {
            type_annotation: None,
            fields: parse_field_patterns(cursor)?,
        }
        .at(cursor.span_from(start))),
        TokenKind::OpenParen => {
            cursor.bump()?; // Consume the (

            let mut patterns = vec![];

            while cursor.first().kind != TokenKind::CloseParen {
                patterns.push(parse_pattern(cursor)?);

                if cursor.first().kind == TokenKind::Comma {
                    cursor.bump()?; // Consume the ,
                }
            }

            cursor.expect(TokenKind::CloseParen)?; // Consume the )
            Ok(PatternKind::Tuple(patterns).at(cursor.span_from(start)))
        }
        _ => Err(Diagnostic::error(format!(
            "Unknown start of pattern: {:?}",
            cursor.first().kind
        ))
        .at(cursor.first().span)),
    }
}

fn parse_comparison_pattern(
    cursor: &mut Cursor,
    operator: ComparisonOperator,
) -> Result<Pattern, Diagnostic> {
    let start = cursor.span();
    cursor.bump()?; // Consume the operator

    Ok(PatternKind::Comparison {
        operator,
        bound: parse_bound(cursor)?,
    }
    .at(cursor.span_from(start)))
}

/// A comparison endpoint is a numeric or rune literal, or a variable holding
/// one. Restricting it here keeps the type checker from having to reject
/// nonsense like `< { x: 1 }` after the fact.
fn parse_bound(cursor: &mut Cursor) -> Result<Bound, Diagnostic> {
    let bound = match cursor.first().kind {
        TokenKind::Literal(token::Literal::Int(v)) => Bound::Int(v.value),
        TokenKind::Literal(token::Literal::UInt(v)) => Bound::UInt(v.value),
        TokenKind::Literal(token::Literal::Float(v)) => Bound::Float(v),
        TokenKind::Literal(token::Literal::Rune(v)) => Bound::Rune(parse_rune(&v)?),
        TokenKind::Identifier(identifier)
            if identifier.validate_variable_identifier_name().is_ok() =>
        {
            Bound::Variable(identifier)
        }
        kind => {
            return Err(Diagnostic::error(format!(
                "Expected a number, rune or variable but found {:?}",
                kind
            ))
            .at(cursor.first().span))
        }
    };

    cursor.bump()?; // Consume the bound
    Ok(bound)
}

fn pattern_into_bound(pattern: Pattern) -> Result<Bound, Diagnostic> {
    let span = pattern.span;

    match pattern.kind {
        PatternKind::Int(v) => Ok(Bound::Int(v)),
        PatternKind::UInt(v) => Ok(Bound::UInt(v)),
        PatternKind::Float(v) => Ok(Bound::Float(v)),
        PatternKind::Rune(v) => Ok(Bound::Rune(v)),
        PatternKind::Binding(v) => Ok(Bound::Variable(v)),
        other => Err(Diagnostic::error(format!(
            "Range endpoints must be numbers, runes or variables, found {}",
            other.at(span)
        ))
        .at(span)),
    }
}

fn parse_optional_field_patterns(cursor: &mut Cursor) -> Result<Vec<FieldPattern>, Diagnostic> {
    if cursor.first().kind != TokenKind::OpenBrace {
        return Ok(vec![]);
    }

    parse_field_patterns(cursor)
}

fn parse_field_patterns(cursor: &mut Cursor) -> Result<Vec<FieldPattern>, Diagnostic> {
    let start = cursor.span();
    cursor.expect(TokenKind::OpenBrace)?; // Consume the {

    let mut fields = vec![];

    while cursor.first().kind != TokenKind::CloseBrace {
        let TokenKind::Identifier(identifier) = cursor.first().kind else {
            return Err(Diagnostic::error(format!(
                "Expected a field name but found {:?}",
                cursor.first().kind
            ))
            .at(cursor.first().span));
        };

        cursor.bump()?; // Consume the field name

        // `{ x }` is shorthand for `{ x: x }`.
        let pattern = if cursor.first().kind == TokenKind::Colon {
            cursor.bump()?; // Consume the :
            parse_pattern(cursor)?
        } else {
            PatternKind::Binding(identifier.clone()).at(cursor.span_from(start))
        };

        fields.push(FieldPattern {
            identifier,
            pattern,
        });

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        }
    }

    cursor.expect(TokenKind::CloseBrace)?; // Consume the }
    Ok(fields)
}

/// Splits `MyEnum::First` or `E1::E2::S3` into the enum being named and the
/// path of variants below it. Returns `None` for a plain type name, which is
/// therefore a struct.
///
/// The split is at the *first* `::`: the leading segment names the enum, and
/// everything after it is the path down through its variants.
fn split_variant_annotation(
    type_annotation: &TypeAnnotation,
) -> Option<(TypeAnnotation, Vec<String>)> {
    let (name, generics) = match type_annotation {
        TypeAnnotation::Type(name) => (name, None),
        TypeAnnotation::ConcreteType(name, generics) => (name, Some(generics.clone())),
        _ => return None,
    };

    let (enum_name, path) = name.split_once("::")?;

    let enum_annotation = match generics {
        Some(generics) => TypeAnnotation::ConcreteType(enum_name.to_owned(), generics),
        None => TypeAnnotation::Type(enum_name.to_owned()),
    };

    Some((
        enum_annotation,
        path.split("::").map(|s| s.to_owned()).collect(),
    ))
}

/// What follows a variant path: a binding, or field patterns, but never both.
///
/// Binding the variant and looking into its fields are alternatives — once the
/// value is bound its fields are reachable through it, so allowing both side by
/// side would be two spellings of one thing. `@` is how to ask for both.
fn parse_variant_tail(
    cursor: &mut Cursor,
    path: &[String],
) -> Result<Option<Box<Pattern>>, Diagnostic> {
    let start = cursor.span();
    if let TokenKind::Identifier(binding) = cursor.first().kind {
        if binding.validate_variable_identifier_name().is_ok() {
            cursor.bump()?; // Consume the binding

            let bound = parse_optional_bound_pattern(cursor, binding.clone(), start)?;

            if cursor.first().kind == TokenKind::OpenBrace {
                return Err(Diagnostic::error(format!(
                    "Pattern `::{}` binds `{}` and destructures its fields; a variant pattern may do one or the other, not both — write `{} @ {{ .. }}` to do both",
                    path.join("::"),
                    binding,
                    binding
                )).at(cursor.first().span));
            }

            return Ok(Some(Box::new(bound)));
        }
    }

    if cursor.first().kind != TokenKind::OpenBrace {
        return Ok(None);
    }

    Ok(Some(Box::new(
        PatternKind::Struct {
            type_annotation: None,
            fields: parse_field_patterns(cursor)?,
        }
        .at(cursor.span_from(start)),
    )))
}

/// A binding, plus the `@ p` that may follow it.
///
/// `@` is the only way to both bind a value and look inside it; without it a
/// binding stands alone.
fn parse_optional_bound_pattern(
    cursor: &mut Cursor,
    identifier: String,
    start: Span,
) -> Result<Pattern, Diagnostic> {
    if cursor.first().kind != TokenKind::At {
        return Ok(PatternKind::Binding(identifier).at(cursor.span_from(start)));
    }

    cursor.bump()?; // Consume the @

    let pattern = parse_pattern(cursor)?;

    if let PatternKind::Binding(inner) = &pattern.kind {
        return Err(Diagnostic::error(format!(
            "Pattern `{} @ {}` binds the same value twice; the right of `@` constrains what was bound",
            identifier, inner
        )).at(cursor.first().span));
    }

    Ok(PatternKind::Bound {
        identifier,
        pattern: Box::new(pattern),
    }
    .at(cursor.span_from(start)))
}

fn parse_rune(literal: &str) -> Result<char, Diagnostic> {
    literal
        .parse::<char>()
        .map_err(|_| Diagnostic::error(format!("Invalid rune literal: {}", literal)))
}

fn to_expression_literal(literal: token::Literal, span: Span) -> Result<Expression, Diagnostic> {
    match literal {
        token::Literal::Unit => Ok(ExpressionKind::Literal(ValueLiteral::Unit).at(span)),
        token::Literal::Int(literal) => {
            Ok(ExpressionKind::Literal(ValueLiteral::Int(literal.value)).at(span))
        }
        token::Literal::UInt(literal) => {
            Ok(ExpressionKind::Literal(ValueLiteral::UInt(literal.value)).at(span))
        }
        token::Literal::Float(value) => {
            Ok(ExpressionKind::Literal(ValueLiteral::Float(value)).at(span))
        }
        token::Literal::String(value) => {
            Ok(ExpressionKind::Literal(ValueLiteral::String(value)).at(span))
        }
        token::Literal::Rune(value) => Ok(ExpressionKind::Literal(ValueLiteral::Rune(
            value.parse::<char>().expect("Failed to parse rune literal"),
        ))
        .at(span)),
        token::Literal::Bool(value) => {
            Ok(ExpressionKind::Literal(ValueLiteral::Bool(value)).at(span))
        }
    }
}
