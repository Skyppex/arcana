use std::collections::HashMap;

use crate::lexer::token::{TokenKind, Keyword, self};

use super::{cursor::Cursor,
    Expression,
    statements::parse_statement,
    ConditionBlock,
    FieldInitializer,
    Literal,
    Member,
    BinaryOperator,
    UnaryOperator,
    VariableDeclaration,
    If,
    Assignment,
    Call,
    Unary,
    Binary,
    Ternary, UnionMemberFieldInitializers
};



pub fn parse_expression(cursor: &mut Cursor) -> Result<Expression, String> {
    #[cfg(feature = "interpreter")]
    let expression = parse_drop(cursor);

    #[cfg(not(feature = "interpreter"))]
    let expression = parse_block(cursor);

    if let TokenKind::Semicolon = cursor.first().kind {
        cursor.bump()?; // Consume the ;
        return expression;
    }

    return expression;
}

#[cfg(feature = "interpreter")]
fn parse_drop(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Drop) {
        return parse_block(cursor);
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

pub fn parse_block(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::OpenBrace {
        return parse_variable_declaration(cursor);
    }

    cursor.bump()?; // Consume the {

    let mut statements = vec![];

    while cursor.first().kind != TokenKind::CloseBrace {
        statements.push(parse_statement(cursor)?);
    }

    cursor.bump()?; // Consume the }

    Ok(Expression::Block(statements))
}

fn parse_variable_declaration(cursor: &mut Cursor) -> Result<Expression, String> {
    match cursor.first().kind {
        TokenKind::Keyword(Keyword::Mutable) => {
            cursor.bump()?; // Consume the mutable

            let TokenKind::Identifier(type_name) = cursor.bump()?.kind else {
                return Err(format!("Expected type identifier but found {:?}", cursor.first().kind));
            };

            let TokenKind::Identifier(identifier) = cursor.bump()?.kind else {
                return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
            };

            match cursor.first().kind {
                TokenKind::Equal => {
                    cursor.bump()?; // Consume the =

                    let initializer = parse_expression(cursor)?;

                    Ok(Expression::VariableDeclaration(VariableDeclaration {
                        mutable: true,
                        type_name,
                        identifier,
                        initializer: Some(Box::new(initializer)),
                    }))
                },
                TokenKind::Semicolon => {
                    Ok(Expression::VariableDeclaration(VariableDeclaration {
                        mutable: true,
                        type_name,
                        identifier,
                        initializer: None,
                    }))
                },
                _ => Err(format!("Expected = or : but found {:?}", cursor.first().kind)),
            }
        },
        TokenKind::Identifier(type_name) => {
            let TokenKind::Identifier(identifier) = cursor.second().kind else {
                return parse_if(cursor);
            };

            cursor.bump()?; // Consume the type identifier
            cursor.bump()?; // Consume the identifier

            match cursor.first().kind {
                TokenKind::Equal => {
                    cursor.bump()?; // Consume the =

                    let expression = parse_expression(cursor)?;

                    Ok(Expression::VariableDeclaration(VariableDeclaration {
                        mutable: false,
                        type_name,
                        identifier,
                        initializer: Some(Box::new(expression)),
                    }))
                },
                TokenKind::Semicolon => {
                    Ok(Expression::VariableDeclaration(VariableDeclaration {
                        mutable: false,
                        type_name,
                        identifier,
                        initializer: None,
                    }))
                },
                _ => Err(format!("Expected = or ; but found {:?}", cursor.first().kind)),
            }
        },
        _ => parse_if(cursor),
    }
}

fn parse_if(cursor: &mut Cursor) -> Result<Expression, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::If) {
        return parse_struct_literal(cursor);
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

fn parse_struct_literal(cursor: &mut Cursor) -> Result<Expression, String> {
    let TokenKind::Identifier(type_name) = cursor.first().kind else {
        return parse_union_literal(cursor);
    };

    let TokenKind::OpenBrace = cursor.second().kind else {
        return parse_union_literal(cursor);
    };

    cursor.bump()?; // Consume the type identifier
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
        type_name,
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

fn parse_union_literal(cursor: &mut Cursor) -> Result<Expression, String> {
    let TokenKind::Identifier(type_name) = cursor.first().kind else {
        return parse_assignment(cursor);
    };

    let TokenKind::Colon = cursor.second().kind else {
        return parse_assignment(cursor);
    };
    
    let TokenKind::Colon = cursor.third().kind else {
        return parse_assignment(cursor);
    };
    
    let TokenKind::Identifier(member) = cursor.fourth().kind else {
        return parse_assignment(cursor);
    };

    cursor.bump()?; // Consume the type identifier
    cursor.bump()?; // Consume the first :
    cursor.bump()?; // Consume the second :
    cursor.bump()?; // Consume the member identifier

    let field_initializers = {
        if cursor.first().kind == TokenKind::OpenParen {
            cursor.bump()?; // Consume the (
            if matches!(cursor.second().kind, TokenKind::Colon) {
                parse_named_union_member_field_initializers(cursor)?
            } else {
                parse_unnamed_union_member_field_initializers(cursor)?
            }
        } else {
            UnionMemberFieldInitializers::None
        }
    };

    cursor.bump()?; // Consume the ) or }
    Ok(Expression::Literal(Literal::Union {
        type_name,
        member,
        field_initializers
    }))
}

fn parse_unnamed_union_member_field_initializers(cursor: &mut Cursor) -> Result<UnionMemberFieldInitializers, String> {
    let mut field_initializers = vec![];
    
    while cursor.first().kind != TokenKind::CloseParen {
        let initializer = parse_expression(cursor)?;

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        }

        field_initializers.push(initializer);
    }

    Ok(UnionMemberFieldInitializers::Unnamed(field_initializers))
}

fn parse_named_union_member_field_initializers(cursor: &mut Cursor) -> Result<UnionMemberFieldInitializers, String> {
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

    Ok(UnionMemberFieldInitializers::Named(field_initializers))
}

fn parse_assignment(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut expression = parse_ternary(cursor)?;

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
        TokenKind::DoubleGreater
        | TokenKind::DoubleLess
        | TokenKind::Caret
        | TokenKind::Ampersand
        | TokenKind::Pipe) {
        let operator = cursor.bump()?.kind; // Consume the >>, <<, ^, &, or |
        let right = parse_additive(cursor)?;

        expression = Expression::Binary(Binary {
            left: Box::new(expression),
            right: Box::new(right),
            operator: match operator {
                TokenKind::DoubleGreater => BinaryOperator::BitwiseRightShift,
                TokenKind::DoubleLess => BinaryOperator::BitwiseLeftShift,
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
    let mut expression = parse_unary(cursor)?;

    while matches!(cursor.first().kind, TokenKind::Star | TokenKind::Slash | TokenKind::Percent) {
        let operator = cursor.bump()?.kind; // Consume the *, /, or %
        let right = parse_unary(cursor)?;

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

    if let TokenKind::OpenParen = cursor.first().kind {
        return parse_call_expression(member, cursor);
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

fn parse_member_access(cursor: &mut Cursor) -> Result<Expression, String> {
    let mut object = parse_primary(cursor)?;

    while let TokenKind::Dot = cursor.first().kind {
        cursor.bump()?; // Consume the .

        let TokenKind::Identifier(identifier) = cursor.first().kind else {
            return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
        };

        let Expression::Member(member) = parse_primary(cursor)? else {
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

fn parse_primary(cursor: &mut Cursor) -> Result<Expression, String> {
    match cursor.first().kind {
        TokenKind::Identifier(identifier) => {
            cursor.bump()?; // Consume the identifier
            Ok(Expression::Member(Member::Identifier { symbol: identifier }))
        },
        TokenKind::Literal(literal) => {
            cursor.bump()?; // Consume the literal
            to_expression_literal(literal)
        },
        TokenKind::OpenParen => {
            cursor.bump()?; // Consume the (

            let x = 5;

            let expression = parse_expression(cursor)?;

            match cursor.first().kind {
                TokenKind::CloseParen => {
                    cursor.bump()?; // Consume the )
                    Ok(expression)
                },
                _ => Err(format!("Expected ) but found {:?}", cursor.first().kind)),
            }
        },
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
