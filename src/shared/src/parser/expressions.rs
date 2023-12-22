use crate::lexer::token::{TokenKind, Keyword};

use super::{cursor::Cursor, Expression, statements::parse_statement, ConditionBlock, FieldInitializer, Literal, Member};



pub fn parse_expression(cursor: &mut Cursor) -> Result<Expression, String> {
    #[cfg(feature = "interpreter")]
    let expression = parse_drop(cursor);

    #[cfg(not(feature = "interpreter"))]
    let expression = parse_block(cursor);

    let TokenKind::Semicolon = cursor.first().kind else {
        return Err(format!("Expected ; but found {:?}", cursor.first().kind));
    };

    expression
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

    Ok(Expression::Drop { identifier })
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

    Ok(Expression::Block { statements })
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

                    Ok(Expression::VariableDeclaration {
                        mutable: true,
                        type_name,
                        identifier,
                        initializer: Some(Box::new(initializer)),
                    })
                },
                TokenKind::Semicolon => {
                    Ok(Expression::VariableDeclaration {
                        mutable: true,
                        type_name,
                        identifier,
                        initializer: None,
                    })
                },
                _ => Err(format!("Expected = or : but found {:?}", cursor.first().kind)),
            }
        },
        TokenKind::Identifier(type_name) => {
            cursor.bump()?; // Consume the type identifier

            let TokenKind::Identifier(identifier) = cursor.bump()?.kind else {
                return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
            };

            match cursor.first().kind {
                TokenKind::Equal => {
                    cursor.bump()?; // Consume the =

                    let expression = parse_expression(cursor)?;

                    Ok(Expression::VariableDeclaration {
                        mutable: false,
                        type_name,
                        identifier,
                        initializer: Some(Box::new(expression)),
                    })
                },
                TokenKind::Semicolon => {
                    Ok(Expression::VariableDeclaration {
                        mutable: false,
                        type_name,
                        identifier,
                        initializer: None,
                    })
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

    return Ok(Expression::If
    {
        r#if: ConditionBlock { condition: Box::new(if_condition), block: Box::new(if_block) },
        else_ifs: if else_ifs.len() > 0 { Some(else_ifs) } else { None },
        r#else
    })
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
        field_initializers: if field_initializers.len() > 0 { Some(field_initializers) } else { None } 
    }))
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

    let TokenKind::OpenParen = cursor.first().kind else {
        return Ok(Expression::Literal(Literal::Union {
            type_name,
            member,
            field_initializers: None }));
    };

    cursor.bump()?; // Consume the (
    
    let mut field_initializers = vec![];
    let mut has_comma = true;

    while cursor.first().kind != TokenKind::CloseParen {
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

    cursor.bump()?; // Consume the )
    Ok(Expression::Literal(Literal::Union {
        type_name,
        member,
        field_initializers: if field_initializers.len() > 0 { Some(field_initializers) } else { None }
    }))
}

fn parse_assignment(cursor: &mut Cursor) -> Result<Expression, String> {
    let expression = parse_ternary(cursor)?;

    match cursor.first().kind {
        TokenKind::Equal => {
            cursor.bump()?; // Consume the =

            let initializer = parse_expression(cursor)?;

            Ok(Expression::Assignment {
                member: Box::new(expression),
                initializer: Box::new(initializer) })
        },
        _ => Ok(expression),
    }
}

fn parse_ternary(cursor: &mut Cursor) -> Result<Expression, String> {
    let expression = parse_logical(cursor)?;

    match cursor.first().kind {
        TokenKind::QuestionMark => {
            cursor.bump()?; // Consume the ?

            let true_expression = parse_expression(cursor)?;

            match cursor.first().kind {
                TokenKind::Colon => {
                    cursor.bump()?; // Consume the :

                    let false_expression = parse_expression(cursor)?;

                    Ok(Expression::Ternary {
                        condition: Box::new(expression),
                        true_expression: Box::new(true_expression),
                        false_expression: Box::new(false_expression) })
                },
                _ => Err(format!("Expected : but found {:?}", cursor.first().kind)),
            }
        },
        _ => Ok(expression),
    }
}

fn parse_logical(cursor: &mut Cursor) -> Result<Expression, String> {
    todo!()
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