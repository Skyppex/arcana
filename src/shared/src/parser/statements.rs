use crate::{lexer::token::{TokenKind, Keyword, Token}, parser::AccessModifier};

use super::{Statement, cursor::Cursor, expressions::{parse_expression, parse_block, self}, Parameter, StructField, UnionMemberField, UnionMember};



pub fn parse_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    let statement = parse_function_declaration_statement(cursor);

    if let TokenKind::Semicolon = cursor.first().kind {
        cursor.bump()?; // Consume the semicolon
    }

    statement
}

fn parse_function_declaration_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    let mut access_modifier = None;

    if let TokenKind::Keyword(Keyword::AccessModifier(am)) = cursor.first().kind {
        if cursor.second().kind != TokenKind::Keyword(Keyword::Fn) {
            return parse_struct_declaration_statement(cursor);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Fn) {
        return parse_struct_declaration_statement(cursor);
    }

    cursor.bump()?; // Consume the fn keyword

    let TokenKind::Identifier(identifier) = cursor.bump()?.kind else {
        return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
    };

    let TokenKind::OpenParen = cursor.bump()?.kind else {
        return Err(format!("Expected ( but found {:?}", cursor.first().kind));
    };

    let parameters = parse_parameters(cursor)?;
    
    let TokenKind::CloseParen = cursor.bump()?.kind else {
        return Err(format!("Expected ) but found {:?}", cursor.first().kind));
    };
    
    let mut return_type = None;
    
    if cursor.first().kind == TokenKind::Colon {
        cursor.bump()?; // Consume the :

        if let TokenKind::Identifier(rt) = cursor.bump()?.kind {
            return_type = Some(rt);
        };
    }

    let body = parse_block(cursor)?;

    Ok(Statement::FunctionDeclaration {
        access_modifier,
        identifier,
        parameters,
        return_type,
        body,
    })
}

fn parse_struct_declaration_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    let mut access_modifier = None;

    if let TokenKind::Keyword(Keyword::AccessModifier(am)) = cursor.first().kind {
        if cursor.second().kind != TokenKind::Keyword(Keyword::Struct) {
            return parse_union_declaration_statement(cursor);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Struct) {
        return parse_union_declaration_statement(cursor);
    }

    cursor.bump()?; // Consume the struct keyword

    let TokenKind::Identifier(type_name) = cursor.bump()?.kind else {
        return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
    };

    let TokenKind::OpenBrace = cursor.bump()?.kind else {
        return Err(format!("Expected {{ but found {:?}", cursor.first().kind));
    };

    let mut fields = vec![];
    let mut has_comma = true;

    while cursor.first().kind != TokenKind::CloseBrace {
        if !has_comma {
            return Err(format!("Expected , but found {:?}", cursor.first().kind));
        }

        has_comma = true;
        fields.push(parse_struct_field(cursor)?);

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        } else {
            has_comma = false;
        }
    }

    cursor.bump()?; // Consume the }

    Ok(Statement::StructDeclaration {
        access_modifier,
        type_name,
        fields,
    })
}

fn parse_union_declaration_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    let mut access_modifier = None;

    if let TokenKind::Keyword(Keyword::AccessModifier(am)) = cursor.first().kind {
        if cursor.second().kind != TokenKind::Keyword(Keyword::Union) {
            return parse_expression_map(cursor);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Union) {
        return parse_expression_map(cursor);
    }

    cursor.bump()?; // Consume the union keyword

    let TokenKind::Identifier(type_name) = cursor.bump()?.kind else {
        return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
    };
    
    let TokenKind::OpenBrace = cursor.bump()?.kind else {
        return Err(format!("Expected {{ but found {:?}", cursor.first().kind));
    };

    let mut fields = vec![];
    let mut has_comma = true;

    while cursor.first().kind != TokenKind::CloseBrace {
        if !has_comma {
            return Err(format!("Expected , but found {:?}", cursor.first().kind));
        }

        has_comma = true;
        fields.push(parse_union_member(cursor)?);

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        } else {
            has_comma = false;
        }
    }

    cursor.bump()?; // Consume the }

    Ok(Statement::UnionDeclaration {
        access_modifier,
        type_name,
        fields,
    })
}

fn parse_expression_map(cursor: &mut Cursor) -> Result<Statement, String> {
    expressions::parse_expression(cursor).map(|e| Statement::Expression(e))
}

fn parse_parameters(cursor: &mut Cursor) -> Result<Vec<Parameter>, String> {
    let mut parameters = vec![];

    let mut has_comma = true;

    while cursor.first().kind != TokenKind::CloseParen {
        if !has_comma {
            return Err(format!("Expected , but found {:?}", cursor.first().kind));
        }

        has_comma = true;
        parameters.push(parse_parameter(cursor)?);

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        } else {
            has_comma = false;
        }
    }

    Ok(parameters)
}

fn parse_parameter(cursor: &mut Cursor) -> Result<Parameter, String> {
    let TokenKind::Identifier(identifier) = cursor.bump()?.kind else {
        return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
    };

    let TokenKind::Colon = cursor.bump()?.kind else {
        return Err(format!("Expected : but found {:?}", cursor.first().kind));
    };

    let TokenKind::Identifier(type_name) = cursor.bump()?.kind else {
        return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
    };

    Ok(Parameter {
        identifier,
        type_name,
    })
}

fn parse_struct_field(cursor: &mut Cursor) -> Result<StructField, String> {
    let mut access_modifier = None;

    if let TokenKind::Keyword(Keyword::AccessModifier(am)) = cursor.first().kind {
        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    let TokenKind::Identifier(identifier) = cursor.bump()?.kind else {
        return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
    };

    let TokenKind::Colon = cursor.bump()?.kind else {
        return Err(format!("Expected : but found {:?}", cursor.first().kind));
    };

    let mutable = match cursor.first().kind {
        TokenKind::Keyword(Keyword::Mutable) => {
            cursor.bump()?; // Consume the mutable keyword
            true
        },
        _ => false,
    };

    let TokenKind::Identifier(type_name) = cursor.bump()?.kind else {
        return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
    };

    Ok(StructField {
        access_modifier,
        mutable,
        identifier,
        type_name,
    })
}

fn parse_union_member(cursor: &mut Cursor) -> Result<UnionMember, String> {
    let TokenKind::Identifier(identifier) = cursor.bump()?.kind else {
        return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
    };
    
    match cursor.first().kind {
        TokenKind::OpenParen => {
            cursor.bump()?; // Consume the (

            let mut fields = vec![];

            let mut has_comma = true;
            
            while cursor.first().kind != TokenKind::CloseParen {
                if !has_comma {
                    return Err(format!("Expected , but found {:?}", cursor.first().kind));
                }

                has_comma = true;
                fields.push(parse_union_field(cursor)?);

                if cursor.first().kind == TokenKind::Comma {
                    cursor.bump()?; // Consume the ,
                } else {
                    has_comma = false;
                }
            }

            cursor.bump()?; // Consume the )

            Ok(UnionMember {
                identifier,
                fields,
            })
        },
        _ => Ok(UnionMember {
            identifier,
            fields: vec![],
        }),
    }
}

fn parse_union_field(cursor: &mut Cursor) -> Result<UnionMemberField, String> {
    let TokenKind::Identifier(first_ident) = cursor.bump()?.kind else {
        return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
    };

    match cursor.first().kind {
        TokenKind::Colon => {
            cursor.bump()?; // Consume the :
            
            let TokenKind::Identifier(type_name) = cursor.bump()?.kind else {
                return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
            };

            Ok(UnionMemberField {
                identifier: Some(first_ident),
                type_name,
            })
        },
        _ => {
            return Ok(UnionMemberField {
                identifier: None,
                type_name: first_ident,
            });
        },
    }
}
