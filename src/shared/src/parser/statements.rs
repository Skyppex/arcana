use std::vec;

use crate::{
    lexer::token::{Keyword, TokenKind},
    types::{
        can_be_type_annotation, parse_type_annotation, parse_type_annotation_from_str,
        parse_type_identifier, GenericConstraint, GenericType, TypeAnnotation, TypeIdentifier,
    },
};

use super::{
    cursor::Cursor,
    expressions::{self, parse_expression},
    AccessModifier, Closure, EnumDeclaration, EnumMember, EnumMemberField, Expression,
    FunctionDeclaration, Literal, Parameter, Statement, StructDeclaration, StructField,
    UnionDeclaration,
};

pub fn parse_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    match parse_break(cursor) {
        Ok(s) => {
            if let TokenKind::Semicolon = cursor.first().kind {
                cursor.bump()?; // Consume the ;
                return Ok(Statement::Semi(Box::new(s)));
            }

            Ok(s)
        }
        Err(e) => Err(e),
    }
}

fn parse_break(cursor: &mut Cursor) -> Result<Statement, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Break) {
        return parse_continue(cursor);
    }

    cursor.bump()?; // Consume the break

    let expression = if cursor.first().kind != TokenKind::Semicolon {
        let expression = parse_expression(cursor)?;
        expect_semicolon(cursor)?;
        Some(expression)
    } else {
        expect_semicolon(cursor)?;
        None
    };

    Ok(Statement::Break(expression))
}

fn parse_continue(cursor: &mut Cursor) -> Result<Statement, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Continue) {
        return parse_function_declaration_statement(cursor);
    }

    cursor.bump()?; // Consume the continue
    Ok(Statement::Continue)
}

fn parse_function_declaration_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    let mut access_modifier = None;

    if let TokenKind::Keyword(Keyword::AccessModifier(am)) = cursor.first().kind {
        if cursor.second().kind != TokenKind::Keyword(Keyword::Fun) {
            return parse_return(cursor);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Fun) {
        return parse_return(cursor);
    }

    cursor.bump()?; // Consume the func keyword

    let type_identifier = parse_type_identifier(cursor, false)?;

    let TokenKind::OpenParen = cursor.bump()?.kind else {
        return Err(format!("Expected ( but found {:?}", cursor.first().kind));
    };

    let params = parse_parameters(cursor)?;

    let TokenKind::CloseParen = cursor.bump()?.kind else {
        return Err(format!("Expected ) but found {:?}", cursor.first().kind));
    };

    let mut return_type_annotation = None;

    if cursor.first().kind == TokenKind::Colon {
        cursor.bump()?; // Consume the :

        if !can_be_type_annotation(cursor) {
            return Err(format!(
                "Expected type identifier but found {:?}",
                cursor.first().kind
            ));
        }

        return_type_annotation = Some(parse_type_annotation(cursor, true)?);
    }

    let TokenKind::FatArrow = cursor.bump()?.kind else {
        return Err(format!("Expected => but found {:?}", cursor.first().kind));
    };

    let body = parse_expression(cursor)?;
    let body = handle_multiple_parameters(
        access_modifier,
        type_identifier,
        &params,
        return_type_annotation.clone(),
        body,
    )?;

    Ok(body)
}

fn handle_multiple_parameters(
    access_modifier: Option<AccessModifier>,
    type_identifier: TypeIdentifier,
    params: &Vec<Parameter>,
    return_type_annotation: Option<TypeAnnotation>,
    body: Expression,
) -> Result<Statement, String> {
    let second_param = params.get(1).map(|p| p.clone());

    if let None = second_param {
        return Ok(Statement::FunctionDeclaration(FunctionDeclaration {
            access_modifier,
            identifier: type_identifier,
            param: params.first().cloned(),
            return_type_annotation,
            body,
        }));
    };

    let new_body = Expression::Closure(Closure {
        param: second_param.clone(),
        return_type_annotation: return_type_annotation.clone(),
        body: Box::new(body),
    });

    let new_return_type_annotation = return_type_annotation.map(|r| {
        TypeAnnotation::Function(
            second_param.map(|p| Box::new(p.type_annotation)),
            Box::new(r),
        )
    });

    Ok(Statement::FunctionDeclaration(FunctionDeclaration {
        access_modifier,
        identifier: type_identifier,
        param: params.first().cloned(),
        return_type_annotation: new_return_type_annotation,
        body: new_body,
    }))
}

fn parse_return(cursor: &mut Cursor) -> Result<Statement, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Return) {
        return parse_struct_declaration_statement(cursor);
    }

    cursor.bump()?; // Consume the return

    let expression = if cursor.first().kind == TokenKind::Semicolon {
        cursor.bump()?; // Consume the ;
        None
    } else {
        Some(parse_expression(cursor)?)
    };

    Ok(Statement::Return(expression))
}

fn parse_struct_declaration_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    let mut access_modifier = None;

    if let TokenKind::Keyword(Keyword::AccessModifier(am)) = cursor.first().kind {
        if cursor.second().kind != TokenKind::Keyword(Keyword::Struct) {
            return parse_enum_declaration_statement(cursor);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Struct) {
        return parse_enum_declaration_statement(cursor);
    }

    cursor.bump()?; // Consume the struct keyword

    let type_identifier = parse_type_identifier(cursor, false)?;

    let where_clause = parse_where_clause(cursor)?;

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

    Ok(Statement::StructDeclaration(StructDeclaration {
        access_modifier,
        type_identifier,
        where_clause,
        fields,
    }))
}

fn parse_enum_declaration_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    let mut access_modifier = None;

    if let TokenKind::Keyword(Keyword::AccessModifier(am)) = cursor.first().kind {
        if cursor.second().kind != TokenKind::Keyword(Keyword::Enum) {
            return parse_union_declaration_statement(cursor);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Enum) {
        return parse_union_declaration_statement(cursor);
    }

    cursor.bump()?; // Consume the enum keyword

    let type_name = parse_type_identifier(cursor, false)?;

    let TokenKind::OpenBrace = cursor.bump()?.kind else {
        return Err(format!("Expected {{ but found {:?}", cursor.first().kind));
    };

    let mut members = vec![];
    let mut has_comma = true;

    while cursor.first().kind != TokenKind::CloseBrace {
        if !has_comma {
            return Err(format!("Expected , but found {:?}", cursor.first().kind));
        }

        has_comma = true;
        members.push(parse_enum_member(cursor)?);

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        } else {
            has_comma = false;
        }
    }

    cursor.bump()?; // Consume the }

    Ok(Statement::EnumDeclaration(EnumDeclaration {
        access_modifier,
        type_identifier: type_name,
        members,
    }))
}

fn parse_union_declaration_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    let mut access_modifier = None;

    if let TokenKind::Keyword(Keyword::AccessModifier(am)) = cursor.first().kind {
        if cursor.second().kind != TokenKind::Keyword(Keyword::Union) {
            return parse_next(cursor);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Union) {
        return parse_next(cursor);
    }

    cursor.bump()?; // Consume the union keyword

    let TokenKind::Identifier(type_name) = cursor.bump()?.kind else {
        return Err(format!(
            "Expected identifier but found {:?}",
            cursor.prev().kind
        ));
    };

    let TokenKind::OpenBrace = cursor.bump()?.kind else {
        return Err(format!("Expected {{ but found {:?}", cursor.prev().kind));
    };

    let mut literals = vec![];
    let mut has_comma = true;

    while cursor.first().kind != TokenKind::CloseBrace {
        if !has_comma {
            return Err(format!("Expected , but found {:?}", cursor.first().kind));
        }

        has_comma = true;
        literals.push(expressions::parse_literal(cursor)?);

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        } else {
            has_comma = false;
        }
    }

    cursor.bump()?; // Consume the }

    let literals: Result<Vec<Literal>, String> = literals
        .iter()
        .map(|l| match l {
            Expression::Literal(l) => Ok(l.clone()),
            _ => Err(format!("Expected literal but found {:?}", l)),
        })
        .collect();

    Ok(Statement::UnionDeclaration(UnionDeclaration {
        access_modifier,
        type_identifier: TypeIdentifier::Type(type_name),
        literals: literals?,
    }))
}

fn parse_next(cursor: &mut Cursor) -> Result<Statement, String> {
    #[cfg(feature = "interpreter")]
    return parse_print(cursor);

    #[cfg(not(feature = "interpreter"))]
    return parse_expression_map(cursor);
}

#[cfg(feature = "interpreter")]
fn parse_print(cursor: &mut Cursor) -> Result<Statement, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Print) {
        return parse_expression_map(cursor);
    }

    cursor.bump()?; // Consume the drop

    let TokenKind::OpenParen = cursor.bump()?.kind else {
        return Err(format!("Expected ( but found {:?}", cursor.first().kind));
    };

    let expression = expressions::parse_expression(cursor)?;

    let TokenKind::CloseParen = cursor.bump()?.kind else {
        return Err(format!("Expected ) but found {:?}", cursor.first().kind));
    };

    Ok(Statement::Print(expression))
}

fn parse_expression_map(cursor: &mut Cursor) -> Result<Statement, String> {
    match expressions::parse_expression(cursor) {
        Ok(e) => {
            if let TokenKind::Semicolon = cursor.first().kind {
                cursor.bump()?; // Consume the ;
                return Ok(Statement::Semi(Box::new(Statement::Expression(e))));
            }

            Ok(Statement::Expression(e))
        }
        Err(e) => Err(e),
    }
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
        return Err(format!(
            "Expected identifier but found {:?}",
            cursor.first().kind
        ));
    };

    let TokenKind::Colon = cursor.bump()?.kind else {
        return Err(format!("Expected : but found {:?}", cursor.first().kind));
    };

    if !can_be_type_annotation(cursor) {
        return Err(format!(
            "Expected type identifier but found {:?}",
            cursor.first().kind
        ));
    }

    let type_anntation = parse_type_annotation(cursor, false)?;

    Ok(Parameter {
        identifier,
        type_annotation: type_anntation,
    })
}

fn parse_struct_field(cursor: &mut Cursor) -> Result<StructField, String> {
    let mut access_modifier = None;

    if let TokenKind::Keyword(Keyword::AccessModifier(am)) = cursor.first().kind {
        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    let TokenKind::Identifier(identifier) = cursor.bump()?.kind else {
        return Err(format!(
            "Expected identifier but found {:?}",
            cursor.first().kind
        ));
    };

    let TokenKind::Colon = cursor.bump()?.kind else {
        return Err(format!("Expected : but found {:?}", cursor.first().kind));
    };

    let mutable = match cursor.first().kind {
        TokenKind::Keyword(Keyword::Mut) => {
            cursor.bump()?; // Consume the mutable keyword
            true
        }
        _ => false,
    };

    if !can_be_type_annotation(cursor) {
        return Err(format!(
            "Expected type identifier but found {:?}",
            cursor.first().kind
        ));
    }

    let type_annotation = parse_type_annotation(cursor, false)?;

    Ok(StructField {
        access_modifier,
        mutable,
        identifier,
        type_annotation,
    })
}

fn parse_enum_member(cursor: &mut Cursor) -> Result<EnumMember, String> {
    let TokenKind::Identifier(identifier) = cursor.bump()?.kind else {
        return Err(format!(
            "Expected identifier but found {:?}",
            cursor.first().kind
        ));
    };

    match cursor.first().kind {
        TokenKind::OpenParen => {
            cursor.bump()?; // Consume the (

            let mut fields = vec![];

            let mut has_comma = true;

            let mut field_position = 0;
            while cursor.first().kind != TokenKind::CloseParen {
                if !has_comma {
                    return Err(format!("Expected , but found {:?}", cursor.first().kind));
                }

                has_comma = true;
                fields.push(parse_enum_field(cursor, field_position)?);

                if cursor.first().kind == TokenKind::Comma {
                    cursor.bump()?; // Consume the ,
                } else {
                    has_comma = false;
                }

                field_position += 1;
            }

            cursor.bump()?; // Consume the )

            Ok(EnumMember { identifier, fields })
        }
        _ => Ok(EnumMember {
            identifier,
            fields: vec![],
        }),
    }
}

fn parse_enum_field(cursor: &mut Cursor, field_position: usize) -> Result<EnumMemberField, String> {
    let TokenKind::Identifier(first_ident) = cursor.bump()?.kind else {
        return Err(format!(
            "Expected identifier but found {:?}",
            cursor.first().kind
        ));
    };

    match cursor.first().kind {
        TokenKind::Colon => {
            cursor.bump()?; // Consume the :

            if !can_be_type_annotation(cursor) {
                return Err(format!(
                    "Expected type identifier but found {:?}",
                    cursor.first().kind
                ));
            }

            let type_annotation = parse_type_annotation(cursor, false)?;

            Ok(EnumMemberField {
                identifier: first_ident,
                type_annotation,
            })
        }
        _ => {
            return Ok(EnumMemberField {
                identifier: format!("f{}", field_position.to_string()),
                type_annotation: parse_type_annotation_from_str(&first_ident, false)?,
            });
        }
    }
}

// fn parse_flags_member(cursor: &mut Cursor) -> Result<FlagsMember, String> {
//     let TokenKind::Identifier(identifier) = cursor.bump()?.kind else {
//         return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
//     };

//     Ok(FlagsMember {
//         identifier,
//         value: FlagsValue::Default,
//     })
// }

fn parse_where_clause(cursor: &mut Cursor) -> Result<Option<Vec<GenericConstraint>>, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Where) {
        return Ok(None);
    }

    cursor.bump()?; // Consume the where

    let mut constraints = vec![];

    while !matches!(cursor.first().kind, TokenKind::Comma | TokenKind::OpenBrace) {
        constraints.push(parse_generic_constraint(cursor)?);
    }

    Ok(Some(constraints))
}

fn parse_generic_constraint(cursor: &mut Cursor) -> Result<GenericConstraint, String> {
    let generic = parse_type_identifier(cursor, false)?;

    let TypeIdentifier::Type(generic_name) = generic else {
        return Err(format!(
            "Expected type identifier variant Type but found {:?}",
            generic
        ));
    };

    let TokenKind::Keyword(Keyword::Is) = cursor.bump()?.kind else {
        return Err(format!("Expected 'is' but found {:?}", cursor.first().kind));
    };

    let mut constraints = vec![];

    let mut has_and = true;

    while let TokenKind::Identifier(_) = cursor.first().kind {
        if !has_and {
            break;
        }

        let type_identifier = parse_type_identifier(cursor, false)?;
        constraints.push(type_identifier);

        if cursor.first().kind == TokenKind::Keyword(Keyword::And) {
            cursor.bump()?; // Consume the and
        } else {
            has_and = false;
        }
    }

    Ok(GenericConstraint {
        generic: GenericType {
            type_name: generic_name,
        },
        constraints,
    })
}

fn expect_semicolon(cursor: &mut Cursor) -> Result<(), String> {
    if cursor.first().kind != TokenKind::Semicolon {
        return Err(format!("Expected ; but found {:?}", cursor.first().kind));
    }

    cursor.bump()?; // Consume the ;
    Ok(())
}
