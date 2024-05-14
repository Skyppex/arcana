use std::vec;

use crate::{
    lexer::token::{Keyword, TokenKind},
    types::{
        can_be_type_annotation, parse_type_annotation, parse_type_annotation_from_str,
        parse_type_identifier, GenericConstraint, GenericType, TypeIdentifier,
    },
};

use super::{
    cursor::Cursor,
    expressions::{self, parse_block_statements, parse_expression},
    EnumDeclaration, EnumMember, EnumMemberField, Expression, FunctionDeclaration, Impl, Literal,
    Parameter, Statement, StructDeclaration, StructField, TraitDeclaration, UnionDeclaration,
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
        if cursor.second().kind != TokenKind::Keyword(Keyword::Func) {
            return parse_return(cursor);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Func) {
        return parse_return(cursor);
    }

    cursor.bump()?; // Consume the func keyword

    let type_identifier = parse_type_identifier(cursor, false)?;

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

        if !can_be_type_annotation(cursor) {
            return Err(format!(
                "Expected type identifier but found {:?}",
                cursor.first().kind
            ));
        }

        return_type = Some(parse_type_annotation(cursor, false)?);
    }

    let body = parse_block_statements(cursor)?;

    Ok(Statement::FunctionDeclaration(FunctionDeclaration {
        access_modifier,
        identifier: type_identifier,
        parameters,
        return_type,
        body,
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
            return parse_trait_declaration_statement(cursor);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Union) {
        return parse_trait_declaration_statement(cursor);
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

fn parse_trait_declaration_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    let mut access_modifier = None;

    if let TokenKind::Keyword(Keyword::AccessModifier(am)) = cursor.first().kind {
        if cursor.second().kind != TokenKind::Keyword(Keyword::Trait) {
            return parse_impl(cursor);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Trait) {
        return parse_impl(cursor);
    }

    let TokenKind::Identifier(type_name) = cursor.bump()?.kind else {
        return Err(format!(
            "Expected identifier but found {:?}",
            cursor.prev().kind
        ));
    };

    let TokenKind::OpenBrace = cursor.bump()?.kind else {
        return Err(format!("Expected {{ but found {:?}", cursor.prev().kind));
    };

    let mut associated_types = vec![];

    while cursor.first().kind == TokenKind::Keyword(Keyword::Type) {
        cursor.bump()?; // Consume the type keyword

        let associated_type = parse_type_identifier(cursor, false)?;

        let TokenKind::Semicolon = cursor.bump()?.kind else {
            return Err(format!("Expected ; but found {:?}", cursor.prev().kind));
        };

        associated_types.push(associated_type);
    }

    let mut functions = vec![];

    while cursor.first().kind != TokenKind::CloseBrace {
        let function_declaration_statement = parse_function_declaration_statement(cursor)?;

        let Statement::FunctionDeclaration(function_declaration) =
            function_declaration_statement.clone()
        else {
            return Err(format!(
                "Expected function declaration but found {:?}",
                function_declaration_statement
            ));
        };

        functions.push(function_declaration);
    }

    cursor.bump()?; // Consume the }

    Ok(Statement::TraitDeclaration(TraitDeclaration {
        access_modifier,
        type_identifier: TypeIdentifier::Type(type_name),
        associated_types,
        functions: functions,
    }))
}

// fn parse_flags_declaration_statement(cursor: &mut Cursor) -> Result<Statement, String> {
//     let mut access_modifier = None;

//     if let TokenKind::Keyword(Keyword::AccessModifier(am)) = cursor.first().kind {
//         if cursor.second().kind != TokenKind::Keyword(Keyword::Flags) {
//             return parse_print(cursor);
//         }

//         cursor.bump()?; // Consume the access modifier
//         access_modifier = Some(am);
//     }

//     if cursor.first().kind != TokenKind::Keyword(Keyword::Flags) {
//         return parse_print(cursor);
//     }

//     cursor.bump()?; // Consume the flags keyword

//     let TokenKind::Identifier(type_name) = cursor.bump()?.kind else {
//         return Err(format!("Expected identifier but found {:?}", cursor.first().kind));
//     };

//     let TokenKind::OpenBrace = cursor.bump()?.kind else {
//         return Err(format!("Expected {{ but found {:?}", cursor.first().kind));
//     };

//     let mut members = vec![];
//     let mut has_comma = true;

//     while cursor.first().kind != TokenKind::CloseBrace {
//         if !has_comma {
//             return Err(format!("Expected , but found {:?}", cursor.first().kind));
//         }

//         has_comma = true;
//         members.push(parse_flags_member(cursor)?);

//         if cursor.first().kind == TokenKind::Comma {
//             cursor.bump()?; // Consume the ,
//         } else {
//             has_comma = false;
//         }
//     }

//     cursor.bump()?; // Consume the }

//     Ok(Statement::FlagsDeclaration(FlagsDeclaration {
//         access_modifier,
//         type_name,
//         members,
//     }))
// }

fn parse_impl(cursor: &mut Cursor) -> Result<Statement, String> {
    let TokenKind::Keyword(Keyword::Impl) = cursor.first().kind else {
        return parse_next(cursor);
    };

    cursor.bump()?; // Consume the impl

    if !can_be_type_annotation(cursor) {
        return Err(format!(
            "Expected type annotation but found {:?}",
            cursor.first().kind
        ));
    }

    let type_annotation = parse_type_annotation(cursor, false)?;
    let methods = parse_block_statements(cursor)?;

    for method in methods.iter() {
        if !matches!(method, Statement::FunctionDeclaration(_)) {
            return Err(format!(
                "Expected function declaration but found {:?}",
                method
            ));
        }
    }

    Ok(Statement::Impl(Impl {
        type_annotation,
        functions: methods,
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
