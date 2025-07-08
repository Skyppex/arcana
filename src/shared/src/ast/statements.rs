use crate::{
    lexer::token::{IdentifierType, Keyword, TokenKind},
    types::{
        can_be_type_annotation, parse_generic_type_parameters, parse_type_annotation,
        parse_type_identifier, TypeAnnotation, TypeIdentifier,
    },
};

use super::{
    cursor::Cursor, expressions, fat_arrow_expr_or_block_expr, AccessModifier, AssociatedType,
    Closure, EmbeddedStruct, EnumDeclaration, Expression, FunctionDeclaration,
    ImplementationDeclaration, ModPath, ModuleDeclaration, Parameter, ProtocolDeclaration,
    Statement, StructData, StructDeclaration, StructField, TypeAliasDeclaration, UnionDeclaration,
    Use, UseItem, ValueLiteral,
};

pub fn parse_module(
    cursor: &mut Cursor,
) -> Result<Option<(Option<AccessModifier>, ModPath, Statement)>, String> {
    let mut access_modifier = None;

    if let Some(am) = cursor.first().kind.is_access_modifier() {
        if cursor.second().kind != TokenKind::Keyword(Keyword::Mod) {
            return Ok(None);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Mod) {
        return Ok(None);
    }

    cursor.bump()?; // Consume the mod keyword

    let TokenKind::Identifier(module_name) = cursor.first().kind else {
        return Err(format!(
            "Expected identifier but found {:?}",
            cursor.first().kind
        ));
    };

    cursor.bump()?; // Consume the identifier

    if !module_name.is_module_identifier_name() {
        return Err(format!("Invalid module name: {}", module_name));
    }

    let mut module_path: Vec<String> = vec![];
    module_path.push(module_name);

    while cursor.first().kind == TokenKind::DoubleColon {
        cursor.bump()?; // Consume the ::
        let TokenKind::Identifier(module_name) = cursor.first().kind else {
            return Err(format!(
                "Expected identifier but found {:?}",
                cursor.first().kind
            ));
        };

        cursor.bump()?; // Consume the identifier

        module_path.push(module_name);
    }

    cursor.expect(TokenKind::Semicolon)?;

    let module = parse_statements(cursor)?;

    Ok(Some((
        access_modifier,
        ModPath::new(module_path),
        Statement::Program { statements: module },
    )))
}

pub fn parse_file(cursor: &mut Cursor) -> Result<Vec<Statement>, String> {
    parse_mod_statement(cursor)
}

fn parse_mod_statement(cursor: &mut Cursor) -> Result<Vec<Statement>, String> {
    let mut access_modifier = None;

    if let Some(am) = cursor.first().kind.is_access_modifier() {
        if cursor.second().kind != TokenKind::Keyword(Keyword::Mod) {
            return parse_statement(cursor).map(|s| vec![s]);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Mod) {
        let mut statements = vec![];

        while !cursor.is_end_of_file() {
            statements.push(parse_statement(cursor)?);
        }

        return Ok(statements);
    }

    cursor.bump()?; // Consume the mod keyword

    let TokenKind::Identifier(module_name) = cursor.first().kind else {
        return Err(format!(
            "Expected identifier but found {:?}",
            cursor.first().kind
        ));
    };

    cursor.bump()?; // Consume the identifier

    if !module_name.is_module_identifier_name() {
        return Err(format!("Invalid module name: {}", module_name));
    }

    let mut module_path: Vec<String> = vec![];
    module_path.push(module_name);

    while cursor.first().kind == TokenKind::DoubleColon {
        cursor.bump()?; // Consume the ::
        let TokenKind::Identifier(module_name) = cursor.first().kind else {
            return Err(format!(
                "Expected identifier but found {:?}",
                cursor.first().kind
            ));
        };

        cursor.bump()?; // Consume the identifier

        module_path.push(module_name);
    }

    cursor.expect(TokenKind::Semicolon)?;

    let module_declaration = Statement::ModuleDeclaration(ModuleDeclaration {
        access_modifier,
        module_path: ModPath::new(module_path),
    });

    let mut statements = parse_statements(cursor)?;
    statements.insert(0, module_declaration);

    Ok(statements)
}

pub fn parse_statements(cursor: &mut Cursor) -> Result<Vec<Statement>, String> {
    let mut statements = vec![];

    while !cursor.is_end_of_file() {
        statements.push(parse_statement(cursor)?);
    }

    Ok(statements)
}

pub fn parse_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    match parse_use(cursor) {
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

fn parse_use(cursor: &mut Cursor) -> Result<Statement, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Use) {
        return parse_function_declaration_statement(cursor);
    }

    cursor.bump()?; // Consume the use keyword

    let use_item = parse_use_item(cursor)?;
    cursor.expect(TokenKind::Semicolon)?;
    Ok(Statement::Use(Use { use_item }))
}

fn parse_use_item(cursor: &mut Cursor) -> Result<UseItem, String> {
    match cursor.first().kind {
        TokenKind::Identifier(module_name) => {
            cursor.bump()?; // Consume the identifier

            if cursor.first().kind == TokenKind::DoubleColon {
                cursor.bump()?; // Consume the ::
                let use_item = parse_use_item(cursor)?;

                return Ok(UseItem::Navigation(module_name, Box::new(use_item)));
            }

            Ok(UseItem::Item(module_name))
        }
        TokenKind::OpenBrace => {
            cursor.bump()?; // Consume the {

            let mut use_items = vec![];
            let mut has_comma = true;

            while cursor.first().kind != TokenKind::CloseBrace {
                if !has_comma {
                    return Err(format!("Expected , but found {:?}", cursor.first().kind));
                }

                has_comma = true;
                use_items.push(parse_use_item(cursor)?);

                if cursor.first().kind == TokenKind::Comma {
                    cursor.bump()?; // Consume the ,
                } else {
                    has_comma = false;
                }
            }

            cursor.bump()?; // Consume the }

            Ok(UseItem::List(use_items))
        }
        _ => Err(format!(
            "Expected identifier or {{ but found {:?}",
            cursor.first().kind
        )),
    }
}

fn parse_function_declaration_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    let mut access_modifier = None;

    if let Some(am) = cursor.first().kind.is_access_modifier() {
        if cursor.second().kind != TokenKind::Keyword(Keyword::Fun) {
            return parse_struct_declaration_statement(cursor);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Fun) {
        return parse_struct_declaration_statement(cursor);
    }

    cursor.bump()?; // Consume the fun keyword

    let type_identifier = parse_type_identifier(cursor, false)?;

    if !type_identifier.is_function_identifier() {
        return Err(format!("Invalid function name: {}", type_identifier.name()));
    }

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
                "Expected type annotation but found {:?}",
                cursor.first().kind
            ));
        }

        return_type_annotation = Some(parse_type_annotation(cursor, true)?);
    }

    if cursor.first().kind == TokenKind::Semicolon {
        cursor.bump()?; // Consume the ;

        let (param, return_type_annotation) =
            unwrap_parameters_only(params, return_type_annotation)?;

        return Ok(Statement::FunctionDeclaration(FunctionDeclaration {
            access_modifier,
            type_identifier,
            param,
            return_type_annotation,
            body: None,
            signature_only: true,
        }));
    }

    let body = fat_arrow_expr_or_block_expr(cursor)?;

    let body = unwrap_parameters(
        access_modifier,
        type_identifier,
        params,
        return_type_annotation.clone(),
        body,
    )?;

    Ok(body)
}

fn unwrap_parameters_only(
    params: Vec<Parameter>,
    return_type_annotation: Option<TypeAnnotation>,
) -> Result<(Option<Parameter>, Option<TypeAnnotation>), String> {
    match params.last().cloned() {
        None => Ok((None, return_type_annotation)),
        Some(last) => {
            if params.len() == 1 {
                return Ok((Some(last), return_type_annotation));
            }

            let new_return_type_annotation = return_type_annotation.map(|r| {
                TypeAnnotation::Function(Some(Box::new(last.type_annotation)), Some(Box::new(r)))
            });

            unwrap_parameters_only(
                params.into_iter().rev().skip(1).rev().collect(),
                new_return_type_annotation,
            )
        }
    }
}

fn unwrap_parameters(
    access_modifier: Option<AccessModifier>,
    type_identifier: TypeIdentifier,
    params: Vec<Parameter>,
    return_type_annotation: Option<TypeAnnotation>,
    body: Expression,
) -> Result<Statement, String> {
    match params.first().cloned() {
        None => Ok(Statement::FunctionDeclaration(FunctionDeclaration {
            access_modifier,
            type_identifier,
            param: None,
            return_type_annotation,
            body: Some(body),
            signature_only: false,
        })),
        Some(first) => {
            let (new_body, new_return_type_annotation) = unwrap_parameters_recurse(
                params.into_iter().skip(1).collect(),
                return_type_annotation,
                body,
            )?;

            Ok(Statement::FunctionDeclaration(FunctionDeclaration {
                access_modifier,
                type_identifier,
                param: Some(first),
                return_type_annotation: new_return_type_annotation,
                body: Some(new_body),
                signature_only: false,
            }))
        }
    }
}

fn unwrap_parameters_recurse(
    params: Vec<Parameter>,
    return_type_annotation: Option<TypeAnnotation>,
    body: Expression,
) -> Result<(Expression, Option<TypeAnnotation>), String> {
    match params.last().cloned() {
        None => Ok((body, return_type_annotation)),
        Some(last) => {
            let new_body = Expression::Closure(Closure {
                param: Some(last.clone().into()),
                return_type_annotation: return_type_annotation.clone(),
                body: Box::new(body),
            });

            let new_return_type_annotation = return_type_annotation.map(|r| {
                TypeAnnotation::Function(Some(Box::new(last.type_annotation)), Some(Box::new(r)))
            });

            unwrap_parameters_recurse(
                params.into_iter().rev().skip(1).rev().collect(),
                new_return_type_annotation,
                new_body,
            )
        }
    }
}

fn parse_struct_declaration_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    let mut access_modifier = None;

    if let Some(am) = cursor.first().kind.is_access_modifier() {
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

    if !type_identifier.name().is_type_identifier_name() {
        return Err(format!("Invalid type name: {}", type_identifier.name()));
    }

    let TokenKind::OpenBrace = cursor.first().kind else {
        return Ok(Statement::StructDeclaration(StructDeclaration {
            access_modifier,
            body: StructData {
                type_identifier,
                embedded_structs: vec![],
                fields: vec![],
            },
        }));
    };

    cursor.bump()?; // Consume the {

    let body = parse_struct(type_identifier, cursor)?;

    cursor.bump()?; // Consume the }

    Ok(Statement::StructDeclaration(StructDeclaration {
        access_modifier,
        body,
    }))
}

fn parse_struct(
    type_identifier: TypeIdentifier,
    cursor: &mut Cursor,
) -> Result<StructData, String> {
    let embedded_structs = parse_embedded_structs(cursor)?;

    let mut fields = vec![];
    let mut has_comma = true;

    while cursor.first().kind != TokenKind::CloseBrace {
        if !has_comma {
            return Err(format!("Expected , but found {:?}", cursor.first().kind));
        }

        has_comma = true;
        fields.push(parse_struct_field(cursor, true)?);

        if cursor.first().kind == TokenKind::Comma {
            cursor.bump()?; // Consume the ,
        } else {
            has_comma = false;
        }
    }

    Ok(StructData {
        type_identifier,
        embedded_structs,
        fields,
    })
}

fn parse_enum_declaration_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    let mut access_modifier = None;

    if let Some(am) = cursor.first().kind.is_access_modifier() {
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

    if !type_name.name().is_type_identifier_name() {
        return Err(format!("Invalid type name: {}", type_name.name()));
    }

    if cursor.first().kind == TokenKind::Semicolon {
        cursor.bump()?; // Consume the ;
        return Ok(Statement::EnumDeclaration(EnumDeclaration {
            access_modifier,
            type_identifier: type_name,
            shared_fields: vec![],
            members: vec![],
        }));
    }

    let TokenKind::OpenBrace = cursor.bump()?.kind else {
        return Err(format!("Expected {{ but found {:?}", cursor.first().kind));
    };

    let mut shared_fields = vec![];
    let mut members = vec![];
    let mut has_comma = true;

    while cursor.first().kind != TokenKind::CloseBrace {
        if !has_comma {
            return Err(format!("Expected , but found {:?}", cursor.first().kind));
        }

        has_comma = true;

        if let (TokenKind::Identifier(_), TokenKind::Colon) =
            (cursor.first().kind, cursor.second().kind)
        {
            shared_fields.push(parse_struct_field(cursor, false)?);
        } else {
            let TokenKind::Identifier(identifier) = cursor.first().kind else {
                return Err(format!(
                    "Expected identifier but found {:?}",
                    cursor.first().kind
                ));
            };

            if !identifier.is_type_identifier_name() {
                return Err(format!("Invalid type name: {}", identifier));
            }

            cursor.bump()?; // Consume the identifier

            if cursor.first().kind == TokenKind::OpenBrace {
                cursor.expect(TokenKind::OpenBrace)?; // Consume the {
                members.push(parse_struct(TypeIdentifier::Type(identifier), cursor)?);
                cursor.expect(TokenKind::CloseBrace)?; // Consume the }
            } else {
                members.push(StructData {
                    type_identifier: TypeIdentifier::Type(identifier),
                    embedded_structs: vec![],
                    fields: vec![],
                });
            }
        }

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
        shared_fields,
        members,
    }))
}

fn parse_union_declaration_statement(cursor: &mut Cursor) -> Result<Statement, String> {
    let mut access_modifier = None;

    if let Some(am) = cursor.first().kind.is_access_modifier() {
        if cursor.second().kind != TokenKind::Keyword(Keyword::Union) {
            return parse_type_alias_declaration(cursor);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Union) {
        return parse_type_alias_declaration(cursor);
    }

    cursor.bump()?; // Consume the union keyword

    let TokenKind::Identifier(type_name) = cursor.bump()?.kind else {
        return Err(format!(
            "Expected identifier but found {:?}",
            cursor.prev().kind
        ));
    };

    if !type_name.is_type_identifier_name() {
        return Err(format!("Invalid type name: {}", type_name));
    }

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

    let literals: Result<Vec<ValueLiteral>, String> = literals
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

fn parse_type_alias_declaration(cursor: &mut Cursor) -> Result<Statement, String> {
    let mut access_modifier = None;
    if let Some(am) = cursor.first().kind.is_access_modifier() {
        if cursor.second().kind != TokenKind::Keyword(Keyword::Type) {
            return parse_protocol_declaration(cursor);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Type) {
        return parse_protocol_declaration(cursor);
    }

    cursor.bump()?; // Consume the type keyword

    let type_identifier = parse_type_identifier(cursor, false)?;

    if !type_identifier.name().is_type_identifier_name() {
        return Err(format!("Invalid type name: {}", type_identifier.name()));
    }

    let TokenKind::Equal = cursor.bump()?.kind else {
        return Err(format!("Expected = but found {:?}", cursor.first().kind));
    };

    let mut type_annotations = vec![parse_type_annotation(cursor, false)?];

    while cursor.first().kind == TokenKind::Keyword(Keyword::Or) {
        cursor.bump()?; // Consume the 'or'

        type_annotations.push(parse_type_annotation(cursor, false)?);
    }

    cursor.expect(TokenKind::Semicolon)?;

    Ok(Statement::TypeAliasDeclaration(TypeAliasDeclaration {
        access_modifier,
        type_identifier,
        type_annotations,
    }))
}

fn parse_protocol_declaration(cursor: &mut Cursor) -> Result<Statement, String> {
    let mut access_modifier = None;

    if let Some(am) = cursor.first().kind.is_access_modifier() {
        if cursor.second().kind != TokenKind::Keyword(Keyword::Proto) {
            return parse_implementation_declaration(cursor);
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    if cursor.first().kind != TokenKind::Keyword(Keyword::Proto) {
        return parse_implementation_declaration(cursor);
    }

    cursor.bump()?; // Consume the proto keyword

    let type_identifier = parse_type_identifier(cursor, false)?;

    if !type_identifier.name().is_type_identifier_name() {
        return Err(format!("Invalid type name: {}", type_identifier.name()));
    }

    if cursor.first().kind == TokenKind::Semicolon {
        cursor.bump()?; // Consume the ;

        return Ok(Statement::ProtocolDeclaration(ProtocolDeclaration {
            access_modifier,
            type_identifier,
            associated_types: vec![],
            functions: vec![],
        }));
    }

    cursor.expect(TokenKind::OpenBrace)?;

    let mut associated_types = vec![];

    while cursor.first().kind == TokenKind::Keyword(Keyword::Type) {
        cursor.bump()?; // Consume the type keyword

        let associated_type = parse_type_identifier(cursor, false)?;

        if !associated_type.name().is_generic_type_identifier_name() {
            return Err(format!(
                "Invalid associated type name: {}",
                associated_type.name()
            ));
        }

        if cursor.first().kind == TokenKind::Equal {
            cursor.bump()?; // Consume the :

            if !can_be_type_annotation(cursor) {
                return Err(format!(
                    "Expected type identifier but found {:?}",
                    cursor.first().kind
                ));
            }

            let type_annotation = parse_type_annotation(cursor, false)?;

            cursor.expect(TokenKind::Semicolon)?;

            associated_types.push(AssociatedType {
                type_identifier: associated_type,
                default_type_annotation: Some(type_annotation),
            });

            continue;
        }

        cursor.expect(TokenKind::Semicolon)?;

        associated_types.push(AssociatedType {
            type_identifier: associated_type,
            default_type_annotation: None,
        });
    }

    let mut functions = vec![];

    while cursor.first().kind == TokenKind::Keyword(Keyword::Fun) {
        let Statement::FunctionDeclaration(function) =
            parse_function_declaration_statement(cursor)?
        else {
            return Err(format!(
                "Expected function declaration but found {:?}",
                cursor.first().kind
            ));
        };

        functions.push(function);
    }

    cursor.expect(TokenKind::CloseBrace)?;

    Ok(Statement::ProtocolDeclaration(ProtocolDeclaration {
        access_modifier,
        type_identifier,
        associated_types,
        functions,
    }))
}

fn parse_implementation_declaration(cursor: &mut Cursor) -> Result<Statement, String> {
    if cursor.first().kind != TokenKind::Keyword(Keyword::Imp) {
        return parse_expression_map(cursor);
    }

    cursor.bump()?; // Consume the imp keyword

    let scoped_generics = if cursor.first().kind == TokenKind::Less {
        parse_generic_type_parameters(cursor)?
    } else {
        vec![]
    };

    let protocol_annotation = parse_type_annotation(cursor, false)?;

    if !protocol_annotation.name().is_type_identifier_name() {
        return Err(format!("Invalid type name: {}", protocol_annotation.name()));
    }

    let TokenKind::Keyword(Keyword::For) = cursor.bump()?.kind else {
        return Err(format!("Expected for but found {:?}", cursor.first().kind));
    };

    let type_annotation = parse_type_annotation(cursor, false)?;

    if !type_annotation.name().is_type_identifier_name() {
        return Err(format!("Invalid protocol name: {}", type_annotation.name()));
    }

    cursor.expect(TokenKind::OpenBrace)?;

    let mut associated_types = vec![];

    while cursor.first().kind == TokenKind::Keyword(Keyword::Type) {
        cursor.bump()?; // Consume the type keyword

        let associated_type = parse_type_identifier(cursor, false)?;

        if !associated_type.name().is_generic_type_identifier_name() {
            return Err(format!(
                "Invalid associated type name: {}",
                associated_type.name()
            ));
        }

        if cursor.first().kind == TokenKind::Equal {
            cursor.bump()?; // Consume the :

            if !can_be_type_annotation(cursor) {
                return Err(format!(
                    "Expected type identifier but found {:?}",
                    cursor.first().kind
                ));
            }

            let type_annotation = parse_type_annotation(cursor, false)?;

            cursor.expect(TokenKind::Semicolon)?;

            associated_types.push(AssociatedType {
                type_identifier: associated_type,
                default_type_annotation: Some(type_annotation),
            });

            continue;
        }

        cursor.expect(TokenKind::Semicolon)?;

        associated_types.push(AssociatedType {
            type_identifier: associated_type,
            default_type_annotation: None,
        });
    }

    let mut functions = vec![];

    while cursor.first().kind == TokenKind::Keyword(Keyword::Fun) {
        let Statement::FunctionDeclaration(function) =
            parse_function_declaration_statement(cursor)?
        else {
            return Err(format!(
                "Expected function declaration but found {:?}",
                cursor.first().kind
            ));
        };

        functions.push(function);
    }

    cursor.optional_bump(TokenKind::Semicolon)?;
    cursor.expect(TokenKind::CloseBrace)?;

    Ok(Statement::ImplementationDeclaration(
        ImplementationDeclaration {
            scoped_generics,
            protocol_annotation,
            type_annotation,
            associated_types,
            functions,
        },
    ))
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

    if !identifier.is_variable_identifier_name() {
        return Err(format!("Invalid variable name: {}", identifier));
    }

    let TokenKind::Colon = cursor.bump()?.kind else {
        return Err(format!("Expected : but found {:?}", cursor.first().kind));
    };

    if !can_be_type_annotation(cursor) {
        return Err(format!(
            "Expected type identifier but found {:?}",
            cursor.first().kind
        ));
    }

    let type_annotation = parse_type_annotation(cursor, false)?;

    Ok(Parameter {
        identifier,
        type_annotation,
    })
}

fn parse_struct_field(
    cursor: &mut Cursor,
    allow_access_modifier: bool,
) -> Result<StructField, String> {
    let mut access_modifier = None;

    if let Some(am) = cursor.first().kind.is_access_modifier() {
        if !allow_access_modifier {
            return Err(format!(
                "Unexpected access modifier {:?}",
                cursor.first().kind
            ));
        }

        cursor.bump()?; // Consume the access modifier
        access_modifier = Some(am);
    }

    let TokenKind::Identifier(identifier) = cursor.first().kind else {
        return Err(format!(
            "Expected identifier but found {:?}",
            cursor.first().kind
        ));
    };

    cursor.bump()?; // Consume the identifier

    if !identifier.is_variable_identifier_name() {
        return Err(format!("Invalid field name: {}", identifier));
    }

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

fn parse_embedded_structs(cursor: &mut Cursor) -> Result<Vec<EmbeddedStruct>, String> {
    let mut embedded_structs = vec![];

    loop {
        if let TokenKind::Identifier(identifier) = cursor.first().kind {
            if identifier.is_type_identifier_name() {
                let type_annotation = parse_type_annotation(cursor, false)?;

                if !matches!(
                    type_annotation,
                    TypeAnnotation::Type(_) | TypeAnnotation::ConcreteType(_, _)
                ) {
                    return Err(format!(
                        "Expected type annotation for a struct but found {:?}",
                        type_annotation
                    ));
                }

                let field_initializers = if cursor.first().kind == TokenKind::OpenBrace {
                    cursor.bump()?; // Consume the {
                    let fis = expressions::parse_field_initializers(cursor)?;
                    cursor.bump()?; // Consume the }
                    fis
                } else {
                    vec![]
                };

                embedded_structs.push(EmbeddedStruct {
                    type_annotation,
                    field_initializers,
                });

                if cursor.first().kind == TokenKind::Comma {
                    cursor.bump()?; // Consume the ,
                    continue;
                }
            }
        }

        break;
    }

    Ok(embedded_structs)
}

// fn parse_where_clause(cursor: &mut Cursor) -> Result<Option<Vec<GenericConstraint>>, String> {
//     if cursor.first().kind != TokenKind::Keyword(Keyword::Where) {
//         return Ok(None);
//     }
//
//     cursor.bump()?; // Consume the where
//
//     let mut constraints = vec![];
//
//     while !matches!(cursor.first().kind, TokenKind::Comma | TokenKind::FatArrow) {
//         constraints.push(parse_generic_constraint(cursor)?);
//     }
//
//     Ok(Some(constraints))
// }

// fn parse_generic_constraint(cursor: &mut Cursor) -> Result<GenericConstraint, String> {
//     let generic = parse_type_identifier(cursor, false)?;
//
//     let TypeIdentifier::Type(generic_name) = generic else {
//         return Err(format!(
//             "Expected type identifier variant Type but found {:?}",
//             generic
//         ));
//     };
//
//     if !generic_name.is_generic_type_identifier_name() {
//         return Err(format!("Invalid field name: {}", generic_name));
//     }
//
//     let TokenKind::Colon = cursor.bump()?.kind else {
//         return Err(format!("Expected ':' but found {:?}", cursor.first().kind));
//     };
//
//     let mut constraints = vec![];
//
//     let mut has_and = true;
//
//     while let TokenKind::Identifier(_) = cursor.first().kind {
//         if !has_and {
//             break;
//         }
//
//         let type_annotation = parse_type_annotation(cursor, false)?;
//         constraints.push(type_annotation);
//
//         if cursor.first().kind == TokenKind::Keyword(Keyword::And) {
//             cursor.bump()?; // Consume the and
//         } else {
//             has_and = false;
//         }
//     }
//
//     Ok(GenericConstraint {
//         generic: GenericType {
//             type_name: generic_name,
//         },
//         constraints,
//     })
// }
