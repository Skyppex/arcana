using System.Globalization;

using Arcana.Lexer;

using static Arcana.Result;
using static Arcana.Option;

namespace Arcana.Parser;

public class Parser
{
    private List<IToken> _tokens = new();
    private readonly Lexer.Lexer _lexer = new();

    public ProgramStatement CreateAst(string sourceCode)
    {
        _tokens = _lexer.Tokenize(sourceCode);
        List<Result<IStatement, string>> statements = new();

        while (Current() is not EndOfFileToken)
            statements.Add(ParseStatement());
        
        var program = new ProgramStatement(statements);

        return program;
    }
    
    // --- Order of precedence ---
    // VariableDeclarationStatement
    // AssignmentExpression
    // TernaryExpression
    // LogicalExpression
    // ComparisonExpression
    // AdditiveExpression
    // MultiplicativeExpression
    // VariableDeclarationExpression
    // UnaryExpression
    // MemberExpression
    // FunctionCallExpression
    // PrimaryExpression -- Highest precedence
    
    private Result<IStatement, string> ParseStatement()
    {
        Result<IStatement, string> statement = ParseVariableDeclarationStatement();

        if (Current() is SemiColonToken)
            Next();
        
        return statement;
    }
    
    private Result<IStatement, string> ParseVariableDeclarationStatement()
    {
        IdentifierToken? typeIdentifier = null;
        
        if (Current() is not MutableToken)
        {
            if (Current() is not IdentifierToken type)
                return ParseIfStatement();
            
            typeIdentifier = type;
        }
        
        if (ExistsBeforeSemiColon<EqualsToken>())
            return ParseIfStatement();
        
        bool mutable = Current() is MutableToken;
        
        if (!NextIs<MutableToken, IdentifierToken>(out _, out IdentifierToken? typeIdentIfMut, false) && !mutable)
            return ParseIfStatement();

        if (mutable)
        {
            typeIdentifier = typeIdentIfMut!;
            Next();
        }
        
        Next();
        
        return Ok<IStatement, string>(new VariableDeclarationStatement(typeIdentifier!.Symbol, mutable, new Identifier(Next().Symbol)));
    }
    
    private Result<IStatement, string> ParseIfStatement()
    {
        if (Current() is not IfToken)
            return ParseStructDeclarationStatement();

        Next();

        Result<IExpression, string> ifCondition = ParseExpression();
        Result<IExpression, string> ifBlock = ParseBlock();

        List<(Result<IExpression, string> Condition, Result<IExpression, string> Block)> elseIfStatements = new();
        
        Option<Result<IExpression, string>> elseBlock = Option<Result<IExpression, string>>.None;

        while (Current() is ElseToken)
        {
            Next();

            if (Current() is IfToken)
            {
                Next();
                Result<IExpression, string> condition = ParseExpression();                
                Result<IExpression, string> block = ParseBlock();
                elseIfStatements.Add((condition, block));
                continue;
            }
            
            elseBlock = Some(ParseBlock());
        }

        if (elseIfStatements.Count == 0 && elseBlock.IsNone())
        {
            return ifCondition.AndThen(ifc => ifBlock.Map(ifb => new IfStatement(new IfStatement.ConditionBlock(ifc, ifb),
                None<IEnumerable<IfStatement.ConditionBlock>>(),
                None<IExpression>()) as IStatement));
        }
        
        if (elseIfStatements.Count > 0 && elseBlock.IsNone())
        {
            return ifCondition.AndThen(ifc => ifBlock.AndThen(ifb => elseIfStatements.Select(elif =>
            {
                return elif.Condition.AndThen(elifc =>
                    elif.Block.Map(elifb => new IfStatement.ConditionBlock(elifc, elifb)));
            }).Invert().Map(cbs =>
                new IfStatement(new IfStatement.ConditionBlock(ifc, ifb),
                    Some(cbs), None<IExpression>()) as IStatement)));
        }

        if (elseIfStatements.Count == 0 && elseBlock.IsSome())
        {
            return ifCondition.AndThen(ifc => ifBlock.AndThen(ifb => elseBlock.Unwrap().Map(eb =>
                new IfStatement(new IfStatement.ConditionBlock(ifc, ifb),
                    None<IEnumerable<IfStatement.ConditionBlock>>(), Some(eb)) as IStatement)));
        }
        
        return ifCondition.AndThen(ifc => ifBlock.AndThen(ifb => elseIfStatements.Select(elif =>
        {
            return elif.Condition.AndThen(elifc =>
                elif.Block.Map(elifb => new IfStatement.ConditionBlock(elifc, elifb)));
        }).Invert().AndThen(cbs =>
            elseBlock.Unwrap().Map(eb =>
                new IfStatement(new IfStatement.ConditionBlock(ifc, ifb),
                    Some(cbs), Some(eb)) as IStatement))));
    }

    private Result<IStatement, string> ParseStructDeclarationStatement()
    {
        Option<string> accessModifier = Option<string>.None;
        if (Current() is AccessToken at)
        {
            if (!NextIs<AccessToken, StructToken>(out _, out _, false))
                return ParseUnionDeclarationStatement();
            
            accessModifier = Some(at.Symbol);
            Next();
        }
        
        if (Current() is not StructToken)
            return ParseUnionDeclarationStatement();
        
        Next();
        
        Result<IdentifierToken, string> structNameIdentifier = Expect<IdentifierToken>("Expected identifier after 'struct' keyword.");
        Result<OpenBlockToken, string> openCurly = Expect<OpenBlockToken>("Expected '{'.");

        List<Result<StructDeclarationStatement.Field, string>> fields = new();
        
        while (Current() is not CloseBlockToken)
            fields.Add(ParseField());

        Result<CloseBlockToken, string> closeCurly = Expect<CloseBlockToken>("Expected '}'.");
        
        return structNameIdentifier.AndThen(sni =>
            openCurly.AndThen(_ =>
                fields.Invert().AndThen(fs =>
                    closeCurly.Map(_ => new StructDeclarationStatement(accessModifier, sni.Symbol, fs) as IStatement))));
        
        Result<StructDeclarationStatement.Field, string> ParseField()
        {
            var accessModifier = Option<string>.None;
            if (Current() is AccessToken at)
            {
                accessModifier = Some(at.Symbol);
                Next();
            }

            Result<IdentifierToken, string> identifier = Expect<IdentifierToken>("Expected identifier.");
            Result<ColonToken, string> colon = Expect<ColonToken>("Expected ':'.");
            
            bool mutable = Current() is MutableToken;
            
            if (mutable)
                Next();
            
            Result<IdentifierToken, string> typeIdentifier = Expect<IdentifierToken>("Expected type identifier.");
            Result<SemiColonToken, string> semi = Expect<SemiColonToken>("Expected ';'.");
            
            return identifier.AndThen(i =>
                colon.AndThen(_ =>
                    typeIdentifier.AndThen(ti =>
                        semi.Map(_ => new StructDeclarationStatement.Field(
                            accessModifier,
                            mutable,
                            ti.Symbol,
                            i.Symbol)))));
        }
    }
    
    private Result<IStatement, string> ParseUnionDeclarationStatement()
    {
        Option<string> accessModifier = Option<string>.None;
        if (Current() is AccessToken at)
        {
            if (!NextIs<AccessToken, UnionToken>(out _, out _, false))
                return ParseExpression().Map(e => e as IStatement);
            
            accessModifier = Some(at.Symbol);
            Next();
        }
        
        if (Current() is not UnionToken)
            return ParseExpression().Map(e => e as IStatement);
        
        Next();
        
        Result<IdentifierToken, string> unionNameIdentifier = Expect<IdentifierToken>("Expected identifier after 'union' keyword.");
        
        var openCurly = Expect<OpenBlockToken>("Expected '{'.");
        List<Result<UnionDeclarationStatement.Member, string>> members = new();
        
        while (Current() is not CloseBlockToken)
            members.Add(ParseMember());

        Result<CloseBlockToken, string> closeCurly = Expect<CloseBlockToken>("Expected '}'.");
        
        return unionNameIdentifier.AndThen(sni =>
            openCurly.AndThen(_ =>
                members.Invert().AndThen(ms =>
                    closeCurly.Map(_ => new UnionDeclarationStatement(accessModifier, sni.Symbol, ms) as IStatement))));
        
        Result<UnionDeclarationStatement.Member, string> ParseMember()
        {
            Result<IdentifierToken, string> identifier = Expect<IdentifierToken>("Expected identifier.");

            if (Current() is not OpenParenToken)
            {
                Result<SemiColonToken, string> semiColon = Expect<SemiColonToken>("Expected ';'.");
                return semiColon.AndThen(_ => identifier.Map(ident => new UnionDeclarationStatement.Member(ident.Symbol, new List<UnionDeclarationStatement.Member.Field>())));
            }

            Next();
            List<Result<UnionDeclarationStatement.Member.Field, string>> fields = new();

            bool delimiter = true;
            
            while (delimiter && Current() is not CloseParenToken)
                fields.Add(ParseField());
            
            Result<CloseParenToken, string> close = Expect<CloseParenToken>("Expected ')'.");
            Result<SemiColonToken, string> semi = Expect<SemiColonToken>("Expected ';'.");
            
            return identifier.AndThen(ident =>
                fields.Invert().AndThen(fs =>
                    close.AndThen(_ => 
                        semi.Map(_ => new UnionDeclarationStatement.Member(ident.Symbol, fs.ToList())))));

            Result<UnionDeclarationStatement.Member.Field, string> ParseField()
            {
                Result<IdentifierToken, string> fieldIdentifier = Expect<IdentifierToken>("Expected identifier.");
                Result<ColonToken, string> colon = Expect<ColonToken>("Expected ':'.");
                Result<IdentifierToken, string> typeIdentifier = Expect<IdentifierToken>("Expected type identifier.");
                
                if (Current() is CommaToken)
                    Next();
                else
                    delimiter = false;

                return fieldIdentifier.AndThen(fi =>
                    colon.AndThen(_ =>
                        typeIdentifier.Map(ti => new UnionDeclarationStatement.Member.Field(
                            ti.Symbol,
                            fi.Symbol))));
            }
        }
    }
    
    private Result<IExpression, string> ParseExpression() => ParseDrop();

    private Result<IExpression, string> ParseDrop()
    {
        if (Current() is not DropToken)
            return ParseBlock();
        
        Next();
        Result<IdentifierToken, string> identifier = Expect<IdentifierToken>("Expected identifier after 'drop' keyword.");
        
        return identifier.Map(i => new DropExpression(new Identifier(i.Symbol)) as IExpression);
    }

    private Result<IExpression, string> ParseBlock()
    {
        if (Current() is not OpenBlockToken)
            return ParseStructLiteralExpression();

        Next();

        var statements = new List<Result<IStatement, string>>();
        while (Current() is not CloseBlockToken)
            statements.Add(ParseStatement());
        
        var closeBlock = Expect<CloseBlockToken>("Expected '}'.");
        
        return statements.Invert().AndThen(s => closeBlock.Map(_ => new BlockExpression(s.ToList()) as IExpression));
    }
    
    private Result<IExpression, string> ParseStructLiteralExpression()
    {
        if (!NextIs<IdentifierToken, OpenBlockToken>(out IdentifierToken? typeIdentifier, out _))
            return ParseUnionLiteralExpression();

        List<Result<StructLiteral.FieldInitializer, string>> fieldInitializers = new();
        
        bool delimiter = true;
        
        while (delimiter && Current() is IdentifierToken)
            fieldInitializers.Add(ParseFieldInitializer());

        Result<CloseBlockToken, string> closeBlock = Expect<CloseBlockToken>("Expected '}'.");
        
        return fieldInitializers.Invert()
            .AndThen(fi => closeBlock
            .Map(_ => new StructLiteral(typeIdentifier!.Symbol, fi.ToList()) as IExpression));

        Result<StructLiteral.FieldInitializer, string> ParseFieldInitializer()
        {
            Result<IdentifierToken, string> fieldIdentifier = Expect<IdentifierToken>("Expected field identifier.");
            Result<ColonToken, string> colon = Expect<ColonToken>("Expected ':'.");
            Result<IExpression, string> initializer = ParseExpression();

            if (Current() is CommaToken)
                Next();
            else
                delimiter = false;

            return fieldIdentifier
                .AndThen(fi => colon
                    .AndThen(_ => initializer
                        .Map(init => new StructLiteral.FieldInitializer(fi.Symbol, init))));
        }
    }
    
    private Result<IExpression, string> ParseUnionLiteralExpression()
    {
        if (!NextIs<IdentifierToken, ColonToken, ColonToken, IdentifierToken>(out IdentifierToken? typeIdentifier, out _, out _, out IdentifierToken? memberIdentifier))
            return ParseAssignmentExpression();

        if (Current() is not OpenParenToken)
            return Ok<IExpression, string>(new UnionLiteral(typeIdentifier!.Symbol, memberIdentifier!.Symbol, new List<UnionLiteral.FieldInitializer>()));
        
        Next();
        
        List<Result<UnionLiteral.FieldInitializer, string>> fieldInitializers = new();
        
        bool delimiter = true;
        
        while (delimiter && Current() is IdentifierToken)
            fieldInitializers.Add(ParseFieldInitializer());
    
        Result<CloseParenToken, string> closeBlock = Expect<CloseParenToken>("Expected ')'.");
        
        return fieldInitializers.Invert()
            .AndThen(fi => closeBlock
                .Map(_ => new UnionLiteral(typeIdentifier!.Symbol, memberIdentifier!.Symbol, fi.ToList()) as IExpression));
    
        Result<UnionLiteral.FieldInitializer, string> ParseFieldInitializer()
        {
            Result<IdentifierToken, string> fieldIdentifier = Expect<IdentifierToken>("Expected field identifier.");
            Result<ColonToken, string> colon = Expect<ColonToken>("Expected ':'.");
            Result<IExpression, string> initializer = ParseExpression();
    
            if (Current() is CommaToken)
                Next();
            else
                delimiter = false;
    
            return fieldIdentifier
                .AndThen(fi => colon
                    .AndThen(_ => initializer
                        .Map(init => new UnionLiteral.FieldInitializer(fi.Symbol, init))));
        }
    }

    private Result<IExpression, string> ParseAssignmentExpression()
    {
        if (Current() is not IdentifierToken)
            return ParseTernaryExpression();

        if (!ExistsBeforeSemiColon<EqualsToken>())
            return ParseTernaryExpression();

        if (NextIs<IdentifierToken, IdentifierToken>(out _, out _, doNext: false))
            return ParseTernaryExpression();
        
        Result<IExpression, string> member = ParseMemberAccessExpression();
        Result<EqualsToken, string> equals = Expect<EqualsToken>("Expected '='.");
        
        return member.Map(m => (MemberExpression)m).AndThen(m => equals.AndThen(_ => ParseExpression().Map(e => new AssignmentExpression(m, e) as IExpression)));
    }

    private Result<IExpression, string> ParseTernaryExpression()
    {
        Result<IExpression, string> condition = ParseLogicalExpression();

        if (Current() is not QuestionToken)
            return condition;

        Next();
        Result<IExpression, string> thenExpression = ParseLogicalExpression();
        Expect<ColonToken>("Expected ':'.");
        Result<IExpression, string> elseExpression = ParseLogicalExpression();
        
        return condition.AndThen(c => thenExpression.AndThen(t => elseExpression.Map(e => new TernaryExpression(c, t, e) as IExpression)));
    }

    private Result<IExpression, string> ParseLogicalExpression()
    {
        Result<IExpression, string> left = ParseComparisonExpression();

        while (Current() is LogicalOperatorToken { Symbol: TokenSymbol.LOGICAL_AND or TokenSymbol.LOGICAL_OR })
        {
            string @operator = Next().Symbol;
            Result<IExpression, string> right = ParseComparisonExpression();
            
            left = left.AndThen(l => right.Map(r => new BinaryExpression(l, @operator, r) as IExpression));
        }
        
        return left;
    }

    private Result<IExpression, string> ParseComparisonExpression()
    {
        Result<IExpression, string> left = ParseAdditiveExpression();

        while (Current() is ComparisonOperatorToken { Symbol: TokenSymbol.LOGICAL_EQUAL or TokenSymbol.LOGICAL_NOT_EQUAL or TokenSymbol.GREATER or TokenSymbol.LESS or TokenSymbol.LOGICAL_GREATER_EQUAL or TokenSymbol.LOGICAL_LESS_EQUAL })
        {
            string @operator = Next().Symbol;
            Result<IExpression, string> right = ParseAdditiveExpression();
            
            left = left.AndThen(l => right.Map(r => new BinaryExpression(l, @operator, r) as IExpression));
        }
        
        return left;
    }

    private Result<IExpression, string> ParseAdditiveExpression()
    {
        Result<IExpression, string> left = ParseMultiplicativeExpression();

        while (Current() is ArithmeticOperatorToken { Symbol: TokenSymbol.PLUS or TokenSymbol.MINUS })
        {
            string @operator = Next().Symbol;
            Result<IExpression, string> right = ParseMultiplicativeExpression();
            
            left = left.AndThen(l => right.Map(r => new BinaryExpression(l, @operator, r) as IExpression));
        }
        
        return left;
    }
    
    private Result<IExpression, string> ParseMultiplicativeExpression()
    {
        Result<IExpression, string> left = ParseVariableDeclarationExpression();

        while (Current() is ArithmeticOperatorToken { Symbol: TokenSymbol.STAR or TokenSymbol.SLASH or TokenSymbol.PERCENT })
        {
            string @operator = Next().Symbol;
            Result<IExpression, string> right = ParseVariableDeclarationExpression();
            
            left = left.AndThen(l => right.Map(r => new BinaryExpression(l, @operator, r) as IExpression));
        }
        
        return left;
    }
    
    private Result<IExpression, string> ParseVariableDeclarationExpression()
    {
        if (Current() is not MutableToken and not IdentifierToken)
            return ParseUnaryExpression();

        if (!ExistsBeforeSemiColon<EqualsToken>())
            return ParseUnaryExpression();

        bool mutable = Current() is MutableToken;

        if (mutable)
            Next();

        Result<IdentifierToken, string> typeIdentifier = 
            Expect<IdentifierToken>($"Expected identifier{(mutable ? " after 'mut' keyword" : "")}.");

        Result<IdentifierToken, string> identifier = typeIdentifier.AndThen(ti =>
            Expect<IdentifierToken>($"Expected identifier after {(mutable ? "'mut'" : $"'{ti.Symbol}'")}."));

        Expect<EqualsToken>("Expected '=' after identifier.");
        Result<IExpression, string> initializer = ParseExpression();
        return typeIdentifier.AndThen(ti => 
            identifier.AndThen(id => initializer.Map(init =>
                new VariableDeclarationExpression(ti.Symbol, mutable, new Identifier(id.Symbol), init) as IExpression)));
    }

    private Result<IExpression, string> ParseUnaryExpression()
    {
        if (Current() is ArithmeticOperatorToken { Symbol: TokenSymbol.PLUS or TokenSymbol.MINUS } operatorToken)
        {
            Next();
            return ParseExpression().Map(e => new UnaryExpression(operatorToken.Symbol, e) as IExpression);
        }
        
        if (Current() is LogicalOperatorToken { Symbol: TokenSymbol.EXCLAMATION } notToken)
        {
            Next();
            return ParseExpression().Map(e => new UnaryExpression(notToken.Symbol, e) as IExpression);
        }
        
        if (Current() is BitwiseOperatorToken { Symbol: TokenSymbol.TILDE } bitwiseNotToken)
        {
            Next();
            return ParseExpression().Map(e => new UnaryExpression(bitwiseNotToken.Symbol, e) as IExpression);
        }

        return ParseCallMemberExpression();
    }

    private Result<IExpression, string> ParseCallMemberExpression()
    {
        var member = ParseMemberAccessExpression();

        if (Current() is OpenParenToken)
            return ParseCallExpression(member);

        return member;
    }

    private Result<IExpression, string> ParseCallExpression(Result<IExpression, string> caller)
    {
        Result<List<IExpression>, string> arguments = ParseArgs();
        Result<IExpression, string> call = caller.AndThen(c => arguments.Map(args => new CallExpression(c, args) as IExpression));

        if (Current() is OpenParenToken)
            call = ParseCallExpression(call);
        
        return call;
    }

    private Result<List<IExpression>, string> ParseArgs()
    {
        Result<OpenParenToken, string> open = Expect<OpenParenToken>("Expected '('.");
        Result<List<IExpression>, string> args = Current() is CloseParenToken ? new() : ParseArgsList();
        Result<CloseParenToken, string> close = Expect<CloseParenToken>("Expected ')'.");
        return open.AndThen(_ => args.AndThen(a => close.Map(_ => a)));
    }

    private Result<List<IExpression>, string> ParseArgsList()
    {
        Result<List<IExpression>, string> args = ParseExpression().Map(e => new List<IExpression> { e });

        while (Current() is CommaToken && TryNext(out _))
            args = args.AndThen(a => ParseExpression().Map(e => a.Append(e).ToList()));

        return args;
    }

    private Result<IExpression, string> ParseMemberAccessExpression()
    {
        Result<IExpression, string> expression = ParsePrimaryExpression();

        while (Current() is MemberAccessorToken)
        {
            Next();

            if (Current() is not IdentifierToken)
                return Error<IExpression, string>("Expected identifier after '.'.");
            
            // Expression is an Identifier here
            Result<Identifier, string> property = ParsePrimaryExpression().Map(e => (Identifier)e);
            expression = expression.AndThen(e => property.Map(p => new MemberAccessExpression(e, p) as IExpression));
        }

        return expression;
    }

    private Result<IExpression, string> ParsePrimaryExpression()
    {
        IToken token = Next();

        switch (token)
        {
            case IdentifierToken identifierToken:
                return Ok<IExpression, string>(new Identifier(identifierToken.Symbol));
            
            case NumberToken numberToken:
                string numberSymbol = numberToken.Symbol;

                if (numberSymbol.Contains(TokenSymbol.PERIOD_CHAR) || numberSymbol.EndsWith(TypeChecker.Type.f32.ToString()))
                {
                    if (numberSymbol.EndsWith(TypeChecker.Type.f32.ToString()))
                        numberSymbol = numberSymbol[..^TypeChecker.Type.f32.ToString().Length];
                    
                    return Ok<IExpression, string>(new Float32Literal(float.Parse(numberSymbol, NumberStyles.Any,
                        CultureInfo.InvariantCulture)));
                }

                if (numberSymbol.EndsWith(TypeChecker.Type.i32.ToString()))
                    numberSymbol = numberSymbol[..^TypeChecker.Type.i32.ToString().Length];
                
                return Ok<IExpression, string>(new Int32Literal(int.Parse(numberSymbol, NumberStyles.Any,
                    CultureInfo.InvariantCulture)));
            
            case StringToken stringToken:
                return Ok<IExpression, string>(new StringLiteral(stringToken.Symbol));
            
            case CharToken charToken:
                if (!char.TryParse(charToken.Symbol, out char value))
                    return Error<IExpression, string>("Could not parse char.");
                    
                return Ok<IExpression, string>(new CharLiteral(value));
            
            case BooleanToken booleanToken:
                return Ok<IExpression, string>(new BooleanLiteral(booleanToken.Symbol == Keyword.TRUE));
            
            case OpenParenToken:
                Result<IExpression, string> expression = ParseExpression();
                Expect<CloseParenToken>("Expected ')'.");
                return expression;
            
            default:
                return Error<IExpression, string>($"Unexpected token found during parsing: {token}");
        }
    }

    private IToken Current() => _tokens[0];
    private IToken Next()
    {
        IToken token = _tokens[0];
        _tokens.RemoveAt(0);
        return token;
    }
    
    private IToken Next(uint amount)
    {
        if (amount == 0)
            throw new ArgumentException("Amount must be greater than 0.", nameof(amount));
        
        IToken token = Next();

        for (int i = 1; i < amount; i++)
            token = Next();

        return token;
    }
    
    private bool TryNext(out IToken? next)
    {
        if (_tokens.Count == 0)
        {
            next = null;
            return false;
        }

        next = _tokens[0];
        _tokens.RemoveAt(0);
        return true;
    }

    private Result<T, string> Expect<T>(string errorMessage) where T : IToken
    {
        IToken token = Current();

        if (token is T t)
        {
            Next();   
            return Ok<T, string>(t);
        }
        
        return Error<T, string>($"Unexpected token found during parsing: {token}. Expected {typeof(T)} | {errorMessage}");
    }
    
    private bool NextIs<T1, T2>(out T1? token, out T2? nextToken, bool doNext = true)
        where T1 : class, IToken
        where T2 : class, IToken
    {
        if (_tokens.Count < 2)
        {
            token = null;
            nextToken = null;
            return false;
        }
        
        bool currentIs = _tokens[0] is T1;
        bool nextIs = _tokens[1] is T2;
        token = currentIs ? _tokens[0] as T1 : null;
        nextToken = nextIs ? _tokens[1] as T2 : null;

        bool @is = currentIs && nextIs;

        if (@is)
        {
            if (!doNext)
                return true;
            
            Next();
            Next();
        }
        else
            token = null;
        
        return @is;
    }

    private bool NextIs<T1, T2, T3>(out T1? token, out T2? nextToken, out T3? nextNextToken, bool doNext = true)
        where T1 : class, IToken
        where T2 : class, IToken
        where T3 : class, IToken
    {
        if (_tokens.Count < 3)
        {
            token = null;
            nextToken = null;
            nextNextToken = null;
            return false;
        }
        
        bool currentIs = _tokens[0] is T1;
        bool nextIs = _tokens[1] is T2;
        bool nextNextIs = _tokens[2] is T3;
        token = currentIs ? _tokens[0] as T1 : null;
        nextToken = nextIs ? _tokens[1] as T2 : null;
        nextNextToken = nextNextIs ? _tokens[2] as T3 : null;

        bool @is = currentIs && nextIs && nextNextIs;

        if (@is)
        {
            if (!doNext)
                return true;
            
            Next();
            Next();
            Next();
        }
        else
            token = null;
        
        return @is;
    }

    private bool NextIs<T1, T2, T3, T4>(out T1? token, out T2? nextToken, out T3? nextNextToken, out T4? nextNextNextToken, bool doNext = true)
        where T1 : class, IToken
        where T2 : class, IToken
        where T3 : class, IToken
        where T4 : class, IToken
    {
        if (_tokens.Count < 4)
        {
            token = null;
            nextToken = null;
            nextNextToken = null;
            nextNextNextToken = null;
            return false;
        }
        
        bool currentIs = _tokens[0] is T1;
        bool nextIs = _tokens[1] is T2;
        bool nextNextIs = _tokens[2] is T3;
        bool nextNextNextIs = _tokens[3] is T4;
        token = currentIs ? _tokens[0] as T1 : null;
        nextToken = nextIs ? _tokens[1] as T2 : null;
        nextNextToken = nextNextIs ? _tokens[2] as T3 : null;
        nextNextNextToken = nextNextNextIs ? _tokens[3] as T4 : null;

        bool @is = currentIs && nextIs && nextNextIs && nextNextNextIs;

        if (@is)
        {
            if (!doNext)
                return true;
            
            Next();
            Next();
            Next();
            Next();
        }
        else
            token = null;
        
        return @is;
    }

    private bool ExistsBeforeSemiColon<T>() where T : IToken
    {
        int currentIndex = 0;
        IToken current = _tokens[currentIndex];

        while (current is not SemiColonToken and not EndOfFileToken)
        {
            if (current is T)
                return true;

            currentIndex++;
            current = _tokens[currentIndex];
        }

        return false;
    }
}