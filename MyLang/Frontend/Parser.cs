using System.Globalization;
using Monads;

using static Monads.Result;
using static Monads.Option;

namespace MyLang;

public class Parser
{
    private List<IToken> _tokens;
    private readonly Lexer _lexer;

    public Parser() => _lexer = new Lexer();

    public Program CreateAST(string sourceCode)
    {
        _tokens = _lexer.Tokenize(sourceCode);
        List<Result<IStatement, string>> statements = new();

        while (Current() is not EndOfFileToken)
            statements.Add(ParseStatement());
        
        var program = new Program(statements);

        return program;
    }
    
    // --- Order of precedence ---
    // VariableDeclarationStatement
    // VariableDeclarationExpression
    // AssignmentExpression
    // MemberExpression
    // FunctionCallExpression
    // LogicalExpression
    // ComparisonExpression
    // AdditiveExpression
    // MultiplicativeExpression
    // UnaryExpression
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
        IdentifierToken? ident = null;

        if (Current() is not MutableToken)
        {
            if (Current() is not IdentifierToken type)
                return ParseExpression().Map(e => e as IStatement);
            
            ident = type;
        }

        if (DoesNotExistBeforeSemiColon<EqualsToken>())
            return ParseExpression().Map(e => e as IStatement);

        bool mutable = NextIs<MutableToken>(out _, out _);
        
        if (!NextIs<IdentifierToken>(out _, out _))
            return ParseExpression().Map(e => e as IStatement);
        
        if (!mutable)
            mutable = NextIs<MutableToken>(out _, out _);

        return Ok<IStatement, string>(new VariableDeclarationStatement(ident!.Symbol, mutable, new Identifier(Next().Symbol)));
    }

    private Result<IExpression, string> ParseExpression() => ParseDrop();

    private Result<IExpression, string> ParseDrop()
    {
        if (Current() is not DropToken)
            return ParseAssignmentExpression();
        
        Next();
        var identifier = Expect<IdentifierToken>("Expected identifier after 'drop' keyword.");
        
        return identifier.Map(i => new DropExpression(new Identifier(i.Symbol)) as IExpression);
    }
    
    private Result<IExpression, string> ParseAssignmentExpression()
    {
        if (Current() is not IdentifierToken)
            return ParseLogicalExpression();

        if (!NextIs<EqualsToken>(out IToken? identifierToken, out _))
            return ParseLogicalExpression();

        return ParseExpression().Map(e => new AssignmentExpression(new Identifier(identifierToken!.Symbol), e) as IExpression);
    }
    
    public Result<IExpression, string> ParseLogicalExpression()
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

    public Result<IExpression, string> ParseComparisonExpression()
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

        while (Current() is ArithmeticOperatorToken { Symbol: TokenSymbol.ADD or TokenSymbol.SUBTRACT })
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

        while (Current() is ArithmeticOperatorToken { Symbol: TokenSymbol.MULTIPLY or TokenSymbol.DIVIDE or TokenSymbol.MODULUS })
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

        if (!DoesNotExistBeforeSemiColon<EqualsToken>())
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
        if (Current() is ArithmeticOperatorToken { Symbol: TokenSymbol.ADD or TokenSymbol.SUBTRACT } operatorToken)
        {
            Next();
            return ParseExpression().Map(e => new UnaryExpression(operatorToken.Symbol, e) as IExpression);
        }
        
        if (Current() is LogicalOperatorToken { Symbol: TokenSymbol.LOGICAL_NOT } notToken)
        {
            Next();
            return ParseExpression().Map(e => new UnaryExpression(notToken.Symbol, e) as IExpression);
        }
        
        if (Current() is BitwiseOperatorToken { Symbol: TokenSymbol.BITWISE_NOT } bitwiseNotToken)
        {
            Next();
            return ParseExpression().Map(e => new UnaryExpression(bitwiseNotToken.Symbol, e) as IExpression);
        }

        return ParsePrimaryExpression();
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

                if (numberSymbol.Contains(TokenSymbol.DECIMAL_POINT_CHAR) || numberSymbol.EndsWith(Type.f32.ToString()))
                {
                    if (numberSymbol.EndsWith(Type.f32.ToString()))
                        numberSymbol = numberSymbol[..^Type.f32.ToString().Length];
                    
                    return Ok<IExpression, string>(new Float32Literal(float.Parse(numberSymbol, NumberStyles.Any,
                        CultureInfo.InvariantCulture)));
                }

                if (numberSymbol.EndsWith(Type.i32.ToString()))
                    numberSymbol = numberSymbol[..^Type.i32.ToString().Length];
                
                return Ok<IExpression, string>(new Int32Literal(int.Parse(numberSymbol, NumberStyles.Any,
                    CultureInfo.InvariantCulture)));
            
            case StringToken stringToken:
                return Ok<IExpression, string>(new StringLiteral(stringToken.Symbol));
            
            case BooleanToken booleanToken:
                return Ok<IExpression, string>(new BooleanLiteral(booleanToken.Symbol == Keyword.TRUE));
            
            case OpenParenToken:
                Result<IExpression, string> expression = ParseExpression();
                Next();
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
    
    private Result<T, string> Expect<T>(string errorMessage) where T : IToken
    {
        IToken token = Next();

        if (token is T t)
            return Ok<T, string>(t);
        
        return Error<T, string>($"Unexpected token found during parsing: {token}. Expected {typeof(T)} | {errorMessage}");
    }
    
    private bool NextIs<T>(out IToken? token, out IToken? nextToken) where T : IToken
    {
        if (_tokens.Count < 2)
        {
            token = null;
            nextToken = null;
            return false;
        }
        
        nextToken = _tokens[1];
        bool nextIs = nextToken is T;

        if (nextIs)
        {
            token = Next();
            Next();
        }
        else
            token = null;
        
        return nextIs;
    }

    private bool DoesNotExistBeforeSemiColon<T>() where T : IToken
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