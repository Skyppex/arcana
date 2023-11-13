using System.Globalization;

namespace MyLang;

public class Parser
{
    private List<IToken> _tokens;
    private readonly Lexer _lexer;

    public Parser() => _lexer = new Lexer();

    public Program CreateAST(string sourceCode)
    {
        _tokens = _lexer.Tokenize(sourceCode);
        List<IStatement> statements = new();

        while (Current() is not EndOfFileToken)
            statements.Add(ParseStatement());
        
        var program = new Program(statements);

        return program;
    }
    
    // Orders of precedence
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
    // PrimaryExpression
    
    private IStatement ParseStatement()
    {
        IStatement statement = ParseVariableDeclarationStatement();

        if (Current() is SemiColonToken)
            Next();
        
        return statement;
    }
    
    private IStatement ParseVariableDeclarationStatement()
    {
        if (Current() is not IdentifierToken type)
            return ParseExpression();

        if (ShouldNotExistBeforeSemiColon<EqualsToken>())
            return ParseExpression();

        bool mutable = NextIs<MutToken>(out _, out _);
        
        if (!NextIs<IdentifierToken>(out _, out _))
            return ParseExpression();
        
        if (!mutable)
            mutable = NextIs<MutToken>(out _, out _);

        return new VariableDeclarationStatement(type.Symbol, mutable, new Identifier(Next().Symbol));
    }

    private IExpression ParseExpression() => ParseDrop();

    private IExpression ParseDrop()
    {
        if (Current() is not DropToken)
            return ParseAssignmentExpression();
        
        Next();
        IToken identifier = Expect<IdentifierToken>("Expected identifier after 'drop' keyword.");
        return new DropExpression(new Identifier(identifier.Symbol));
    }
    
    private IExpression ParseAssignmentExpression()
    {
        if (Current() is not IdentifierToken)
            return ParseAdditiveExpression();

        if (!NextIs<EqualsToken>(out IToken identifierToken, out _))
            return ParseAdditiveExpression();

        return new AssignmentExpression(new Identifier(identifierToken.Symbol), ParseExpression());
    }
    
    private IExpression ParseAdditiveExpression()
    {
        IExpression left = ParseMultiplicativeExpression();

        while (Current().Symbol is TokenSymbol.ADD or TokenSymbol.SUBTRACT)
        {
            string @operator = Next().Symbol;
            IExpression right = ParseMultiplicativeExpression();
            
            left = new BinaryExpression(left, @operator, right);
        }
        
        return left;
    }
    
    private IExpression ParseMultiplicativeExpression()
    {
        IExpression left = ParseVariableDeclarationExpression();

        while (Current().Symbol is TokenSymbol.MULTIPLY or TokenSymbol.DIVIDE or TokenSymbol.MODULUS)
        {
            string @operator = Next().Symbol;
            IExpression right = ParseVariableDeclarationExpression();
            
            left = new BinaryExpression(left, @operator, right);
        }
        
        return left;
    }
    
    private IExpression ParseVariableDeclarationExpression()
    {
        if (Current() is not IdentifierToken type)
            return ParsePrimaryExpression();

        if (!ShouldNotExistBeforeSemiColon<EqualsToken>())
            return ParsePrimaryExpression();
        
        bool mutable = NextIs<MutToken>(out _, out _);

        Next();
        
        if (mutable)
            Next();
        
        IToken identifier = Expect<IdentifierToken>($"Expected identifier after {(mutable ? "'mut'" : $"'{type.Symbol}'")} keyword.");
        Expect<EqualsToken>("Expected '=' after identifier.");
        IExpression initializer = ParseExpression();
        return new VariableDeclarationExpression(type.Symbol, mutable, new Identifier(identifier.Symbol), initializer);
    }

    private IExpression ParsePrimaryExpression()
    {
        IToken token = Next();

        switch (token)
        {
            case IdentifierToken identifierToken:
                return new Identifier(identifierToken.Symbol);
            
            case NumberToken numberToken:
                string numberSymbol = numberToken.Symbol;

                if (numberSymbol.Contains(TokenSymbol.DECIMAL_POINT_CHAR))
                    return new Float32Literal(float.Parse(numberSymbol, NumberStyles.Any,
                        CultureInfo.InvariantCulture));

                return new Int32Literal(int.Parse(numberSymbol, NumberStyles.Any,
                    CultureInfo.InvariantCulture));
            
            case OpenParenToken:
                IExpression expression = ParseExpression();
                Next();
                return expression;
            
            default:
                Console.WriteLine($"Unexpected token found during parsing: {token}");
                return null;
        }
    }

    private IToken Current() => _tokens[0];
    private IToken Next()
    {
        IToken token = _tokens[0];
        _tokens.RemoveAt(0);
        return token;
    }
    
    private IToken Expect<T>(string errorMessage) where T : IToken
    {
        IToken token = Next();

        if (token is T)
            return token;
        
        Console.WriteLine($"Unexpected token found during parsing: {token}. Expected {typeof(T)} | {errorMessage}");
        return null;
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

    private bool ShouldNotExistBeforeSemiColon<T>() where T : IToken
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