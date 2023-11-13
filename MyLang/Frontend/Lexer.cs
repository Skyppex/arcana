using System.Text;

namespace MyLang;

public class Lexer
{
    public List<IToken> Tokenize(string sourceCode)
    {
        var tokens = new List<IToken>();
        var chars = new Queue<char>(sourceCode);

        while (chars.TryPeek(out char c))
        {
            switch (c)
            {
                case TokenSymbol.OPEN_PAREN_CHAR:
                    tokens.Add(new OpenParenToken());
                    chars.Dequeue();
                    break;
                
                case TokenSymbol.CLOSE_PAREN_CHAR:
                    tokens.Add(new CloseParenToken());
                    chars.Dequeue();
                    break;
                
                case TokenSymbol.ADD_CHAR or TokenSymbol.SUBTRACT_CHAR or TokenSymbol.MULTIPLY_CHAR or TokenSymbol.DIVIDE_CHAR or TokenSymbol.MODULUS_CHAR:
                    tokens.Add(new BinaryOperatorToken(chars.Dequeue().ToString()));
                    break;
                
                case TokenSymbol.EQUALS_CHAR:
                    tokens.Add(new EqualsToken());
                    chars.Dequeue();
                    break;
                
                case TokenSymbol.QUESTION_CHAR:
                    tokens.Add(new QuestionToken());
                    chars.Dequeue();
                    break;
                
                case TokenSymbol.COLON_CHAR:
                    tokens.Add(new ColonToken());
                    chars.Dequeue();
                    break;
                
                case TokenSymbol.SEMI_COLON_CHAR:
                    tokens.Add(new SemiColonToken());
                    chars.Dequeue();
                    break;
                
                default:
                    HandleMultiCharToken(c, chars, tokens);
                    break;
            }
        }
        
        tokens.Add(new EndOfFileToken());
        return tokens;
    }
    
    private void HandleMultiCharToken(char c, Queue<char> chars, List<IToken> tokens)
    {
        if (IsDigit(c))
        {
            StringBuilder builder = new();

            while (chars.TryPeek(out char nextChar) && (IsDigit(nextChar) || nextChar is TokenSymbol.DECIMAL_POINT_CHAR))
                builder.Append(chars.Dequeue());
        
            tokens.Add(new NumberToken(builder.ToString()));
            return;
        }

        if (IsLetter(c))
        {
            StringBuilder builder = new();

            while (chars.TryPeek(out char nextChar) && IsAlphaNumeric(nextChar))
                builder.Append(chars.Dequeue());
        
            // Check for reserved keywords
            string symbol = builder.ToString();

            if (Keyword.Keywords.TryGetValue(symbol, out IToken? keywordToken))
            {
                tokens.Add(keywordToken);
                return;
            }
            
            tokens.Add(new IdentifierToken(symbol));
            return;
        }

        if (IsSkippable(c))
        {
            chars.Dequeue();
            return;
        }
        
        Console.WriteLine($"Unrecognized character found in source: {c}");
    }
    
    private static bool IsSkippable(char c) => c is TokenSymbol.SPACE_CHAR 
     or TokenSymbol.NEW_LINE_CHAR or 
        TokenSymbol.TAB_CHAR or 
        TokenSymbol.RETURN_CHAR;
    
    private static bool IsLetter(char c) => char.IsLetter(c);
    private static bool IsDigit(char c) => char.IsDigit(c);
    private static bool IsAlphaNumeric(char c) => IsLetter(c) || IsDigit(c);
}

public static class TokenSymbol
{
    public const char ADD_CHAR = '+';
    public const char SUBTRACT_CHAR = '-';
    public const char MULTIPLY_CHAR = '*';
    public const char DIVIDE_CHAR = '/';
    public const char MODULUS_CHAR = '%';
    public const char EQUALS_CHAR = '=';
    public const char OPEN_PAREN_CHAR = '(';
    public const char CLOSE_PAREN_CHAR = ')';
    public const char OPEN_BRACE_CHAR = '[';
    public const char CLOSE_BRACE_CHAR = ']';

    public const char DELIMITER_CHAR = ',';
    public const char DECIMAL_POINT_CHAR = '.';
    public const char QUESTION_CHAR = '?';
    public const char COLON_CHAR = ':';
    public const char SEMI_COLON_CHAR = ';';

    public const char SPACE_CHAR = ' ';
    public const char NEW_LINE_CHAR = '\n';
    public const char RETURN_CHAR = '\r';
    public const char TAB_CHAR = '\t';

    public const string ADD = "+";
    public const string SUBTRACT = "-";
    public const string MULTIPLY = "*";
    public const string DIVIDE = "/";
    public const string MODULUS = "%";
    public const string EQUALS = "=";
    public const string OPEN_PAREN = "(";
    public const string CLOSE_PAREN = ")";
    public const string OPEN_BRACE = "[";
    public const string CLOSE_BRACE = "]";

    public const string DELIMITER = ",";
    public const string DECIMAL_POINT = ".";
    public const string QUESTION = "?";
    public const string COLON = ":";
    public const string SEMI_COLON = ";";

    public const string SPACE = " ";
    public const string NEW_LINE = "\n";
    public const string RETURN = "\r";
    public const string TAB = "\t";
}

public static class Keyword
{
    public static readonly Dictionary<string, IToken> Keywords = new()
    {
        // { LET, new LetToken() },
        { MUT, new MutToken() },
        { DROP, new DropToken() },
        { STRUCT, new StructToken() },
    };
    
    // public const string LET = "let";
    public const string MUT = "mut";
    public const string DROP = "drop";
    public const string STRUCT = "struct";
}

public interface IToken
{
    public string Symbol { get; }
    
    // ReSharper disable once ReturnTypeCanBeNotNullable
    public string? ToString() => Symbol;
}

// public sealed class LetToken : IToken
// {
//     public string Symbol => Keyword.LET;
// }

public sealed class MutToken : IToken
{
    public string Symbol => Keyword.MUT;
}

public sealed class DropToken : IToken
{
    public string Symbol => Keyword.DROP;
}

public sealed class NumberToken : IToken
{
    public NumberToken(string symbol) => Symbol = symbol;
    
    public string Symbol { get; }
}

public sealed class IdentifierToken : IToken
{
    public IdentifierToken(string symbol) => Symbol = symbol;
    
    public string Symbol { get; }
}

public sealed class BinaryOperatorToken : IToken
{
    public BinaryOperatorToken(string symbol) => Symbol = symbol;

    public string Symbol { get; }
}

public sealed class EqualsToken : IToken
{
    public string Symbol => TokenSymbol.EQUALS;
}
public sealed class OpenParenToken : IToken
{
    public string Symbol => TokenSymbol.OPEN_PAREN;
}

public sealed class CloseParenToken : IToken
{
    public string Symbol => TokenSymbol.CLOSE_PAREN;
}

public sealed class OpenBraceToken : IToken
{
    public string Symbol => TokenSymbol.OPEN_BRACE;
}

public sealed class CloseBraceToken : IToken
{
    public string Symbol => TokenSymbol.CLOSE_BRACE;
}

public sealed class DelimiterToken : IToken
{
    public string Symbol => TokenSymbol.DELIMITER;
}

public sealed class DecimalPointToken : IToken
{
    public string Symbol => TokenSymbol.DECIMAL_POINT;
}

public sealed class QuestionToken : IToken
{
    public string Symbol => TokenSymbol.QUESTION;
}

public sealed class ColonToken : IToken
{
    public string Symbol => TokenSymbol.COLON;
}

public sealed class SemiColonToken : IToken
{
    public string Symbol => TokenSymbol.SEMI_COLON;
}

public sealed class StructToken : IToken
{
    public string Symbol => Keyword.STRUCT;
}

public sealed class EndOfFileToken : IToken
{
    public string Symbol => string.Empty;
}