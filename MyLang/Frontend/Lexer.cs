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
                    tokens.Add(new ArithmeticOperatorToken(chars.Dequeue().ToString()));
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
        if (IsDigit(c) || c is TokenSymbol.DECIMAL_POINT_CHAR)
        {
            StringBuilder builder = new();

            while (chars.TryPeek(out char nextChar) && (IsDigit(nextChar) || nextChar is TokenSymbol.DECIMAL_POINT_CHAR || IsLetter(nextChar)))
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

        if (c is TokenSymbol.STRING_DELIMITER_CHAR)
        {
            StringBuilder builder = new();
            chars.Dequeue(); // Skip the opening quote

            while (chars.TryPeek(out char nextChar) && nextChar != TokenSymbol.STRING_DELIMITER_CHAR)
                builder.Append(chars.Dequeue());
            
            chars.Dequeue(); // Skip the closing quote
            tokens.Add(new StringToken(builder.ToString()));
            return;
        }

        if (c is TokenSymbol.AND_CHAR or TokenSymbol.OR_CHAR or TokenSymbol.BITWISE_XOR_CHAR or TokenSymbol.BITWISE_NOT_CHAR)
        {
            if (c is TokenSymbol.BITWISE_XOR_CHAR or TokenSymbol.BITWISE_NOT_CHAR)
            {
                chars.Dequeue();
                tokens.Add(new BitwiseOperatorToken(c.ToString()));
                return;
            }

            char previousChar = chars.Dequeue();

            if (chars.TryPeek(out char nextChar) && nextChar == previousChar)
            {
                chars.Dequeue();
                tokens.Add(new LogicalOperatorToken($"{previousChar}{nextChar}"));
                return;
            }
        }
        
        if (c is TokenSymbol.EQUALS_CHAR or TokenSymbol.GREATER_CHAR or TokenSymbol.LESS_CHAR or TokenSymbol.LOGICAL_NOT_CHAR)
        {
            char previousChar = chars.Dequeue();

            if (chars.TryPeek(out char nextChar) && nextChar == TokenSymbol.EQUALS_CHAR)
            {
                chars.Dequeue();
                tokens.Add(new ComparisonOperatorToken($"{previousChar}{nextChar}"));
                return;
            }

            if (c is TokenSymbol.LOGICAL_NOT_CHAR)
            {
                tokens.Add(new LogicalOperatorToken(TokenSymbol.LOGICAL_NOT));
                return;
            }
            
            tokens.Add(c is TokenSymbol.EQUALS_CHAR ? new EqualsToken() : new ComparisonOperatorToken(previousChar.ToString()));
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
    // Arithmetic operators
    public const char ADD_CHAR = '+';
    public const char SUBTRACT_CHAR = '-';
    public const char MULTIPLY_CHAR = '*';
    public const char DIVIDE_CHAR = '/';
    public const char MODULUS_CHAR = '%';
    
    public const string ADD = "+";
    public const string SUBTRACT = "-";
    public const string MULTIPLY = "*";
    public const string DIVIDE = "/";
    public const string MODULUS = "%";
    
    // Bitwise operators
    public const char AND_CHAR = '&';
    public const char OR_CHAR = '|';
    public const char BITWISE_XOR_CHAR = '^';
    public const char BITWISE_NOT_CHAR = '~';
    
    public const string BITWISE_AND = "&";
    public const string BITWISE_OR = "|";
    public const string BITWISE_XOR = "^";
    public const string BITWISE_NOT = "~";
    
    // Logical operators
    public const char LOGICAL_NOT_CHAR = '!';
    public const char GREATER_CHAR = '<';
    public const char LESS_CHAR = '>';
    
    public const string LOGICAL_AND = "&&";
    public const string LOGICAL_OR = "||";
    public const string LOGICAL_NOT = "!";
    public const string GREATER = "<";
    public const string LESS = ">";
    public const string LOGICAL_EQUAL = "==";
    public const string LOGICAL_NOT_EQUAL = "!=";
    public const string LOGICAL_GREATER_EQUAL = ">=";
    public const string LOGICAL_LESS_EQUAL = "<=";
    
    public const char EQUALS_CHAR = '=';
    public const string EQUALS = "=";

    // Parenthesis and braces
    public const char OPEN_PAREN_CHAR = '(';
    public const char CLOSE_PAREN_CHAR = ')';
    public const char OPEN_BRACE_CHAR = '[';
    public const char CLOSE_BRACE_CHAR = ']';
    public const char OPEN_ANGLE_BRACKET_CHAR = '<';
    public const char CLOSE_ANGLE_BRACKET_CHAR = '>';
    public const char STRING_DELIMITER_CHAR = '"';
    public const char CHAR_DELIMITER_CHAR = '\'';

    public const string OPEN_PAREN = "(";
    public const string CLOSE_PAREN = ")";
    public const string OPEN_BRACE = "[";
    public const string CLOSE_BRACE = "]";
    public const string OPEN_ANGLE_BRACKET = "<";
    public const string CLOSE_ANGLE_BRACKET = ">";
    public const string STRING_DELIMITER = "\"";
    public const string CHAR_DELIMITER = "'";

    // Punctuation
    public const char ARRAY_DELIMITER_CHAR = ',';
    public const char FUNCTION_ARGUMENT_DELIMITER_CHAR = ',';
    public const char GENERIC_ARGUMENT_DELIMITER_CHAR = ',';
    public const char MODULE_OR_TYPE_ACCESSOR_CHAR = '.';
    public const char MEMBER_ACCESSOR_CHAR = '.';
    public const char DECIMAL_POINT_CHAR = '.';
    public const char QUESTION_CHAR = '?';
    public const char COLON_CHAR = ':';
    public const char SEMI_COLON_CHAR = ';';

    public const string ARRAY_DELIMITER = ",";
    public const string FUNCTION_ARGUMENT_DELIMITER = ",";
    public const string GENERIC_ARGUMENT_DELIMITER = ",";
    public const string MODULE_OR_TYPE_ACCESSOR = ".";
    public const string MEMBER_ACCESSOR = ".";
    public const string DECIMAL_POINT = ".";
    public const string QUESTION = "?";
    public const string COLON = ":";
    public const string SEMI_COLON = ";";

    // Whitespace
    public const char SPACE_CHAR = ' ';
    public const char NEW_LINE_CHAR = '\n';
    public const char RETURN_CHAR = '\r';
    public const char TAB_CHAR = '\t';

    public const string SPACE = " ";
    public const string NEW_LINE = "\n";
    public const string RETURN = "\r";
    public const string TAB = "\t";
}

public static class Keyword
{
    public static readonly Dictionary<string, IToken> Keywords = new()
    {
        { DROP, new DropToken() },
        { TRUE, new BooleanToken(TRUE) },
        { FALSE, new BooleanToken(FALSE) },
        { MUTABLE, new MutableToken() },
        // { STRUCT, new StructToken() },
        // { UNION, new UnionToken() },
        // { FLAGS, new FlagsToken() },
        // { IF, new IfToken() },
        // { ELSE, new ElseToken() },
        // { MATCH, new MatchToken() },
        // { LOOP, new LoopToken() },
        // { FOR, new ForToken() },
        // { WHILE, new WhileToken() },
        // { DO, new DoToken() },
        // { BREAK, new BreakToken() },
        // { CONTINUE, new ContinueToken() },
    };
    
    // Built in functions
    public const string DROP = "drop"; // TODO: Remove this keyword
    
    // Booleans
    public const string TRUE = "true";
    public const string FALSE = "false";
    
    // Mutability
    public const string MUTABLE = "mutable";
    
    // // Types
    // public const string STRUCT = "struct";
    // public const string UNION = "union";
    // public const string FLAGS = "flags";
    //
    // // Branching
    // public const string IF = "if";
    // public const string ELSE = "else";
    // public const string MATCH = "match";
    //
    // // Loops
    // public const string LOOP = "loop";
    // public const string FOR = "for";
    // public const string WHILE = "while";
    // public const string DO = "do";
    // public const string BREAK = "break";
    // public const string CONTINUE = "continue";
}

public interface IToken
{
    public string Symbol { get; }
    
    // ReSharper disable once ReturnTypeCanBeNotNullable
    public string? ToString() => Symbol;
}

public sealed class DropToken : IToken { public string Symbol => Keyword.DROP; }
public sealed class MutableToken : IToken { public string Symbol => Keyword.MUTABLE; }

public sealed class NumberToken : IToken
{
    public NumberToken(string symbol) => Symbol = symbol;     
    public string Symbol { get; }
}

public sealed class StringToken : IToken
{
    public StringToken(string symbol) => Symbol = symbol;     
    public string Symbol { get; }
}

public sealed class BooleanToken : IToken
{
    public BooleanToken(string symbol) => Symbol = symbol;     
    public string Symbol { get; }
}

public sealed class IdentifierToken : IToken
{
    public IdentifierToken(string symbol) => Symbol = symbol;     
    public string Symbol { get; }
}

public sealed class ArithmeticOperatorToken : IToken
{
    public ArithmeticOperatorToken(string symbol) => Symbol = symbol; 
    public string Symbol { get; }
}

public sealed class BitwiseOperatorToken : IToken
{
    public BitwiseOperatorToken(string symbol) => Symbol = symbol; 
    public string Symbol { get; }
}

public sealed class LogicalOperatorToken : IToken
{
    public LogicalOperatorToken(string symbol) => Symbol = symbol; 
    public string Symbol { get; }
}

public sealed class ComparisonOperatorToken : IToken
{
    public ComparisonOperatorToken(string symbol) => Symbol = symbol; 
    public string Symbol { get; }
}

public sealed class EqualsToken : IToken { public string Symbol => TokenSymbol.EQUALS; }

public sealed class OpenParenToken : IToken { public string Symbol => TokenSymbol.OPEN_PAREN; }
public sealed class CloseParenToken : IToken { public string Symbol => TokenSymbol.CLOSE_PAREN; }
public sealed class OpenBraceToken : IToken { public string Symbol => TokenSymbol.OPEN_BRACE; }
public sealed class CloseBraceToken : IToken { public string Symbol => TokenSymbol.CLOSE_BRACE; }

public sealed class DelimiterToken : IToken { public string Symbol => TokenSymbol.ARRAY_DELIMITER; }
public sealed class DecimalPointToken : IToken { public string Symbol => TokenSymbol.DECIMAL_POINT; }

public sealed class QuestionToken : IToken { public string Symbol => TokenSymbol.QUESTION; }
public sealed class ColonToken : IToken { public string Symbol => TokenSymbol.COLON; }
public sealed class SemiColonToken : IToken { public string Symbol => TokenSymbol.SEMI_COLON; }

// public sealed class StructToken : IToken { public string Symbol => Keyword.STRUCT; }
// public sealed class UnionToken : IToken { public string Symbol => Keyword.UNION; }
// public sealed class FlagsToken : IToken { public string Symbol => Keyword.FLAGS; }
//
// public sealed class IfToken : IToken { public string Symbol => Keyword.IF; }
// public sealed class ElseToken : IToken { public string Symbol => Keyword.ELSE; }
// public sealed class MatchToken : IToken { public string Symbol => Keyword.MATCH; }
//
// public sealed class LoopToken : IToken { public string Symbol => Keyword.LOOP; }
// public sealed class ForToken : IToken { public string Symbol => Keyword.FOR; }
// public sealed class WhileToken : IToken { public string Symbol => Keyword.WHILE; }
// public sealed class DoToken : IToken { public string Symbol => Keyword.DO; }
// public sealed class BreakToken : IToken { public string Symbol => Keyword.BREAK; }
// public sealed class ContinueToken : IToken { public string Symbol => Keyword.CONTINUE; }

public sealed class EndOfFileToken : IToken { public string Symbol => string.Empty; }