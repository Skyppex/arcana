using System.Text;

using static MyLang.Option;

namespace MyLang.Lexer;

public class Lexer
{
    public List<IToken> Tokenize(string sourceCode)
    {
        var tokens = new List<IToken>();
        var cursor = new Cursor(sourceCode);

        while (!cursor.IsEndOfFile())
        {
            var c = cursor.First();
            
            switch (c)
            {
                case TokenSymbol.PERIOD_CHAR:
                    tokens.Add(new MemberAccessorToken());
                    cursor.Bump();
                    break;
                
                case TokenSymbol.OPEN_PAREN_CHAR:
                    tokens.Add(new OpenParenToken());
                    cursor.Bump();
                    break;
                
                case TokenSymbol.CLOSE_PAREN_CHAR:
                    tokens.Add(new CloseParenToken());
                    cursor.Bump();
                    break;
                
                case TokenSymbol.OPEN_BRACE_CHAR:
                    tokens.Add(new OpenBraceToken());
                    cursor.Bump();
                    break;
                
                case TokenSymbol.CLOSE_BRACE_CHAR:
                    tokens.Add(new CloseBraceToken());
                    cursor.Bump();
                    break;
                
                case TokenSymbol.OPEN_CURLY_BRACE_CHAR:
                    tokens.Add(new OpenBlockToken());
                    cursor.Bump();
                    break;
                
                case TokenSymbol.CLOSE_CURLY_BRACE_CHAR:
                    tokens.Add(new CloseBlockToken());
                    cursor.Bump();
                    break;
                
                case TokenSymbol.PLUS_CHAR or TokenSymbol.MINUS_CHAR or TokenSymbol.STAR_CHAR or TokenSymbol.SLASH_CHAR or TokenSymbol.PERCENT_CHAR: 
                    tokens.Add(new ArithmeticOperatorToken(cursor.Bump().ToString()!));
                    break;
                
                case TokenSymbol.QUESTION_CHAR:
                    tokens.Add(new QuestionToken());
                    cursor.Bump();
                    break;
                
                case TokenSymbol.COLON_CHAR:
                    tokens.Add(new ColonToken());
                    cursor.Bump();
                    break;
                
                case TokenSymbol.SEMI_COLON_CHAR:
                    tokens.Add(new SemiColonToken());
                    cursor.Bump();
                    break;
                
                case TokenSymbol.COMMA_CHAR:
                    tokens.Add(new CommaToken());
                    cursor.Bump();
                    break;
                
                default:
                    HandleMultiCharToken(c, cursor, tokens);
                    break;
            }
        }
        
        tokens.Add(new EndOfFileToken());
        return tokens;
    }
    
    private void HandleMultiCharToken(char c, Cursor cursor, List<IToken> tokens)
    {
        if (IsNumericConstantFirst(c))
        {
            StringBuilder builder = new();
            builder.Append(c);
            
            char @base = 'd';
            
            if (IsNumberBasePrefixFirst(cursor.First()))
            {
                var second = cursor.Second(); // Read the second character of the prefix

                if (IsNumberBasePrefixSecond(second))
                {
                    @base = second;
                    cursor.Bump(); // Skip the first 0
                }
            }
            
            cursor.Bump(); // Skip 
            
            
            while (IsNumericConstantContinue(cursor.First()))
                builder.Append(cursor.Bump().Unwrap());
        
            tokens.Add(new NumberToken(@base, builder.ToString()));
            return;
        }

        if (IsIdentifierFirst(c))
        {
            StringBuilder builder = new();

            while (IsIdentifierContinue(cursor.First()))
                builder.Append(cursor.Bump().Unwrap());
        
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

        if (c is TokenSymbol.DOUBLE_QUOTE_CHAR)
        {
            StringBuilder builder = new();
            cursor.Bump(); // Skip the opening quote

            while (cursor.First() != TokenSymbol.DOUBLE_QUOTE_CHAR)
            {
                if (cursor.First() is TokenSymbol.BACKSLASH_CHAR)
                {
                    EscapableInString(cursor.Second())
                        .Match(
                            some: character =>
                            {
                                builder.Append(character);
                                cursor.Bump();
                                cursor.Bump();
                            },
                            none: () => builder.Append(cursor.Bump().Unwrap()));
                    
                    continue;
                }
                
                builder.Append(cursor.Bump().Unwrap());
            }
            
            cursor.Bump(); // Skip the closing quote
            tokens.Add(new StringToken(builder.ToString()));
            return;
        }

        if (c is TokenSymbol.SINGLE_QUOTE_CHAR)
        {
            StringBuilder builder = new();
            cursor.Bump(); // Skip the opening quote
            
            if (cursor.First() is TokenSymbol.BACKSLASH_CHAR)
            {
                EscapableInChar(cursor.Second())
                    .Match(
                        some: character =>
                        {
                            builder.Append(character);
                            cursor.Bump();
                            cursor.Bump();
                        },
                        none: () => builder.Append(cursor.Bump().Unwrap()));
            }
            else
                builder.Append(cursor.Bump().Unwrap());

            cursor.Bump().UnwrapOrElse(() => throw new Exception("Expected '")); // Skip the closing quote
            tokens.Add(new CharToken(builder.ToString()));
            return;
        }

        if (c is TokenSymbol.AMPERSAND_CHAR or TokenSymbol.PIPE_CHAR or TokenSymbol.HAT_CHAR or TokenSymbol.TILDE_CHAR)
        {
            if (c is TokenSymbol.HAT_CHAR or TokenSymbol.TILDE_CHAR)
            {
                cursor.Bump();
                tokens.Add(new BitwiseOperatorToken(c.ToString()));
                return;
            }

            cursor.Bump();

            if (cursor.First() == c)
            {
                cursor.Bump();
                tokens.Add(new LogicalOperatorToken($"{c}{c}"));
                return;
            }
        }
        
        if (c is TokenSymbol.EQUALS_CHAR or TokenSymbol.GREATER_CHAR or TokenSymbol.LESS_CHAR or TokenSymbol.EXCLAMATION_CHAR)
        {
            cursor.Bump();

            if (cursor.First() == TokenSymbol.EQUALS_CHAR)
            {
                cursor.Bump();
                tokens.Add(new ComparisonOperatorToken($"{c}{TokenSymbol.EQUALS_CHAR}"));
                return;
            }

            if (c is TokenSymbol.EXCLAMATION_CHAR)
            {
                tokens.Add(new LogicalOperatorToken(TokenSymbol.EXCLAMATION));
                return;
            }
            
            tokens.Add(c is TokenSymbol.EQUALS_CHAR ? new EqualsToken() : new ComparisonOperatorToken(c.ToString()));
            return;
        }
        
        if (IsWhitespace(c))
        {
            cursor.Bump();
            return;
        }

        // if (c is TokenSymbol.PERIOD_CHAR)
        // {
        //     tokens.Add(new MemberAccessorToken());
        //     cursor.Bump();
        //     return;
        // }
        
        Console.WriteLine($"Unrecognized character found in source: {c}");
    }

    private static bool IsIdentifierFirst(char c) => IsLetter(c) || c == '_';
    private static bool IsIdentifierContinue(char c) => IsAlphaNumeric(c) || c == '_';

    private static bool IsWhitespace(char c) => c is
        TokenSymbol.SPACE_CHAR or
        TokenSymbol.NEW_LINE_CHAR or
        TokenSymbol.TAB_CHAR or
        TokenSymbol.RETURN_CHAR;

    private static bool IsNumericConstantFirst(char c) => IsDigit(c) || c == TokenSymbol.PERIOD_CHAR;
    private static bool IsNumericConstantContinue(char c) => IsDigit(c) || c == TokenSymbol.PERIOD_CHAR;
    private static bool IsNumberBasePrefixFirst(char c) => c is '0';
    private static bool IsNumberBasePrefixSecond(char c) => c is 'b' or 'x' or 'd' or 'o' or 'h';

    private static Option<char> EscapableInString(char c) => AlwaysEscapable(c)
        .Or(c switch
        {
            '"' => Some(TokenSymbol.DOUBLE_QUOTE_CHAR),
            _ => None<char>()
        });

    private static Option<char> EscapableInChar(char c) => AlwaysEscapable(c)
        .Or(c switch
        {
            '\'' => Some(TokenSymbol.SINGLE_QUOTE_CHAR),
            _ => None<char>()
        });

    private static Option<char> AlwaysEscapable(char c) => c switch
    {
        '0' => Some(TokenSymbol.ENF_OF_FILE_CHAR),
        'a' => Some('\a'),
        'b' => Some('\b'),
        'f' => Some('\f'),
        'n' => Some(TokenSymbol.NEW_LINE_CHAR),
        'r' => Some(TokenSymbol.RETURN_CHAR),
        't' => Some(TokenSymbol.TAB_CHAR),
        'v' => Some('\v'),
        '\\' => Some(TokenSymbol.BACKSLASH_CHAR),
        _ => None<char>()
    };

    private static bool IsLetter(char c) => char.IsLetter(c);
    private static bool IsDigit(char c) => char.IsDigit(c);
    private static bool IsAlphaNumeric(char c) => IsLetter(c) || IsDigit(c);
}

public static class TokenSymbol
{
    public const char ENF_OF_FILE_CHAR = '\0';
    
    // Arithmetic operators
    public const char PLUS_CHAR = '+';
    public const char MINUS_CHAR = '-';
    public const char STAR_CHAR = '*';
    public const char SLASH_CHAR = '/';
    public const char PERCENT_CHAR = '%';
    
    public const string PLUS = "+";
    public const string MINUS = "-";
    public const string STAR = "*";
    public const string SLASH = "/";
    public const string PERCENT = "%";
    
    // Bitwise operators
    public const char AMPERSAND_CHAR = '&';
    public const char PIPE_CHAR = '|';
    public const char HAT_CHAR = '^';
    public const char TILDE_CHAR = '~';
    
    public const string AMPERSAND = "&";
    public const string PIPE = "|";
    public const string HAT = "^";
    public const string TILDE = "~";
    
    // Logical operators
    public const char EXCLAMATION_CHAR = '!';
    public const char LESS_CHAR = '<';
    public const char GREATER_CHAR = '>';
    
    public const string LOGICAL_AND = "&&";
    public const string LOGICAL_OR = "||";
    public const string EXCLAMATION = "!";
    public const string LESS = "<";
    public const string GREATER = ">";
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
    public const char OPEN_CURLY_BRACE_CHAR = '{';
    public const char CLOSE_CURLY_BRACE_CHAR = '}';
    public const char DOUBLE_QUOTE_CHAR = '"';
    public const char SINGLE_QUOTE_CHAR = '\'';
    public const char BACKSLASH_CHAR = '\\';

    public const string OPEN_PAREN = "(";
    public const string CLOSE_PAREN = ")";
    public const string OPEN_BRACE = "[";
    public const string CLOSE_BRACE = "]";
    public const string OPEN_CURLY_BRACE = "{";
    public const string CLOSE_CURLY_BRACE = "}";
    public const string DOUBLE_QUOTE = "\"";
    public const string SINGLE_QUOTE = "'";
    public const string BACKSLASH = "\\";

    // Punctuation
    public const char COMMA_CHAR = ',';
    public const char PERIOD_CHAR = '.';
    public const char QUESTION_CHAR = '?';
    public const char COLON_CHAR = ':';
    public const char SEMI_COLON_CHAR = ';';

    public const string COMMA = ",";
    public const string PERIOD = ".";
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
        { IF, new IfToken() },
        { ELSE, new ElseToken() },
        // { MATCH, new MatchToken() },
        // { LOOP, new LoopToken() },
        // { FOR, new ForToken() },
        // { WHILE, new WhileToken() },
        // { DO, new DoToken() },
        // { BREAK, new BreakToken() },
        // { CONTINUE, new ContinueToken() },
        { STRUCT, new StructToken() },
        { UNION, new UnionToken() },
        // { FLAGS, new FlagsToken() },
        { PUBLIC, new AccessToken(PUBLIC) },
        // { INTERNAL, new AccessToken(INTERNAL) },
    };
    
    // Built in functions
    public const string DROP = "drop"; // TODO: Remove this keyword
    
    // Booleans
    public const string TRUE = "true";
    public const string FALSE = "false";
    
    // Mutability
    public const string MUTABLE = "mutable";
    
    // Types
    public const string STRUCT = "struct";
    public const string UNION = "union";
    // public const string FLAGS = "flags";
    
    // Branching
    public const string IF = "if";
    public const string ELSE = "else";
    // public const string MATCH = "match";
    
    // Loops
    // public const string LOOP = "loop";
    // public const string FOR = "for";
    // public const string WHILE = "while";
    // public const string DO = "do";
    // public const string BREAK = "break";
    // public const string CONTINUE = "continue";
    
    // Access
    public const string PUBLIC = "public";
    // public const string INTERNAL = "internal";
}

public interface IToken
{
    public string Symbol { get; }
    public int Length => Symbol.Length;
    
    // ReSharper disable once ReturnTypeCanBeNotNullable
    public string? ToString() => Symbol;
}

public sealed class DropToken : IToken { public string Symbol => Keyword.DROP; }
public sealed class MutableToken : IToken { public string Symbol => Keyword.MUTABLE; }

public sealed class IfToken : IToken { public string Symbol => Keyword.IF; }
public sealed class ElseToken : IToken { public string Symbol => Keyword.ELSE; }

public sealed class NumberToken(char @base, string symbol) : IToken
{
    public char Base { get; } = @base;
    public string Symbol { get; } = symbol;
}

public sealed class StringToken(string symbol) : IToken
{
    public string Symbol { get; } = symbol;
}

public sealed class CharToken(string symbol) : IToken
{
    public string Symbol { get; } = symbol;
}

public sealed class BooleanToken(string symbol) : IToken
{
    public string Symbol { get; } = symbol;
}

public sealed class IdentifierToken(string symbol) : IToken
{
    public string Symbol { get; } = symbol;
}

public sealed class ArithmeticOperatorToken(string symbol) : IToken
{
    public string Symbol { get; } = symbol;
}

public sealed class BitwiseOperatorToken(string symbol) : IToken
{
    public string Symbol { get; } = symbol;
}

public sealed class LogicalOperatorToken(string symbol) : IToken
{
    public string Symbol { get; } = symbol;
}

public sealed class ComparisonOperatorToken(string symbol) : IToken
{
    public string Symbol { get; } = symbol;
}

public sealed class EqualsToken : IToken { public string Symbol => TokenSymbol.EQUALS; }

public sealed class OpenParenToken : IToken { public string Symbol => TokenSymbol.OPEN_PAREN; }
public sealed class CloseParenToken : IToken { public string Symbol => TokenSymbol.CLOSE_PAREN; }
public sealed class OpenBraceToken : IToken { public string Symbol => TokenSymbol.OPEN_BRACE; }
public sealed class CloseBraceToken : IToken { public string Symbol => TokenSymbol.CLOSE_BRACE; }
public sealed class OpenBlockToken : IToken { public string Symbol => TokenSymbol.OPEN_CURLY_BRACE; }
public sealed class CloseBlockToken : IToken { public string Symbol => TokenSymbol.CLOSE_CURLY_BRACE; }

public sealed class ArrayDelimiterToken : IToken { public string Symbol => TokenSymbol.COMMA; }
public sealed class CommaToken : IToken { public string Symbol => TokenSymbol.COMMA; }
public sealed class UnionFieldDelimiterToken : IToken { public string Symbol => TokenSymbol.COMMA; }
public sealed class DecimalPointToken : IToken { public string Symbol => TokenSymbol.PERIOD; }

public sealed class MemberAccessorToken : IToken { public string Symbol => TokenSymbol.PERIOD; }

public sealed class QuestionToken : IToken { public string Symbol => TokenSymbol.QUESTION; }
public sealed class ColonToken : IToken { public string Symbol => TokenSymbol.COLON; }
public sealed class SemiColonToken : IToken { public string Symbol => TokenSymbol.SEMI_COLON; }

public sealed class StructToken : IToken { public string Symbol => Keyword.STRUCT; }
public sealed class UnionToken : IToken { public string Symbol => Keyword.UNION; }
// public sealed class FlagsToken : IToken { public string Symbol => Keyword.FLAGS; }

// public sealed class IfToken : IToken { public string Symbol => Keyword.IF; }
// public sealed class ElseToken : IToken { public string Symbol => Keyword.ELSE; }
// public sealed class MatchToken : IToken { public string Symbol => Keyword.MATCH; }

// public sealed class LoopToken : IToken { public string Symbol => Keyword.LOOP; }
// public sealed class ForToken : IToken { public string Symbol => Keyword.FOR; }
// public sealed class WhileToken : IToken { public string Symbol => Keyword.WHILE; }
// public sealed class DoToken : IToken { public string Symbol => Keyword.DO; }
// public sealed class BreakToken : IToken { public string Symbol => Keyword.BREAK; }
// public sealed class ContinueToken : IToken { public string Symbol => Keyword.CONTINUE; }

public sealed class AccessToken(string symbol) : IToken
{
    public string Symbol { get; } = symbol;
}

public sealed class EndOfFileToken : IToken { public string Symbol => string.Empty; }