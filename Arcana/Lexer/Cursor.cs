using static Arcana.Option;

namespace Arcana.Lexer;

public class Cursor(string sourceCode)
{
    private readonly Queue<char> _chars = new Queue<char>(sourceCode);
    private int _lengthRemaining = sourceCode.Length;

    public override string ToString() => _chars.Aggregate(string.Empty, (current, c) => current + c);
    
    public char First() => _chars.TryPeek(out char c) ? c : TokenSymbol.ENF_OF_FILE_CHAR;
    
    public char Second()
    {
        var clone = _chars.Clone();
        
        if (!clone.TryDequeue(out _))
            return TokenSymbol.ENF_OF_FILE_CHAR;
        
        return clone.TryPeek(out char c) ? c : TokenSymbol.ENF_OF_FILE_CHAR;
    }
    
    public bool IsEndOfFile() => _chars.Count == 0;
    public int PositionWithinToken() => _lengthRemaining - ToString().Length;
    public void ResetPositionWithinToken() => _lengthRemaining = ToString().Length;

    public Option<char> Bump()
    {
        if (_chars.TryDequeue(out char c))
        {
            return Some(c);
        }

        return None<char>();
    }

    public void EatWhile(Predicate<char> predicate)
    {
        while (predicate(First()) && !IsEndOfFile())
            Bump();
    }
}

public static class QueueExtensions
{
    public static Queue<char> Clone(this Queue<char> queue) => new(queue);
}
