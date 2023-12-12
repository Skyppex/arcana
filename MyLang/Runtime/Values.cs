using System.Globalization;

using MyLang.Models;

namespace MyLang.Runtime;

public interface IRuntimeValue;

public abstract class NumberValue : IRuntimeValue;

public sealed class Uninitialized : IRuntimeValue
{
    public static readonly Uninitialized Instance = new();
    private Uninitialized() { }
}

public sealed class NoValue : IRuntimeValue
{
    public static readonly NoValue Instance = new();
    private NoValue() { }
}

public class Int32Value(int value) : NumberValue
{
    public int Value { get; } = value;
    public override string ToString() => Value.ToString();
}

public class Float32Value(float value) : NumberValue
{
    public float Value { get; } = value;
    public override string ToString() => Value.ToString(CultureInfo.InvariantCulture);
}

public abstract class TextValue : IRuntimeValue
{
    public abstract string Text { get; }
} 

public class StringValue(string value) : TextValue
{
    public string Value { get; } = value;
    public override string ToString() => $"\"{Value}\"";

    public override string Text => ToString();
}

public class CharValue(char value) : TextValue
{
    public char Value { get; } = value;
    public override string ToString() => $"'{Value.ToString()}'";
    public override string Text => ToString();
}

public class BooleanValue(bool value) : IRuntimeValue
{
    public bool Value { get; } = value;
    public override string ToString() => Value.ToString();
}

public class StructValue(Dictionary<string, IRuntimeValue> fields) : IRuntimeValue
{
    public Dictionary<string, IRuntimeValue> Fields { get; } = fields;

    public override string ToString() => $"{{{string.Join(", ", Fields.Select(kv => $"{kv.Key}: {kv.Value}"))}}}";
}

public class UnionValue(UnionMember unionMember, Dictionary<string, IRuntimeValue> fields)
    : IRuntimeValue
{
    public UnionMember UnionMember { get; } = unionMember;
    public Dictionary<string, IRuntimeValue> Fields { get; } = fields;

    public override string ToString()
    {
        if (Fields.Count <= 0)
            return $"{UnionMember}";
        
        return $"{UnionMember}({string.Join(", ", Fields.Select(kv => $"{kv.Key}: {kv.Value}"))})";
    } 
}

public class EmptyProgramValue : IRuntimeValue;