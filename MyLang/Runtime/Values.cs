using MyLang.Models;

namespace MyLang.Runtime;

public interface IRuntimeValue { }

public abstract class NumberValue : IRuntimeValue
{
    public abstract object Boxed { get; }
}

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

public class Int32Value : NumberValue
{
    public Int32Value(int value) => Value = value;
    public int Value { get; }
    public override object Boxed => Value;
    public override string ToString() => Value.ToString();
}

public class Float32Value : NumberValue
{
    public Float32Value(float value) => Value = value;
    public float Value { get; }
    public override object Boxed => Value;
    public override string ToString() => Value.ToString();
}

public abstract class TextValue : IRuntimeValue
{
    public abstract string Text { get; }
} 

public class StringValue : TextValue
{
    public StringValue(string value) => Value = value;
    public string Value { get; }
    public override string ToString() => $"\"{Value}\"";

    public override string Text => ToString();
}

public class CharValue : TextValue
{
    public CharValue(char value) => Value = value;
    public char Value { get; }
    public override string ToString() => $"'{Value.ToString()}'";
    public override string Text => ToString();
}

public class BooleanValue : IRuntimeValue
{
    public BooleanValue(bool value) => Value = value;
    public bool Value { get; }
    public override string ToString() => Value.ToString();
}

public class StructValue : IRuntimeValue
{
    public StructValue(Dictionary<string, IRuntimeValue> fields) => Fields = fields;

    public Dictionary<string, IRuntimeValue> Fields { get; }
    
    public override string ToString() => $"{{{string.Join(", ", Fields.Select(kv => $"{kv.Key}: {kv.Value}"))}}}";
}

public class UnionValue : IRuntimeValue
{
    public UnionValue(UnionMember unionMember, Dictionary<string, IRuntimeValue> fields)
    {
        UnionMember = unionMember;
        Fields = fields;
    }

    public UnionMember UnionMember { get; }
    public Dictionary<string, IRuntimeValue> Fields { get; }

    public override string ToString()
    {
        if (Fields.Count <= 0)
            return $"{UnionMember}";
        
        return $"{UnionMember}({string.Join(", ", Fields.Select(kv => $"{kv.Key}: {kv.Value}"))})";
    } 
}

public class EmptyProgramValue : IRuntimeValue { }