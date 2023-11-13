namespace MyLang.Runtime;

public interface IRuntimeValue { }

public abstract class NumberValue : IRuntimeValue { }

public class Uninitialized : IRuntimeValue
{
    public static readonly Uninitialized Instance = new();

    private Uninitialized() { }
}

public class Int32Value : NumberValue
{
    public Int32Value(int value) => Value = value;
    public int Value { get; }
    public override string ToString() => Value.ToString();
}

public class Float32Value : NumberValue
{
    public Float32Value(float value) => Value = value;
    public float Value { get; }
    public override string ToString() => Value.ToString();
}

public class BooleanValue : NumberValue
{
    public BooleanValue(bool value) => Value = value;
    public bool Value { get; init; }
    public override string ToString() => Value.ToString();
}

public class EmptyProgramValue : IRuntimeValue { }