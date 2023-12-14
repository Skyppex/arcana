using System.Reflection;

// ReSharper disable InconsistentNaming

namespace Arcana.TypeChecker;

public abstract class Type(string name)
{
    public static readonly Type statement = new UnionType("Statement");
    public static readonly Type never = new UnionType("never");
    public static readonly Type i32 = new StructType("i32");
    public static readonly Type f32 = new StructType("f32");
    public static readonly Type @string = new StructType("string");
    public static readonly Type @char = new StructType("char");
    public static readonly Type @bool = new StructType("bool");
    
    public static readonly Type[] Numbers = { i32, f32 };

    public string Name { get; } = name;

    public static Type FromString(string typeName) =>
        typeof(Type).GetFields(BindingFlags.Public | BindingFlags.Static)
            .FirstOrDefault(fi => fi.Name == typeName)
            ?.GetValue(null) as Type ?? throw new InvalidOperationException($"Unknown type: {typeName}");

    public override string ToString() => Name;
}

public sealed class StructType : Type
{
    public StructType(string name) : base(name) => Fields = new Dictionary<string, Type>();
    public StructType(string name, Dictionary<string, Type> fields) : base(name) => Fields = fields;

    public Dictionary<string, Type> Fields { get; }
}

public sealed class UnionType : Type
{
    public UnionType(string name) : base(name) => Members = new Dictionary<string, Dictionary<string, Type>>();
    public UnionType(string name, Dictionary<string, Dictionary<string, Type>> members) : base(name) => Members = members;

    public Dictionary<string, Dictionary<string, Type>> Members { get; }
}