using System.Reflection;

// ReSharper disable InconsistentNaming

namespace Arcana.TypeChecker;

public abstract class Type(string name)
{
    public static readonly Type statement = new UnionType(nameof(statement));
    public static readonly Type @void = new UnionType(nameof(@void));
    public static readonly Type i32 = new StructType(nameof(i32));
    public static readonly Type f32 = new StructType(nameof(f32));
    public static readonly Type @string = new StructType(nameof(@string));
    public static readonly Type @char = new StructType(nameof(@char));
    public static readonly Type @bool = new StructType(nameof(@bool));
    
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

public sealed class FunctionType : Type
{
    public FunctionType(string name, Type returnType, IEnumerable<Type> parameterTypes, TypeEnvironment typeEnvironment) : base(name)
    {
        ReturnType = returnType;
        ParameterTypes = parameterTypes;
        TypeEnvironment = typeEnvironment;
    }

    public Type ReturnType { get; }
    public IEnumerable<Type> ParameterTypes { get; }
    public TypeEnvironment TypeEnvironment { get; }
}