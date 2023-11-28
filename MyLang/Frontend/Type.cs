using System.Reflection;
// ReSharper disable InconsistentNaming

namespace MyLang;

public class Type : IEquatable<Type>
{
    public static readonly Type statement = new("Statement", TypeMode.Union);
    public static readonly Type never = new("never", TypeMode.Union);
    public static readonly Type i32 = new("i32", TypeMode.Struct);
    public static readonly Type f32 = new("f32", TypeMode.Struct);
    public static readonly Type @string = new("string", TypeMode.Struct);
    public static readonly Type @bool = new("bool", TypeMode.Struct);
    
    public static readonly Type[] Numbers = { i32, f32 };
    
    public Type(string name, TypeMode mode)
    {
        Name = name;
        Mode = mode;
        Fields = new Dictionary<string, Type>();
    }
    
    public Type(string name, TypeMode mode, Dictionary<string, Type> fields)
    {
        Name = name;
        Mode = mode;
        Fields = fields;
    }

    public string Name { get; }
    public TypeMode Mode { get; }
    public Dictionary<string, Type> Fields { get; }

    public static Type FromString(string typeName) =>
        typeof(Type).GetFields(BindingFlags.Public | BindingFlags.Static)
            .FirstOrDefault(fi => fi.Name == typeName)
            ?.GetValue(null) as Type ?? throw new InvalidOperationException($"Unknown type: {typeName}");

    public override string ToString() => Name;
    
    public enum TypeMode
    {
        Struct,
        Union,
        Flags,
    }

    #region Equality
    public bool Equals(Type? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return Name == other.Name && Mode == other.Mode && Fields.Equals(other.Fields);
    }

    public override bool Equals(object? obj)
    {
        if (ReferenceEquals(null, obj)) return false;
        if (ReferenceEquals(this, obj)) return true;
        if (obj.GetType() != this.GetType()) return false;
        return Equals((Type)obj);
    }

    public override int GetHashCode() => HashCode.Combine(Name, (int)Mode, Fields);

    public static bool operator ==(Type? left, Type? right) => Equals(left, right);

    public static bool operator !=(Type? left, Type? right) => !Equals(left, right);
    #endregion
}
