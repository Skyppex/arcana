using System.Reflection;
// ReSharper disable InconsistentNaming

namespace MyLang;

public class Type : IEquatable<Type>
{
    public static readonly Type never = new("Never");
    public static readonly Type i32 = new("i32");
    public static readonly Type f32 = new("f32");
    public static readonly Type @string = new("string");
    public static readonly Type @bool = new("bool");
    
    public Type(string name) => Name = name;

    public string Name { get; }

    public static Type FromString(string typeName) =>
        typeof(Type).GetFields(BindingFlags.Public | BindingFlags.Static)
            .FirstOrDefault(fi => fi.Name == typeName)
            ?.GetValue(null) as Type ?? throw new InvalidOperationException($"Unknown type: {typeName}");

    public override string ToString() => Name;
    
    #region Equality 
    public bool Equals(Type? other)
    {
        if (ReferenceEquals(null, other)) return false;
        if (ReferenceEquals(this, other)) return true;
        return Name == other.Name;
    }

    public override bool Equals(object? obj)
    {
        if (ReferenceEquals(null, obj)) return false;
        if (ReferenceEquals(this, obj)) return true;
        if (obj.GetType() != this.GetType()) return false;
        return Equals((Type)obj);
    }

    public override int GetHashCode() => Name.GetHashCode();

    public static bool operator ==(Type? left, Type? right) => Equals(left, right);

    public static bool operator !=(Type? left, Type? right) => !Equals(left, right);
    #endregion
}