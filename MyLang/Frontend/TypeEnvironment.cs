namespace MyLang;

public class TypeEnvironment
{
    public static TypeEnvironment Create() => new()
    {
        _typesByName =
        {
            ["never"] = Type.never,
            ["i32"] = Type.i32,
            ["f32"] = Type.f32,
            ["string"] = Type.@string,
            ["bool"] = Type.@bool,
        }
    };
    
    private readonly Dictionary<string, Type> _typesByName;
    
    public TypeEnvironment(TypeEnvironment? parent = null)
    {
        _typesByName = new Dictionary<string, Type>();
        Parent = parent;
    }

    public TypeEnvironment? Parent { get; }
    
    public Type Define(string name, Type type)
    {
        _typesByName[name] = type;
        return type;
    }
    
    public bool IsDefined(string name) => _typesByName.ContainsKey(name);
    
    public bool Lookup(string name, out Type? type)
    {
        if (_typesByName.TryGetValue(name, out Type? t))
        {
            type = t;
            return true;
        }

        if (Parent?.Lookup(name, out t) ?? false)
        {
            type = t;
            return true;
        }

        type = null;
        return false;
    }
}