﻿namespace MyLang;

public class TypeEnvironment
{
    public static TypeEnvironment Create() => new()
    {
        _types =
        {
            ["never"] = Type.never,
            ["i32"] = Type.i32,
            ["f32"] = Type.f32,
            ["string"] = Type.@string,
            ["bool"] = Type.@bool,
        }
    };
    
    private readonly Dictionary<string, Type> _types;
    
    public TypeEnvironment(TypeEnvironment? parent = null)
    {
        _types = new Dictionary<string, Type>();
        Parent = parent;
    }

    public TypeEnvironment? Parent { get; }
    
    public Type Define(string name, Type type)
    {
        _types[name] = type;
        return type;
    }
    
    public bool Lookup(string name, out Type? type)
    {
        if (_types.TryGetValue(name, out Type? t))
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