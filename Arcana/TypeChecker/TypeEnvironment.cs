﻿namespace Arcana.TypeChecker;

public class TypeEnvironment(TypeEnvironment? parent = null)
{
    public static TypeEnvironment Create() => new()
    {
        _typesByName =
        {
            ["void"] = Type.@void,
            ["i32"] = Type.i32,
            ["f32"] = Type.f32,
            ["string"] = Type.@string,
            ["char"] = Type.@char,
            ["bool"] = Type.@bool,
        }
    };
    
    private readonly Dictionary<string, Type> _typesByName = new();
    private readonly Dictionary<string, Type> _typesByVariableName = new();

    private TypeEnvironment? Parent { get; } = parent;

    public Type DefineType(Type type)
    {
        _typesByName[type.Name] = type;
        return type;
    }

    public Type DefineVariable(string name, Type type)
    {
        _typesByVariableName[name] = type;
        return type;
    }
    
    public Type DefineVariable(KeyValuePair<string, Type> field) => DefineVariable(field.Key, field.Value);

    public bool IsTypeDefined(string name) => _typesByName.ContainsKey(name);
    public bool IsVariableDefined(string name) => _typesByVariableName.ContainsKey(name);
    
    public bool LookupType<T>(string name, out T? type)
        where T : Type
    {
        if (_typesByName.TryGetValue(name, out Type? t))
        {
            if (t is T value)
            {
                type = value;
                return true;
            }

            type = null;
            return false;
        }

        if (Parent?.LookupType(name, out t) ?? false)
        {
            if (t is T value)
            {
                type = value;
                return true;
            }
            
            type = null;
            return false;
        }

        type = null;
        return false;
    }
    
    public bool LookupVariable(string name, out Type? type, Type? accessorRootType = null)
    {
        if (_typesByVariableName.TryGetValue(name, out Type? t))
        {
            type = t;
            return true;
        }

        if (Parent?.LookupVariable(name, out t, accessorRootType) ?? false)
        {
            type = t;
            return true;
        }

        type = null;
        return false;
    }
}