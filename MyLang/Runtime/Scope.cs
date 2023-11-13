namespace MyLang.Runtime;

public class Scope
{
    private readonly Scope? _parent;
    private readonly Dictionary<string, Variable> _variables = new();
    private readonly List<string> _typeNames = new();

    public IReadOnlyDictionary<string, IRuntimeValue> Variables
    {
        get
        {
            Dictionary<string, IRuntimeValue> variables = _variables.Keys.ToDictionary(name => name, name => _variables[name].Value);
            return _parent is null ? variables : _parent.Variables.Concat(variables).ToDictionary(pair => pair.Key, pair => pair.Value);
        }
    } 
    
    public Scope(Scope? parent = null) => _parent = parent;
    
    public IRuntimeValue Declare(Identifier identifier, bool mutable, IRuntimeValue value)
    {
        string name = identifier.Symbol;
        
        if (_variables.ContainsKey(name))
            throw new Exception($"Variable '{name}' already exists in this scope.");
        
        _variables.Add(name, new Variable(mutable, value));
        return value;
    }
    
    public IRuntimeValue Assign(Identifier identifier, IRuntimeValue value)
    {
        string name = identifier.Symbol;
        Variable variable = Resolve(name)._variables[name];
        
        if (!variable.Mutable)
            throw new Exception($"Variable '{name}' is not mutable.");
        
        return variable.Value = value;
    }

    public IRuntimeValue Get(Identifier identifier)
    {
        string name = identifier.Symbol;
        return Resolve(name)._variables[name].Value;
    }

    public IRuntimeValue Drop(Identifier identifier)
    {
        string name = identifier.Symbol;
        
        return Resolve(name)._variables.Remove(name)
            ? Uninitialized.Instance
            : throw new Exception($"Variable '{name}' does not exist.");
    }

    public Scope Resolve(string name)
    {
        if (_variables.ContainsKey(name))
            return this;

        if (_parent is not null)
            return _parent.Resolve(name);
        
        throw new Exception($"Variable '{name}' does not exist in this scope.");
    }
    
    public Scope ResolveTypeName(string name)
    {
        if (_typeNames.Contains(name))
            return this;

        if (_parent is not null)
            return _parent.ResolveTypeName(name);
        
        throw new Exception($"Type '{name}' does not exist in this scope.");
    }
    
    public IRuntimeValue MakeBool(string name, bool value) => Declare(new Identifier(name), false, new BooleanValue(value));

    public void DeclareType(string name)
    {
        if (_typeNames.Contains(name))
            throw new Exception($"Type '{name}' already exists in this scope.");
        
        _typeNames.Add(name);
    }
    
    private class Variable
    {
        public Variable(bool mutable, IRuntimeValue value)
        {
            Mutable = mutable;
            Value = value;
        }
        
        public bool Mutable { get; }
        public IRuntimeValue Value { get; set; }
    }
}