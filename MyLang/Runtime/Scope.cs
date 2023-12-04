namespace MyLang.Runtime;

public class Scope
{
    private readonly Scope? _parent;
    private readonly Dictionary<string, Variable> _variables = new();

    public IReadOnlyDictionary<string, IRuntimeValue> Variables
    {
        get
        {
            Dictionary<string, IRuntimeValue> variables = _variables.Keys.ToDictionary(name => name, name => _variables[name].Value);
            return _parent is null ? variables : variables.Concat(_parent.Variables).ToDictionary(pair => pair.Key, pair => pair.Value);
        }
    } 
    
    public Scope(Scope? parent = null) => _parent = parent;
    
    public bool HasDefinedVariable(string name) => _variables.ContainsKey(name);
    public bool HasDefinedVariable(Identifier identifier) => HasDefinedVariable(identifier.Symbol);
    public bool CanAccessVariable(string name) => HasDefinedVariable(name) || _parent?.CanAccessVariable(name) == true;
    public bool CanAccessVariable(Identifier identifier) => CanAccessVariable(identifier.Symbol);
    
    public IRuntimeValue Declare(Identifier identifier, bool mutable, IRuntimeValue value)
    {
        string name = identifier.Symbol;
        _variables.Remove(name);
        _variables.Add(name, new Variable(mutable, value));
        return value;
    }
    
    public IRuntimeValue Assign(MemberExpression member, IRuntimeValue value)
    {
        switch (member)
        {
            case Identifier identifier:
            {
                string name = identifier.Symbol;
                Variable variable = Resolve(name)._variables[name];
            
                if (variable.Value is not Uninitialized && !variable.Mutable)
                    throw new Exception($"Variable '{name}' is not mutable.");
            
                return variable.Value = value;
            }

            case MemberAccessExpression memberAccess:
                return Assign(memberAccess.Member, value);

            default:
                throw new Exception($"Invalid member expression: {member}");
        }
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
    
    public IRuntimeValue MakeBool(string name, bool value) => Declare(new Identifier(name), false, new BooleanValue(value));

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