using System.Text;

namespace MyLang;

public static class Indent
{
    public static string Get(int indent) => new(' ', indent * 4);
    public static string Dash(int indent) => new string(' ', (indent - 1) * 4) + "  - ";
}

public interface INode
{
    public string GetNodeTree(ref int indent);

    public void Traverse(Action<INode> action);
}

public interface IStatement : INode { }

public interface IExpression : IStatement { }

public sealed class Program : IStatement
{
    public Program(List<IStatement> body) => Body = body;

    public List<IStatement> Body { get; }

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();

        foreach (IStatement statement in Body)
            builder.AppendLine(Indent.Get(indent) + statement.GetNodeTree(ref indent));

        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        foreach (IStatement statement in Body)
        {
            statement.Traverse(action);
            action(this);
        }
    }
}

public sealed class VariableDeclarationStatement : IStatement
{
    public VariableDeclarationStatement(string typeName, bool mutable, Identifier identifier)
    {
        TypeName = typeName;
        Mutable = mutable;
        Identifier = identifier;
    }

    public string TypeName { get; }
    public bool Mutable { get; }
    public Identifier Identifier { get; }

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(VariableDeclarationStatement)}>");
        
        indent++;
        builder.AppendLine(Indent.Get(indent) + $"TypeName: {TypeName}");
        builder.AppendLine(Indent.Get(indent) + $"Mutable: {Mutable}");
        builder.Append(Indent.Get(indent) + $"Identifier: {Identifier.GetNodeTree(ref indent)}");
        indent--;

        return builder.ToString();
    }

    public void Traverse(Action<INode> action) => action(this);
}

public sealed class VariableDeclarationExpression : IExpression
{
    public VariableDeclarationExpression(string typeName, bool mutable, Identifier identifier, IExpression initializer)
    {
        TypeName = typeName;
        Mutable = mutable;
        Identifier = identifier;
        Initializer = initializer;
    }

    public string TypeName { get; }
    public bool Mutable { get; }
    public Identifier Identifier { get; }
    public IExpression Initializer { get; }

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(VariableDeclarationExpression)}>");
        
        indent++;
        builder.AppendLine(Indent.Get(indent) + $"TypeName: {TypeName}");
        builder.AppendLine(Indent.Get(indent) + $"Mutable: {Mutable}");
        builder.AppendLine(Indent.Get(indent) + $"Identifier: {Identifier.GetNodeTree(ref indent)}");
        builder.Append(Indent.Dash(indent) + $"Initializer: {Initializer.GetNodeTree(ref indent)}");
        indent--;
        
        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
        Initializer.Traverse(action);
    }
}

public sealed class AssignmentExpression : IExpression
{
    public AssignmentExpression(Identifier identifier, IExpression assignment)
    {
        Identifier = identifier;
        Assignment = assignment;
    }
    
    public Identifier Identifier { get; }
    public IExpression Assignment { get; }
    
    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(AssignmentExpression)}>");
        
        indent++;
        builder.AppendLine(Indent.Get(indent) + $"Identifier: {Identifier.GetNodeTree(ref indent)}");
        builder.Append(Indent.Dash(indent) + $"Assignment: {Assignment.GetNodeTree(ref indent)}");
        indent--;
        
        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
        Assignment.Traverse(action);
    }
}

public sealed class Identifier : IExpression
{
    public Identifier(string symbol) => Symbol = symbol;
    
    public string Symbol { get; }

    public string GetNodeTree(ref int indent) => $"<{nameof(Identifier)}>, Symbol: {Symbol}";
    public void Traverse(Action<INode> action) => action(this);
}

public abstract class NumericLiteral : IExpression
{
    public abstract string GetNodeTree(ref int indent);
    public void Traverse(Action<INode> action) => action(this);
}

public sealed class Int32Literal : NumericLiteral
{
    public Int32Literal(int value) => Value = value;
    
    public int Value { get; }

    public override string GetNodeTree(ref int indent) => $"<{nameof(Int32Literal)}>, Value: {Value}";
}

public sealed class Float32Literal : NumericLiteral
{
    public Float32Literal(float value) => Value = value;
    
    public float Value { get; }
    
    public override string GetNodeTree(ref int indent) => $"<{nameof(Float32Literal)}>, Value: {Value}";
}

public sealed class StringLiteral : IExpression
{
    public StringLiteral(string value) => Value = value;
    
    public string Value { get; }
    
    public string GetNodeTree(ref int indent) => $"<{nameof(StringLiteral)}>, Value: {Value}";
    public void Traverse(Action<INode> action) => action(this);
}

public sealed class BinaryExpression : IExpression
{
    public BinaryExpression(IExpression left, string @operator, IExpression right)
    {
        Left = left;
        Operator = @operator;
        Right = right;
    }
    
    public IExpression Left { get; }
    public string Operator { get; }
    public IExpression Right { get; }
    
    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(BinaryExpression)}>");

        indent++;
        builder.AppendLine(Indent.Dash(indent) + $"Left: {Left.GetNodeTree(ref indent)}");
        builder.AppendLine(Indent.Get(indent) + $"Operator: '{Operator}'");
        builder.Append(Indent.Dash(indent) + $"Right: {Right.GetNodeTree(ref indent)}");
        indent--;

        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
        Left.Traverse(action);
        Right.Traverse(action);
    }
}

public sealed class DropExpression : IExpression
{
    public DropExpression(Identifier identifier) => Identifier = identifier;

    public Identifier Identifier { get; }
        
    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(DropExpression)}>");
        
        indent++;
        builder.Append(Indent.Get(indent) + $"Identifier: {Identifier.GetNodeTree(ref indent)}");
        indent--;
        
        return builder.ToString();
    }

    public void Traverse(Action<INode> action) => action(this);
}