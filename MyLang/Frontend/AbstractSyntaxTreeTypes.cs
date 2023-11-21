using System.Text;
using Monads;

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
    public Program(List<Result<IStatement, string>> body) => Body = body;

    public List<Result<IStatement, string>> Body { get; }

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();

        foreach (IStatement statement in Body.Flatten())
            builder.AppendLine(Indent.Get(indent) + statement.GetNodeTree(ref indent));

        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        foreach (IStatement statement in Body.Flatten())
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

public sealed class IfStatement : IStatement
{
    public IfStatement(ConditionBlock @if, Option<IEnumerable<ConditionBlock>> elseIfs, Option<IStatement> @else)
    {
        If = @if;
        ElseIfs = elseIfs;
        Else = @else;
    }
    
    public ConditionBlock If { get; }

    public Option<IEnumerable<ConditionBlock>> ElseIfs { get; }
    
    public Option<IStatement> Else { get; }
    
    
    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(IfStatement)}>");
        
        indent++;
        builder.AppendLine(Indent.Get(indent) + $"If: {If.Condition.GetNodeTree(ref indent)}");
        indent++;
        builder.AppendLine(Indent.Get(indent) + $"Block: {If.Block.GetNodeTree(ref indent)}");
        indent--;

        if (ElseIfs.IsSome())
        {
            List<ConditionBlock> elseIfs = ElseIfs.Unwrap().ToList();

            foreach (ConditionBlock elseIf in elseIfs)
            {
                builder.AppendLine(Indent.Get(indent) + $"ElseIf: {elseIf.Condition.GetNodeTree(ref indent)}");
                indent++;
                builder.AppendLine(Indent.Get(indent) + $"Block: {elseIf.Block.GetNodeTree(ref indent)}");
                indent--;
            }
        }

        if (!Else.IsSome())
            return builder.ToString();
        
        IStatement @else = Else.Unwrap();
        builder.AppendLine(Indent.Get(indent) + $"Else Block: {@else.GetNodeTree(ref indent)}");

        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
        If.Condition.Traverse(action);
        If.Block.Traverse(action);

        if (ElseIfs.IsSome())
        {
            List<ConditionBlock> elseIfs = ElseIfs.Unwrap().ToList();

            foreach (ConditionBlock elseIf in elseIfs)
            {
                elseIf.Condition.Traverse(action);
                elseIf.Block.Traverse(action);
            }
        }

        if (Else.IsSome())
            Else.Unwrap().Traverse(action);
    }

    public sealed class ConditionBlock
    {
        public ConditionBlock(IExpression condition, IStatement block)
        {
            Condition = condition;
            Block = block;
        }
        
        public IExpression Condition { get; }
        public IStatement Block { get; }
    }
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

public sealed class BooleanLiteral : IExpression
{
    public BooleanLiteral(bool value) => Value = value;
    
    public bool Value { get; set; }
    
    public string GetNodeTree(ref int indent) => $"<{nameof(BooleanLiteral)}>, Value: {Value}";

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

public sealed class UnaryExpression : IExpression
{
    public UnaryExpression(string @operator, IExpression operand)
    {
        Operator = @operator;
        Operand = operand;
    }
    
    public string Operator { get; }
    public IExpression Operand { get; }
    
    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(UnaryExpression)}>");

        indent++;
        builder.AppendLine(Indent.Get(indent) + $"Operator: '{Operator}'");
        builder.Append(Indent.Dash(indent) + $"Operand: {Operand.GetNodeTree(ref indent)}");
        indent--;

        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
        Operand.Traverse(action);
    }
}

public sealed class TernaryExpression : IExpression
{
    public TernaryExpression(IExpression condition, IExpression @true, IExpression @false)
    {
        Condition = condition;
        True = @true;
        False = @false;
    }
    
    public IExpression Condition { get; }
    public IExpression True { get; }
    public IExpression False { get; }
    
    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(TernaryExpression)}>");

        indent++;
        builder.AppendLine(Indent.Dash(indent) + $"Condition: {Condition.GetNodeTree(ref indent)}");
        builder.AppendLine(Indent.Get(indent) + $"Then: {True.GetNodeTree(ref indent)}");
        builder.Append(Indent.Dash(indent) + $"Else: {False.GetNodeTree(ref indent)}");
        indent--;

        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
        Condition.Traverse(action);
        True.Traverse(action);
        False.Traverse(action);
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

    public void Traverse(Action<INode> action)
    {
        action(this);
        Identifier.Traverse(action);
    }
}