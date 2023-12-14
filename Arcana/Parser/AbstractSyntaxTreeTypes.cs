using System.Text;

using Arcana.Models;

namespace Arcana.Parser;

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

public interface IStatement : INode;

public interface IExpression : IStatement;

public sealed class ProgramStatement(List<Result<IStatement, string>> body) : IStatement
{
    public List<Result<IStatement, string>> Body { get; } = body;

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

public sealed class VariableDeclarationStatement(string typeName, bool mutable, Identifier identifier) : IStatement
{
    public string TypeName { get; } = typeName;
    public bool Mutable { get; } = mutable;
    public Identifier Identifier { get; } = identifier;

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(VariableDeclarationStatement)}>");
        
        indent++;
        builder.AppendLine(Indent.Get(indent) + $"{nameof(TypeName)}: {TypeName}");
        builder.AppendLine(Indent.Get(indent) + $"{nameof(Mutable)}: {Mutable}");
        builder.Append(Indent.Get(indent) + $"{nameof(Identifier)}: {Identifier.GetNodeTree(ref indent)}");
        indent--;

        return builder.ToString();
    }

    public void Traverse(Action<INode> action) => action(this);
}

public sealed class IfStatement(
    IfStatement.ConditionBlock @if,
    Option<IEnumerable<IfStatement.ConditionBlock>> elseIfs,
    Option<IExpression> @else)
    : IStatement
{
    public ConditionBlock If { get; } = @if;

    public Option<IEnumerable<ConditionBlock>> ElseIfs { get; } = elseIfs;

    public Option<IExpression> Else { get; } = @else;

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(IfStatement)}>");
        
        indent++;
        builder.AppendLine(Indent.Get(indent) + $"{nameof(If)}: {If.Condition.GetNodeTree(ref indent)}");
        indent++;
        builder.AppendLine(Indent.Get(indent) + $"{nameof(If.Block)}: {If.Block.GetNodeTree(ref indent)}");
        indent--;

        if (ElseIfs.IsSome())
        {
            List<ConditionBlock> elseIfs = ElseIfs.Unwrap().ToList();

            foreach (ConditionBlock elseIf in elseIfs)
            {
                builder.AppendLine(Indent.Get(indent) + $"ElseIf: {elseIf.Condition.GetNodeTree(ref indent)}");
                indent++;
                builder.AppendLine(Indent.Get(indent) + $"{nameof(elseIf.Block)}: {elseIf.Block.GetNodeTree(ref indent)}");
                indent--;
            }
        }

        if (!Else.IsSome())
            return builder.ToString();
        
        IExpression @else = Else.Unwrap();
        builder.AppendLine(Indent.Get(indent) + $"{nameof(Else)} Block: {@else.GetNodeTree(ref indent)}");
        indent--;
        
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

    public sealed class ConditionBlock(IExpression condition, IExpression block)
    {
        public IExpression Condition { get; } = condition;
        public IExpression Block { get; } = block;
    }
}

public sealed class StructDeclarationStatement(
    Option<string> accessModifier,
    string typeName,
    IEnumerable<StructDeclarationStatement.Field> fields)
    : IStatement
{
    public Option<string> AccessModifier { get; } = accessModifier;
    public string TypeName { get; } = typeName;
    public IEnumerable<Field> Fields { get; } = fields;

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(StructDeclarationStatement)}>");
        
        indent++;
        builder.Append(Indent.Get(indent) + $"{nameof(TypeName)}: {TypeName}");

        if (Fields.Any())
        {
            builder.AppendLine();
            builder.Append(Indent.Get(indent) + $"{nameof(Fields)}:");
            indent++;
            
            foreach (Field field in Fields)
            {
                builder.AppendLine();
                builder.AppendLine(Indent.Get(indent) + $"{nameof(Field)}:");
                indent++;
                builder.AppendLine(Indent.Get(indent) + $"{nameof(field.AccessModifier)}: {field.AccessModifier}");
                builder.AppendLine(Indent.Get(indent) + $"{nameof(field.Mutable)}: {field.Mutable}");
                builder.AppendLine(Indent.Get(indent) + $"{nameof(field.TypeName)}: {field.TypeName}");
                builder.Append(Indent.Get(indent) + $"{nameof(field.Identifier)}: {field.Identifier}");
                indent--;
            }
            indent--;
        }
        indent--;
        
        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
    }

    public sealed class Field(Option<string> accessModifier, bool mutable, string typeName, string identifier)
    {
        public Option<string> AccessModifier { get; } = accessModifier;
        public bool Mutable { get; } = mutable;
        public string TypeName { get; } = typeName;
        public string Identifier { get; } = identifier;
    }
}

public sealed class UnionDeclarationStatement(
    Option<string> accessModifier,
    string typeName,
    IEnumerable<UnionDeclarationStatement.Member> members)
    : IStatement
{
    public Option<string> AccessModifier { get; } = accessModifier;
    public string TypeName { get; } = typeName;
    public IEnumerable<Member> Members { get; } = members;

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(StructDeclarationStatement)}>");
        
        indent++;
        builder.Append(Indent.Get(indent) + $"{nameof(TypeName)}: {TypeName}");

        if (Members.Any())
        {
            builder.AppendLine();
            builder.Append(Indent.Dash(indent) + $"{nameof(Members)}:");

            indent++;
            foreach (Member member in Members)
            {
                builder.AppendLine();
                builder.AppendLine(Indent.Dash(indent) + $"{nameof(Member)}: {nameof(member.Identifier)}: {member.Identifier}");
                
                if (member.Fields.Count == 0)
                    continue;
                
                indent++;
                builder.Append(Indent.Dash(indent) + $"{nameof(member.Fields)}:");
                indent++;
                foreach (Member.Field field in member.Fields)
                {
                    builder.AppendLine();
                    builder.AppendLine(Indent.Dash(indent) + $"{nameof(Member.Field)}:");
                    indent++;
                    builder.AppendLine(Indent.Get(indent) + $"{nameof(field.TypeName)}: {field.TypeName}");
                    builder.Append(Indent.Get(indent) + $"{nameof(field.Identifier)}: {field.Identifier}");
                    indent--;
                }
                indent--;
                indent--;
            }
            indent--;
        }
        indent--;
        
        return builder.ToString();
    }

    public void Traverse(Action<INode> action) => action(this);

    public sealed class Member(string identifier, List<Member.Field> fields)
    {
        public string Identifier { get; } = identifier;
        public List<Field> Fields { get; } = fields;

        public sealed class Field(string typeName, string identifier)
        {
            public string TypeName { get; } = typeName;
            public string Identifier { get; } = identifier;
        }
    }
}

public sealed class FunctionDeclarationStatement(
    Option<string> accessModifier,
    string functionIdentifier,
    IEnumerable<FunctionDeclarationStatement.Parameter> parameters,
    Option<string> returnTypeName,
    IExpression body)
    : IStatement
{
    public Option<string> AccessModifier { get; } = accessModifier;
    public string FunctionIdentifier { get; } = functionIdentifier;
    public IEnumerable<Parameter> Parameters { get; } = parameters;
    public Option<string> ReturnTypeName { get; } = returnTypeName;
    public IExpression Body { get; } = body;

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(FunctionDeclarationStatement)}>");
        
        indent++;
        builder.AppendLine(Indent.Get(indent) + $"{nameof(FunctionIdentifier)}: {FunctionIdentifier}");
        builder.AppendLine(Indent.Get(indent) + $"{nameof(ReturnTypeName)}: {ReturnTypeName}");

        if (Parameters.Any())
        {
            builder.AppendLine();
            builder.Append(Indent.Dash(indent) + $"{nameof(Parameters)}:");
            indent++;
            
            foreach (Parameter parameter in Parameters)
            {
                builder.AppendLine();
                builder.AppendLine(Indent.Get(indent) + $"{nameof(Parameter)}:");
                indent++;
                builder.AppendLine(Indent.Get(indent) + $"{nameof(parameter.TypeName)}: {parameter.TypeName}");
                builder.Append(Indent.Get(indent) + $"{nameof(parameter.Identifier)}: {parameter.Identifier}");
                indent--;
            }
            indent--;
        }
        
        builder.AppendLine();
        builder.Append(Indent.Get(indent) + $"{nameof(Body)}: {Body.GetNodeTree(ref indent)}");
        indent--;
        
        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
        Body.Traverse(action);
    }

    public sealed class Parameter(string identifier, string typeName)
    {
        public string Identifier { get; } = identifier;
        public string TypeName { get; } = typeName;
    }
}

public sealed class VariableDeclarationExpression(
    string typeName,
    bool mutable,
    Identifier identifier,
    IExpression initializer)
    : IExpression
{
    public string TypeName { get; } = typeName;
    public bool Mutable { get; } = mutable;
    public Identifier Identifier { get; } = identifier;
    public IExpression Initializer { get; } = initializer;

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(VariableDeclarationExpression)}>");
        
        indent++;
        builder.AppendLine(Indent.Get(indent) + $"{nameof(TypeName)}: {TypeName}");
        builder.AppendLine(Indent.Get(indent) + $"{nameof(Mutable)}: {Mutable}");
        builder.AppendLine(Indent.Get(indent) + $"{nameof(Identifier)}: {Identifier.GetNodeTree(ref indent)}");
        builder.Append(Indent.Dash(indent) + $"{nameof(Initializer)}: {Initializer.GetNodeTree(ref indent)}");
        indent--;
        
        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
        Initializer.Traverse(action);
    }
}

public sealed class AssignmentExpression(MemberExpression member, IExpression assignment) : IExpression
{
    public MemberExpression Member { get; } = member;
    public IExpression Assignment { get; } = assignment;

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(AssignmentExpression)}>");
        
        indent++;
        builder.AppendLine(Indent.Get(indent) + $"{nameof(Member)}: {Member.GetNodeTree(ref indent)}");
        builder.Append(Indent.Dash(indent) + $"{nameof(Assignment)}: {Assignment.GetNodeTree(ref indent)}");
        indent--;
        
        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
        Assignment.Traverse(action);
    }
}

public abstract class MemberExpression : IExpression
{
    public abstract string Symbol { get; }
    public abstract string GetNodeTree(ref int indent);
    public abstract void Traverse(Action<INode> action);
}

public sealed class Identifier(string symbol) : MemberExpression
{
    public override string Symbol { get; } = symbol;

    public override string GetNodeTree(ref int indent) => $"<{nameof(Identifier)}>, {nameof(Symbol)}: {Symbol}";
    public override void Traverse(Action<INode> action) => action(this);
}

public abstract class NumericLiteral : IExpression
{
    public abstract string GetNodeTree(ref int indent);
    public void Traverse(Action<INode> action) => action(this);
}

public sealed class Int32Literal(int value) : NumericLiteral
{
    public int Value { get; } = value;

    public override string GetNodeTree(ref int indent) => $"<{nameof(Int32Literal)}>, {nameof(Value)}: {Value}";
}

public sealed class Float32Literal(float value) : NumericLiteral
{
    public float Value { get; } = value;

    public override string GetNodeTree(ref int indent) => $"<{nameof(Float32Literal)}>, {nameof(Value)}: {Value}";
}

public sealed class StringLiteral(string value) : IExpression
{
    public string Value { get; } = value;

    public string GetNodeTree(ref int indent) => $"<{nameof(StringLiteral)}>, {nameof(Value)}: \"{Value.ToLiteral()}\"";
    public void Traverse(Action<INode> action) => action(this);
}

public sealed class CharLiteral(char value) : IExpression
{
    public char Value { get; } = value;

    public string GetNodeTree(ref int indent) => $"<{nameof(CharLiteral)}>, {nameof(Value)}: '{Value.ToLiteral()}'";
    public void Traverse(Action<INode> action) => action(this);
}

public sealed class BooleanLiteral(bool value) : IExpression
{
    public bool Value { get; } = value;

    public string GetNodeTree(ref int indent) => $"<{nameof(BooleanLiteral)}>, {nameof(Value)}: {Value}";

    public void Traverse(Action<INode> action) => action(this);
}

public sealed class StructLiteral(string identifier, List<StructLiteral.FieldInitializer> fieldInitializers)
    : IExpression
{
    public string Identifier { get; } = identifier;
    public List<FieldInitializer> FieldInitializers { get; } = fieldInitializers;

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(StructLiteral)}>");

        indent++;
        builder.Append(Indent.Get(indent) + $"{nameof(Identifier)}: {Identifier}");

        foreach (FieldInitializer fieldInitializer in FieldInitializers)
        {
            builder.AppendLine();
            builder.AppendLine(Indent.Get(indent) + $"{nameof(FieldInitializer)}: ");
            indent++;

            builder.AppendLine(Indent.Get(indent) + $"{nameof(fieldInitializer.FieldIdentifier)}: {fieldInitializer.FieldIdentifier}");
            builder.Append(Indent.Get(indent) + $"{nameof(fieldInitializer.Initializer)}: {fieldInitializer.Initializer.GetNodeTree(ref indent)}");
            
            indent--;
        }
        
        indent--;
        
        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);

        foreach (FieldInitializer fieldInitializer in FieldInitializers)
            fieldInitializer.Initializer.Traverse(action);
    }

    public sealed class FieldInitializer(string fieldIdentifier, IExpression initializer)
    {
        public string FieldIdentifier { get; } = fieldIdentifier;
        public IExpression Initializer { get; } = initializer;
    }
}

public sealed class UnionLiteral(
    string identifier,
    string member,
    List<UnionLiteral.FieldInitializer> fieldInitializers)
    : IExpression
{
    public UnionMember UnionMember { get; } = new(identifier, member);
    public List<FieldInitializer> FieldInitializers { get; } = fieldInitializers;

    public string Identifier => UnionMember.TypeName;
    public string Member => UnionMember.MemberName;
    
    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(UnionLiteral)}>");

        indent++;
        builder.Append(Indent.Get(indent) + $"{nameof(UnionMember)}: {UnionMember}");

        foreach (FieldInitializer fieldInitializer in FieldInitializers)
        {
            builder.AppendLine();
            builder.AppendLine(Indent.Get(indent) + $"{nameof(FieldInitializer)}: ");
            indent++;

            builder.AppendLine(Indent.Get(indent) + $"{nameof(fieldInitializer.FieldIdentifier)}: {fieldInitializer.FieldIdentifier}");
            builder.Append(Indent.Get(indent) + $"{nameof(fieldInitializer.Initializer)}: {fieldInitializer.Initializer.GetNodeTree(ref indent)}");
            
            indent--;
        }
        
        indent--;
        
        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);

        foreach (FieldInitializer fieldInitializer in FieldInitializers)
            fieldInitializer.Initializer.Traverse(action);
    }

    public sealed class FieldInitializer(string fieldIdentifier, IExpression initializer)
    {
        public string FieldIdentifier { get; } = fieldIdentifier;
        public IExpression Initializer { get; } = initializer;
    }
}

public sealed class MemberAccessExpression(IExpression @object, MemberExpression member) : MemberExpression
{
    public IExpression Object { get; } = @object;
    public MemberExpression Member { get; } = member;
    public override string Symbol => Member.Symbol;
    
    public override string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(MemberAccessExpression)}>");

        indent++;
        builder.AppendLine(Indent.Get(indent) + $"{nameof(Object)}: {Object.GetNodeTree(ref indent)}");
        builder.Append(Indent.Get(indent) + $"{nameof(Member)}: {Member.GetNodeTree(ref indent)}");
        indent--;

        return builder.ToString();
    }

    public override void Traverse(Action<INode> action)
    {
        action(this);
        Object.Traverse(action);
        // Member.Traverse(action);
    }
}

public sealed class CallExpression(IExpression caller, List<IExpression> arguments) : IExpression
{
    public IExpression Caller { get; } = caller;
    public List<IExpression> Arguments { get; } = arguments;

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(CallExpression)}>");

        indent++;
        builder.AppendLine(Indent.Get(indent) + $"{nameof(Caller)}: {Caller.GetNodeTree(ref indent)}");

        if (Arguments.Any())
        {
            builder.AppendLine(Indent.Dash(indent) + $"{nameof(Arguments)}:");
            indent++;
            
            foreach (IExpression argument in Arguments)
                builder.AppendLine(Indent.Get(indent) + $"{argument.GetNodeTree(ref indent)}");
            
            indent--;
        }
        
        indent--;
        
        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
        Caller.Traverse(action);

        foreach (IExpression argument in Arguments)
            argument.Traverse(action);
    }
}

public sealed class BinaryExpression(IExpression left, string @operator, IExpression right) : IExpression
{
    public IExpression Left { get; } = left;
    public string Operator { get; } = @operator;
    public IExpression Right { get; } = right;

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(BinaryExpression)}>");

        indent++;
        builder.AppendLine(Indent.Dash(indent) + $"{nameof(Left)}: {Left.GetNodeTree(ref indent)}");
        builder.AppendLine(Indent.Get(indent) + $"{nameof(Operator)}: '{Operator}'");
        builder.Append(Indent.Dash(indent) + $"{nameof(Right)}: {Right.GetNodeTree(ref indent)}");
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

public sealed class UnaryExpression(string @operator, IExpression operand) : IExpression
{
    public string Operator { get; } = @operator;
    public IExpression Operand { get; } = operand;

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(UnaryExpression)}>");

        indent++;
        builder.AppendLine(Indent.Get(indent) + $"{nameof(Operator)}: '{Operator}'");
        builder.Append(Indent.Dash(indent) + $"{nameof(Operand)}: {Operand.GetNodeTree(ref indent)}");
        indent--;

        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
        Operand.Traverse(action);
    }
}

public sealed class TernaryExpression(IExpression condition, IExpression then, IExpression @else) : IExpression
{
    public IExpression Condition { get; } = condition;
    public IExpression Then { get; } = then;
    public IExpression Else { get; } = @else;

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(TernaryExpression)}>");

        indent++;
        builder.AppendLine(Indent.Dash(indent) + $"{nameof(Condition)}: {Condition.GetNodeTree(ref indent)}");
        builder.AppendLine(Indent.Get(indent) + $"{nameof(Then)}: {Then.GetNodeTree(ref indent)}");
        builder.Append(Indent.Dash(indent) + $"{nameof(Else)}: {Else.GetNodeTree(ref indent)}");
        indent--;

        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
        Condition.Traverse(action);
        Then.Traverse(action);
        Else.Traverse(action);
    }
}

public sealed class BlockExpression(List<IStatement> statements) : IExpression
{
    public List<IStatement> Statements { get; } = statements;

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.Append($"<{nameof(BlockExpression)}> {nameof(Statements)}:");

        indent++;

        if (Statements.Any())
        {
            indent++;
            
            foreach (IStatement statement in Statements)
            {
                builder.AppendLine();
                builder.Append(Indent.Get(indent) + $"{statement.GetNodeTree(ref indent)}");
            }

            indent--;
        }
        else
            builder.Append(" None");
        
        indent--;
        
        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
        
        foreach (IStatement statement in Statements)
            statement.Traverse(action);
    }
}

public sealed class DropExpression(Identifier identifier) : IExpression
{
    public Identifier Identifier { get; } = identifier;

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(DropExpression)}>");
        
        indent++;
        builder.Append(Indent.Get(indent) + $"{nameof(Identifier)}: {Identifier.GetNodeTree(ref indent)}");
        indent--;
        
        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
        Identifier.Traverse(action);
    }
}