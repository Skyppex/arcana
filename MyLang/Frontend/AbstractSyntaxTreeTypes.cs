using System.Text;
using Monads;
using MyLang.Models;

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
        builder.AppendLine(Indent.Get(indent) + $"{nameof(TypeName)}: {TypeName}");
        builder.AppendLine(Indent.Get(indent) + $"{nameof(Mutable)}: {Mutable}");
        builder.Append(Indent.Get(indent) + $"{nameof(Identifier)}: {Identifier.GetNodeTree(ref indent)}");
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
        
        IStatement @else = Else.Unwrap();
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

public sealed class StructDeclarationStatement : IStatement
{
    public StructDeclarationStatement(Option<string> accessModifier, string typeName, IEnumerable<Field> fields)
    {
        AccessModifier = accessModifier;
        TypeName = typeName;
        Fields = fields;
    }
    
    public Option<string> AccessModifier { get; }
    public string TypeName { get; }
    public IEnumerable<Field> Fields { get; }
    
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

    public sealed class Field
    {
        public Field(Option<string> accessModifier, bool mutable, string typeName, string identifier)
        {
            AccessModifier = accessModifier;
            Mutable = mutable;
            TypeName = typeName;
            Identifier = identifier;
        }
        
        public Option<string> AccessModifier { get; }
        public bool Mutable { get; }
        public string TypeName { get; }
        public string Identifier { get; }
    }
}

public sealed class UnionDeclarationStatement : IStatement
{
    public UnionDeclarationStatement(Option<string> accessModifier, string typeName, IEnumerable<Member> members)
    {
        AccessModifier = accessModifier;
        TypeName = typeName;
        Members = members;
    }
    
    public Option<string> AccessModifier { get; }
    public string TypeName { get; }
    public IEnumerable<Member> Members { get; }
    
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

    public sealed class Member
    {
        public Member(string identifier, List<Field> fields)
        {
            Identifier = identifier;
            Fields = fields;
        }
        
        public string Identifier { get; }
        public List<Field> Fields { get; }

        public sealed class Field
        {
            public Field(string typeName, string identifier)
            {
                TypeName = typeName;
                Identifier = identifier;
            }

            public string TypeName { get; }
            public string Identifier { get; }
        }
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

public sealed class AssignmentExpression : IExpression
{
    public AssignmentExpression(MemberExpression member, IExpression assignment)
    {
        Member = member;
        Assignment = assignment;
    }
    
    public MemberExpression Member { get; }
    public IExpression Assignment { get; }
    
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

public sealed class Identifier : MemberExpression
{
    public Identifier(string symbol) => Symbol = symbol;
    
    public override string Symbol { get; }

    public override string GetNodeTree(ref int indent) => $"<{nameof(Identifier)}>, {nameof(Symbol)}: {Symbol}";
    public override void Traverse(Action<INode> action) => action(this);
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

    public override string GetNodeTree(ref int indent) => $"<{nameof(Int32Literal)}>, {nameof(Value)}: {Value}";
}

public sealed class Float32Literal : NumericLiteral
{
    public Float32Literal(float value) => Value = value;
    
    public float Value { get; }
    
    public override string GetNodeTree(ref int indent) => $"<{nameof(Float32Literal)}>, {nameof(Value)}: {Value}";
}

public sealed class StringLiteral : IExpression
{
    public StringLiteral(string value) => Value = value;
    
    public string Value { get; }
    
    public string GetNodeTree(ref int indent) => $"<{nameof(StringLiteral)}>, {nameof(Value)}: {Value}";
    public void Traverse(Action<INode> action) => action(this);
}

public sealed class BooleanLiteral : IExpression
{
    public BooleanLiteral(bool value) => Value = value;
    
    public bool Value { get; set; }
    
    public string GetNodeTree(ref int indent) => $"<{nameof(BooleanLiteral)}>, {nameof(Value)}: {Value}";

    public void Traverse(Action<INode> action) => action(this);
}

public sealed class StructLiteral : IExpression
{
    public StructLiteral(string identifier, List<FieldInitializer> fieldInitializers)
    {
        Identifier = identifier;
        FieldInitializers = fieldInitializers;
    }
    
    public string Identifier { get; }
    public List<FieldInitializer> FieldInitializers { get; }
    
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

    public sealed class FieldInitializer
    {
        public FieldInitializer(string fieldIdentifier, IExpression initializer)
        {
            FieldIdentifier = fieldIdentifier;
            Initializer = initializer;
        }
        
        public string FieldIdentifier { get; }
        public IExpression Initializer { get; }
    }
}

public sealed class UnionLiteral : IExpression
{
    public UnionLiteral(string identifier, string member, List<FieldInitializer> fieldInitializers)
    {
        UnionMember = new UnionMember(identifier, member);
        FieldInitializers = fieldInitializers;
    }
    
    public UnionMember UnionMember { get; }
    public List<FieldInitializer> FieldInitializers { get; }

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

    public sealed class FieldInitializer
    {
        public FieldInitializer(string fieldIdentifier, IExpression initializer)
        {
            FieldIdentifier = fieldIdentifier;
            Initializer = initializer;
        }
        
        public string FieldIdentifier { get; }
        public IExpression Initializer { get; }
    }
}

public sealed class MemberAccessExpression : MemberExpression
{
    public MemberAccessExpression(IExpression @object, MemberExpression member)
    {
        Object = @object;
        Member = member;
    }

    public IExpression Object { get; }
    public MemberExpression Member { get; }
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

public sealed class CallExpression : IExpression
{
    public CallExpression(IExpression caller, List<IExpression> arguments)
    {
        Caller = caller;
        Arguments = arguments;
    }

    public IExpression Caller { get; }
    public List<IExpression> Arguments { get; }


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

public sealed class TernaryExpression : IExpression
{
    public TernaryExpression(IExpression condition, IExpression then, IExpression @else)
    {
        Condition = condition;
        Then = then;
        Else = @else;
    }
    
    public IExpression Condition { get; }
    public IExpression Then { get; }
    public IExpression Else { get; }
    
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

public sealed class BlockExpression : IExpression
{
    public BlockExpression(IExpression expression) => Expression = expression;
    public IExpression Expression { get; }

    public string GetNodeTree(ref int indent)
    {
        StringBuilder builder = new();
        builder.AppendLine($"<{nameof(BlockExpression)}>");

        indent++;
        builder.Append(Indent.Get(indent) + $"{nameof(Expression)}: {Expression.GetNodeTree(ref indent)}");
        indent--;
        
        return builder.ToString();
    }

    public void Traverse(Action<INode> action)
    {
        action(this);
        Expression.Traverse(action);
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