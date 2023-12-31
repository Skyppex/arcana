﻿using Arcana;
using Arcana.Lexer;
using Arcana.Parser;

using static Arcana.Option;

namespace Mage.Interpreter;

public static class Interpreter
{
    public static IRuntimeValue Evaluate(IStatement statement, Environment environment)
    {
        return statement switch
        {
            Int32Literal numericLiteral => new Int32Value(numericLiteral.Value),
            Float32Literal numericLiteral => new Float32Value(numericLiteral.Value),
            StringLiteral stringLiteral => new StringValue(stringLiteral.Value),
            CharLiteral charLiteral => new CharValue(charLiteral.Value),
            BooleanLiteral booleanLiteral => new BooleanValue(booleanLiteral.Value),
            StructLiteral structLiteral => EvaluateStructLiteral(structLiteral, environment),
            UnionLiteral unionLiteral => EvaluateUnionLiteral(unionLiteral, environment),
            UnaryExpression unaryExpression when unaryExpression is
            {
                Operator: TokenSymbol.PLUS or TokenSymbol.MINUS or TokenSymbol.TILDE
            } => EvaluateNumberUnaryExpression(unaryExpression, Evaluate(unaryExpression.Operand, environment)),
            UnaryExpression unaryExpression when unaryExpression is
            {
                Operator: TokenSymbol.EXCLAMATION or TokenSymbol.TILDE
            } => EvaluateBooleanUnaryExpression(unaryExpression, Evaluate(unaryExpression.Operand, environment)),
            TernaryExpression ternaryExpression => EvaluateTernaryExpression(ternaryExpression, environment),
            BinaryExpression binaryExpression => EvaluateBinaryExpression(binaryExpression, environment),
            MemberAccessExpression memberExpression => EvaluateMemberExpression(memberExpression, environment),
            Identifier identifier => EvaluateIdentifier(identifier, environment),
            AssignmentExpression assignmentExpression => EvaluateAssignmentExpression(
                assignmentExpression,
                environment),
            VariableDeclarationStatement variableDeclarationStatement => EvaluateVariableDeclarationStatement(
                variableDeclarationStatement,
                environment),
            StructDeclarationStatement => EvaluateStructDeclarationStatement(),
            UnionDeclarationStatement => EvaluateUnionDeclarationStatement(),
            FunctionDeclarationStatement functionDeclarationStatement => EvaluateFunctionDeclarationStatement(functionDeclarationStatement, environment),
            IfStatement ifStatement => EvaluateIfStatement(ifStatement, environment),
            VariableDeclarationExpression variableDeclarationExpression => EvaluateVariableDeclarationExpression(
                variableDeclarationExpression,
                environment),
            BlockExpression blockExpression => EvaluateBlockExpression(blockExpression, environment),
            DropExpression dropExpression => EvaluateDropExpression(dropExpression, environment),
            ProgramStatement program => EvaluateProgram(program, environment),
            _ => throw new InvalidProgramException(
                $"The {statement.GetType()} Node has not been setup for interpretation.")
        };
    }

    private static IRuntimeValue EvaluateStructLiteral(
        StructLiteral structLiteral,
        Environment environment) =>
        new StructValue(structLiteral.FieldInitializers
            .ToDictionary(f => f.FieldIdentifier, f =>
            {
                IRuntimeValue value = Evaluate(f.Initializer, environment);
                // environment.Declare(new Identifier($"{structLiteral.Identifier}.{f.FieldIdentifier}"), true, value);
                return value;
            }));

    private static IRuntimeValue EvaluateUnionLiteral(
        UnionLiteral unionLiteral,
        Environment environment) =>
        new UnionValue(unionLiteral.UnionMember, unionLiteral.FieldInitializers
            .ToDictionary(f => f.FieldIdentifier, f => Evaluate(f.Initializer, environment)));

    private static IRuntimeValue EvaluateBinaryExpression(BinaryExpression binaryExpression, Environment environment)
    {
        IRuntimeValue lhs = Evaluate(binaryExpression.Left, environment);
        IRuntimeValue rhs = Evaluate(binaryExpression.Right, environment);
        
        if (lhs is EmptyProgramValue || rhs is EmptyProgramValue)
            throw new InvalidProgramException("Cannot evaluate an empty program.");

        if (lhs is NumberValue lhsNum && rhs is NumberValue rhsNum)
            return EvaluateNumberBinaryExpression(binaryExpression, lhsNum, rhsNum);
        
        if (lhs is TextValue lhsTxt && rhs is TextValue rhsTxt)
            return EvaluateTextBinaryExpression(binaryExpression, lhsTxt, rhsTxt);
        
        if (lhs is BooleanValue lhsBool && rhs is BooleanValue rhsBool)
            return EvaluateBooleanBinaryExpression(binaryExpression, lhsBool, rhsBool);
        
        throw new InvalidProgramException("Cannot do a binary operation on an expression that is not a number.");
    }
    
    private static IRuntimeValue EvaluateNumberUnaryExpression(UnaryExpression unaryExpression, IRuntimeValue value)
    {
        if (value is Int32Value int32Value)
        {
            switch (unaryExpression.Operator)
            {
                case TokenSymbol.PLUS:
                    return new Int32Value(int32Value.Value);
                
                case TokenSymbol.MINUS:
                    return new Int32Value(-int32Value.Value);
                
                case TokenSymbol.TILDE:
                    return new Int32Value(~int32Value.Value);

                default:
                    throw new InvalidProgramException($"The unary operator {unaryExpression.Operator} is not supported.");
            }
        }

        if (value is Float32Value float32Value)
        {
            switch (unaryExpression.Operator)
            {
                case TokenSymbol.PLUS:
                    return new Float32Value(float32Value.Value);
                
                case TokenSymbol.MINUS:
                    return new Float32Value(-float32Value.Value);
                
                default:
                    throw new InvalidProgramException($"The unary operator {unaryExpression.Operator} is not supported.");
            }
        }

        throw new InvalidProgramException($"Cannot do a unary operation '{unaryExpression.Operator}' on an expression that is not a number.");
    }

    private static IRuntimeValue EvaluateBooleanUnaryExpression(UnaryExpression unaryExpression, IRuntimeValue value)
    {
        if (value is BooleanValue boolValue)
        {
            switch (unaryExpression.Operator)
            {
                case TokenSymbol.EXCLAMATION:
                    return new BooleanValue(!boolValue.Value);
                
                default:
                    throw new InvalidProgramException($"The unary operator {unaryExpression.Operator} is not supported for booleans.");
            }
        }

        throw new InvalidProgramException($"The unary operator {unaryExpression.Operator} is not supported for booleans.");
    }

    private static IRuntimeValue EvaluateNumberBinaryExpression(BinaryExpression binaryExpression, NumberValue lhs, NumberValue rhs)
    {
        if (lhs is Int32Value lhsInt32 && rhs is Int32Value rhsInt32)
        {
            switch (binaryExpression.Operator)
            {
                case TokenSymbol.PLUS:
                    return new Int32Value(lhsInt32.Value + rhsInt32.Value);

                case TokenSymbol.MINUS:
                    return new Int32Value(lhsInt32.Value - rhsInt32.Value);

                case TokenSymbol.STAR:
                    return new Int32Value(lhsInt32.Value * rhsInt32.Value);

                case TokenSymbol.SLASH:
                    return new Int32Value(lhsInt32.Value / rhsInt32.Value); // TODO: Handle divide by zero.

                case TokenSymbol.PERCENT:
                    return new Int32Value(lhsInt32.Value % rhsInt32.Value);
                
                case TokenSymbol.LESS:
                    return new BooleanValue(lhsInt32.Value < rhsInt32.Value);

                case TokenSymbol.GREATER:
                    return new BooleanValue(lhsInt32.Value > rhsInt32.Value);

                case TokenSymbol.LOGICAL_LESS_EQUAL:
                    return new BooleanValue(lhsInt32.Value <= rhsInt32.Value);
                
                case TokenSymbol.LOGICAL_GREATER_EQUAL:
                    return new BooleanValue(lhsInt32.Value >= rhsInt32.Value);
                    
                case TokenSymbol.LOGICAL_EQUAL:
                    return new BooleanValue(lhsInt32.Value == rhsInt32.Value);
            
                case TokenSymbol.LOGICAL_NOT_EQUAL:
                    return new BooleanValue(lhsInt32.Value != rhsInt32.Value);

                default:
                    throw new InvalidProgramException($"The binary operator {binaryExpression.Operator} is not supported.");
            }
        }

        if (lhs is Float32Value lhsFloat32 && rhs is Float32Value rhsFloat32)
        {
            switch (binaryExpression.Operator)
            {
                case TokenSymbol.PLUS:
                    return new Float32Value(lhsFloat32.Value + rhsFloat32.Value);

                case TokenSymbol.MINUS:
                    return new Float32Value(lhsFloat32.Value - rhsFloat32.Value);

                case TokenSymbol.STAR:
                    return new Float32Value(lhsFloat32.Value * rhsFloat32.Value);

                case TokenSymbol.SLASH:
                    return new Float32Value(lhsFloat32.Value / rhsFloat32.Value); // TODO: Handle divide by zero.
                
                case TokenSymbol.PERCENT:
                    return new Float32Value(lhsFloat32.Value % rhsFloat32.Value);

                case TokenSymbol.LESS:
                    return new BooleanValue(lhsFloat32.Value < rhsFloat32.Value);

                case TokenSymbol.GREATER:
                    return new BooleanValue(lhsFloat32.Value > rhsFloat32.Value);

                case TokenSymbol.LOGICAL_LESS_EQUAL:
                    return new BooleanValue(lhsFloat32.Value <= rhsFloat32.Value);
                
                case TokenSymbol.LOGICAL_GREATER_EQUAL:
                    return new BooleanValue(lhsFloat32.Value >= rhsFloat32.Value);
                    
                case TokenSymbol.LOGICAL_EQUAL:
                    return new BooleanValue(lhsFloat32.Value == rhsFloat32.Value);
            
                case TokenSymbol.LOGICAL_NOT_EQUAL:
                    return new BooleanValue(lhsFloat32.Value != rhsFloat32.Value);
                
                default:
                    throw new InvalidProgramException($"The binary operator {binaryExpression.Operator} is not supported.");
            }
        }

        throw new InvalidProgramException("Cannot do a binary operation on an expression that is not a number.");
    }

    private static IRuntimeValue EvaluateTextBinaryExpression(BinaryExpression binaryExpression, TextValue lhs, TextValue rhs)
    {
        switch (binaryExpression.Operator)
        {
            case TokenSymbol.PLUS:
                return new StringValue(lhs.Text + rhs.Text);
            
            case TokenSymbol.LOGICAL_EQUAL:
                return new BooleanValue(lhs.Text == rhs.Text);
            
            case TokenSymbol.LOGICAL_NOT_EQUAL:
                return new BooleanValue(lhs.Text != rhs.Text);

            default:
                throw new InvalidProgramException($"The binary operator {binaryExpression.Operator} is not supported.");
        }
    }

    private static IRuntimeValue EvaluateBooleanBinaryExpression(BinaryExpression binaryExpression, BooleanValue lhs, BooleanValue rhs)
    {
        switch (binaryExpression.Operator)
        {
            case TokenSymbol.LOGICAL_AND:
                return new BooleanValue(lhs.Value && rhs.Value);

            case TokenSymbol.LOGICAL_OR:
                return new BooleanValue(lhs.Value || rhs.Value);
            
            case TokenSymbol.LOGICAL_EQUAL:
                return new BooleanValue(lhs.Value == rhs.Value);

            case TokenSymbol.LOGICAL_NOT_EQUAL:
                return new BooleanValue(lhs.Value != rhs.Value);

            default:
                throw new InvalidProgramException($"The binary operator {binaryExpression.Operator} is not supported.");
        }
    }

    private static IRuntimeValue EvaluateMemberExpression(MemberAccessExpression memberAccessExpression, Environment environment)
    {
        IRuntimeValue objectValue = Evaluate(memberAccessExpression.Object, environment);

        switch (objectValue)
        {
            case StructValue structValue when structValue.Fields.TryGetValue(memberAccessExpression.Member.Symbol, out IRuntimeValue? value):
                return value;
            
            case UnionValue unionValue when unionValue.Fields.TryGetValue(memberAccessExpression.Member.Symbol, out IRuntimeValue? value):
                return value;
        }
        
        throw new Exception($"Member '{memberAccessExpression.Member.Symbol}' does not exist on type '{objectValue.GetType()}'.");
    }
    
    private static IRuntimeValue EvaluateIdentifier(Identifier identifier, Environment environment) => environment.Get(identifier);

    private static IRuntimeValue EvaluateTernaryExpression(TernaryExpression ternaryExpression, Environment environment)
    {
        var fallThrough = new Environment(environment, "TernaryFallThrough");
        IRuntimeValue condition = Evaluate(ternaryExpression.Condition, fallThrough);
        
        if (condition is not BooleanValue booleanValue)
            throw new Exception($"Condition must be a boolean value, but was {condition.GetType()}.");

        if (booleanValue.Value)
            return Evaluate(ternaryExpression.Then, fallThrough);
        
        return Evaluate(ternaryExpression.Else, fallThrough);
    }
    
    private static IRuntimeValue EvaluateAssignmentExpression(AssignmentExpression assignmentExpression, Environment environment)
    {
        IRuntimeValue value = Evaluate(assignmentExpression.Assignment, environment);
        environment.Assign(assignmentExpression.Member, value);
        return value;
    }
    
    private static IRuntimeValue EvaluateVariableDeclarationStatement(
        VariableDeclarationStatement variableDeclarationStatement,
        Environment environment)
    {
        environment.Declare(variableDeclarationStatement.Identifier, variableDeclarationStatement.Mutable, Uninitialized.Instance);
        return Uninitialized.Instance;
    }

    private static IRuntimeValue EvaluateStructDeclarationStatement() => NoValue.Instance;
    private static IRuntimeValue EvaluateUnionDeclarationStatement() => NoValue.Instance;
    private static IRuntimeValue EvaluateFunctionDeclarationStatement(FunctionDeclarationStatement functionDeclarationStatement, Environment environment)
    {
        var function = new FunctionValue(
            functionDeclarationStatement.FunctionIdentifier,
            functionDeclarationStatement.Parameters,
            environment,
            functionDeclarationStatement.Body);
        
        return environment.Declare(new Identifier(functionDeclarationStatement.FunctionIdentifier), false, function);
    }

    private static IRuntimeValue EvaluateIfStatement(
        IfStatement ifStatement,
        Environment environment)
    {
        var fallThrough = new Environment(environment, "IfFallThroughBlock");
        IRuntimeValue condition = Evaluate(ifStatement.If.Condition, fallThrough);
        
        if (condition is not BooleanValue booleanValue)
            throw new Exception($"Condition must be a boolean value, but was {condition.GetType().Name}.");

        if (booleanValue.Value)
            return Evaluate(ifStatement.If.Block, fallThrough);

        var resultOfElifs = ifStatement.ElseIfs.Match(
            some: elifs =>
            {
                foreach (var elseIf in elifs)
                {
                    fallThrough = new Environment(environment, "ElifBlock");
                    IRuntimeValue elifCondition = Evaluate(elseIf.Condition, fallThrough);

                    if (elifCondition is not BooleanValue elifBooleanValue)
                        throw new Exception($"Condition must be a boolean value, but was {elifCondition.GetType()}.");

                    if (elifBooleanValue.Value)
                        return Some(Evaluate(elseIf.Block, fallThrough));
                }
                
                return None<IRuntimeValue>();
            },
            none: None<IRuntimeValue>);
        
        if (resultOfElifs.IsSome())
            return resultOfElifs.Unwrap();

        const string FALL_THROUGH_NAME = "ElseFallThroughBlock";
        fallThrough.Name = FALL_THROUGH_NAME;

        var hasElifs = ifStatement.ElseIfs.Match(
            some: elifs => elifs.Any(),
            none: false);

        Environment elseEnvironment = hasElifs ? new Environment(environment, FALL_THROUGH_NAME) : fallThrough;
        
        return ifStatement.Else.Match(
            some: e => Evaluate(e, elseEnvironment),
            none: () => NoValue.Instance);
    }
    
    private static IRuntimeValue EvaluateVariableDeclarationExpression(
        VariableDeclarationExpression variableDeclarationExpression,
        Environment environment)
    {
        IRuntimeValue value = Evaluate(variableDeclarationExpression.Initializer, environment);
        environment.Declare(variableDeclarationExpression.Identifier, variableDeclarationExpression.Mutable, value);
        return value;
    }

    private static IRuntimeValue EvaluateBlockExpression(
        BlockExpression blockExpression,
        Environment environment)
    {
        IRuntimeValue lastEvaluated = NoValue.Instance;

        foreach (IStatement statement in blockExpression.Statements)
            lastEvaluated = Evaluate(statement, new Environment(environment, "Block"));

        return lastEvaluated;
    }

    private static IRuntimeValue EvaluateDropExpression(DropExpression dropExpression, Environment environment)
    {
        IRuntimeValue value = Evaluate(dropExpression.Identifier, environment);
        environment.Drop(dropExpression.Identifier);
        return value;
    }
    
    private static IRuntimeValue EvaluateProgram(ProgramStatement program, Environment environment)
    {
        IRuntimeValue lastEvaluated = new EmptyProgramValue();

        foreach (Result<IStatement, string> statement in program.Body)
        {
            IRuntimeValue evaluated = lastEvaluated;

            lastEvaluated = statement.Match(
                ok: s => Evaluate(s, environment),
                error: e =>
                {
                    Console.WriteLine($"ERROR: {e}");
                    return evaluated;
                });
        }

        return lastEvaluated;
    }
}