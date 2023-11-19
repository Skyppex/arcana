using Monads;

namespace MyLang.Runtime;

public class Interpreter
{
    public static IRuntimeValue Evaluate(IStatement statement, Scope scope, TypeEnvironment typeEnvironment)
    {
        switch (statement)
        {
            case Int32Literal numericLiteral:
                return new Int32Value(numericLiteral.Value);
            
            case Float32Literal numericLiteral:
                return new Float32Value(numericLiteral.Value);
            
            case StringLiteral stringLiteral:
                return new StringValue(stringLiteral.Value);
            
            case BooleanLiteral booleanLiteral:
                return new BooleanValue(booleanLiteral.Value);
            
            case UnaryExpression unaryExpression when unaryExpression is { Operator: TokenSymbol.ADD or TokenSymbol.SUBTRACT or TokenSymbol.BITWISE_NOT }:
                return EvaluateNumberUnaryExpression(unaryExpression, Evaluate(unaryExpression.Operand, scope, typeEnvironment));

            case UnaryExpression unaryExpression when unaryExpression is { Operator: TokenSymbol.LOGICAL_NOT or TokenSymbol.BITWISE_NOT }:
                return EvaluateBooleanUnaryExpression(unaryExpression, Evaluate(unaryExpression.Operand, scope, typeEnvironment));

            case BinaryExpression binaryExpression:
                return EvaluatePrimaryExpression(binaryExpression, scope, typeEnvironment);
            
            case Identifier identifier:
                return EvaluateIdentifier(identifier, scope);
            
            case AssignmentExpression assignmentExpression:
                return EvaluateAssignmentExpression(assignmentExpression, scope, typeEnvironment);
            
            case VariableDeclarationStatement variableDeclarationStatement:
                return EvaluateVariableDeclarationStatement(variableDeclarationStatement, scope, typeEnvironment);
            
            case VariableDeclarationExpression variableDeclarationExpression:
                return EvaluateVariableDeclarationExpression(variableDeclarationExpression, scope, typeEnvironment);
            
            case DropExpression dropExpression:
                return EvaluateDropExpression(dropExpression, scope, typeEnvironment);
            
            case Program program:
                return EvaluateProgram(program, scope, typeEnvironment);
            
            default:
                throw new InvalidProgramException($"The {statement.GetType()} Node has not been setup for interpretation.");
        }
    }

    private static IRuntimeValue EvaluatePrimaryExpression(BinaryExpression binaryExpression, Scope scope, TypeEnvironment typeEnvironment)
    {
        IRuntimeValue lhs = Evaluate(binaryExpression.Left, scope, typeEnvironment);
        IRuntimeValue rhs = Evaluate(binaryExpression.Right, scope, typeEnvironment);
        
        if (lhs is EmptyProgramValue || rhs is EmptyProgramValue)
            throw new InvalidProgramException("Cannot evaluate an empty program.");

        if (lhs is NumberValue lhsNum && rhs is NumberValue rhsNum)
            return EvaluateNumberBinaryExpression(binaryExpression, lhsNum, rhsNum);
        
        if (lhs is StringValue lhsStr && rhs is StringValue rhsStr)
            return EvaluateStringBinaryExpression(binaryExpression, lhsStr, rhsStr);
        
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
                case TokenSymbol.ADD:
                    return new Int32Value(int32Value.Value);
                
                case TokenSymbol.SUBTRACT:
                    return new Int32Value(-int32Value.Value);
                
                case TokenSymbol.BITWISE_NOT:
                    return new Int32Value(~int32Value.Value);

                default:
                    throw new InvalidProgramException($"The operator {unaryExpression.Operator} is not supported.");
            }
        }

        if (value is Float32Value float32Value)
        {
            switch (unaryExpression.Operator)
            {
                case TokenSymbol.ADD:
                    return new Float32Value(float32Value.Value);
                
                case TokenSymbol.SUBTRACT:
                    return new Float32Value(-float32Value.Value);
                
                default:
                    throw new InvalidProgramException($"The operator {unaryExpression.Operator} is not supported.");
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
                case TokenSymbol.LOGICAL_NOT:
                    return new BooleanValue(!boolValue.Value);
                
                default:
                    throw new InvalidProgramException($"The operator {unaryExpression.Operator} is not supported for booleans.");
            }
        }

        throw new InvalidProgramException($"The operator {unaryExpression.Operator} is not supported for booleans.");
    }

    private static IRuntimeValue EvaluateNumberBinaryExpression(BinaryExpression binaryExpression, NumberValue lhs, NumberValue rhs)
    {
        if (lhs is Int32Value lhsInt32 && rhs is Int32Value rhsInt32)
        {
            switch (binaryExpression.Operator)
            {
                case TokenSymbol.ADD:
                    return new Int32Value(lhsInt32.Value + rhsInt32.Value);

                case TokenSymbol.SUBTRACT:
                    return new Int32Value(lhsInt32.Value - rhsInt32.Value);

                case TokenSymbol.MULTIPLY:
                    return new Int32Value(lhsInt32.Value * rhsInt32.Value);

                case TokenSymbol.DIVIDE:
                    return new Int32Value(lhsInt32.Value / rhsInt32.Value); // TODO: Handle divide by zero.

                case TokenSymbol.MODULUS:
                    return new Int32Value(lhsInt32.Value % rhsInt32.Value);
                
                case TokenSymbol.LESS:
                    return new BooleanValue(lhsInt32.Value < rhsInt32.Value);

                case TokenSymbol.GREATER:
                    return new BooleanValue(lhsInt32.Value > rhsInt32.Value);
                
                default:
                    throw new InvalidProgramException($"The operator {binaryExpression.Operator} is not supported.");
            }
        }

        if (lhs is Float32Value lhsFloat32 && rhs is Float32Value rhsFloat32)
        {
            switch (binaryExpression.Operator)
            {
                case TokenSymbol.ADD:
                    return new Float32Value(lhsFloat32.Value + rhsFloat32.Value);

                case TokenSymbol.SUBTRACT:
                    return new Float32Value(lhsFloat32.Value - rhsFloat32.Value);

                case TokenSymbol.MULTIPLY:
                    return new Float32Value(lhsFloat32.Value * rhsFloat32.Value);

                case TokenSymbol.DIVIDE:
                    return new Float32Value(lhsFloat32.Value / rhsFloat32.Value); // TODO: Handle divide by zero.
                
                case TokenSymbol.MODULUS:
                    return new Float32Value(lhsFloat32.Value % rhsFloat32.Value);

                case TokenSymbol.LESS:
                    return new BooleanValue(lhsFloat32.Value < rhsFloat32.Value);

                case TokenSymbol.GREATER:
                    return new BooleanValue(lhsFloat32.Value > rhsFloat32.Value);

                default:
                    throw new InvalidProgramException($"The operator {binaryExpression.Operator} is not supported.");
            }
        }

        throw new InvalidProgramException("Cannot do a binary operation on an expression that is not a number.");
    }

    private static IRuntimeValue EvaluateStringBinaryExpression(BinaryExpression binaryExpression, StringValue lhs, StringValue rhs)
    {
        switch (binaryExpression.Operator)
        {
            case TokenSymbol.ADD:
                return new StringValue(lhs.Value + rhs.Value);

            default:
                throw new InvalidProgramException($"The operator {binaryExpression.Operator} is not supported.");
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

            default:
                throw new InvalidProgramException($"The operator {binaryExpression.Operator} is not supported.");
        }
    }
    
    private static IRuntimeValue EvaluateIdentifier(Identifier identifier, Scope scope) => scope.Get(identifier);

    private static IRuntimeValue EvaluateAssignmentExpression(AssignmentExpression assignmentExpression, Scope scope, TypeEnvironment typeEnvironment)
    {
        IRuntimeValue value = Evaluate(assignmentExpression.Assignment, scope, typeEnvironment);
        scope.Assign(assignmentExpression.Identifier, value);
        return value;
    }
    
    private static IRuntimeValue EvaluateVariableDeclarationStatement(
        VariableDeclarationStatement variableDeclarationStatement,
        Scope scope,
        TypeEnvironment typeEnvironment)
    {
        if (!typeEnvironment.Lookup(variableDeclarationStatement.TypeName, out _))
            throw new Exception($"Type '{variableDeclarationStatement.TypeName}' does not exist in current context.");
        
        scope.Declare(variableDeclarationStatement.Identifier, variableDeclarationStatement.Mutable, Uninitialized.Instance);
        return Uninitialized.Instance;
    }
    
    private static IRuntimeValue EvaluateVariableDeclarationExpression(
        VariableDeclarationExpression variableDeclarationExpression,
        Scope scope,
        TypeEnvironment typeEnvironment)
    {
        if (!typeEnvironment.Lookup(variableDeclarationExpression.TypeName, out _))
            throw new Exception($"Type '{variableDeclarationExpression.TypeName}' does not exist in current context.");

        IRuntimeValue value = Evaluate(variableDeclarationExpression.Initializer, scope, typeEnvironment);
        scope.Declare(variableDeclarationExpression.Identifier, variableDeclarationExpression.Mutable, value);
        return value;
    }

    private static IRuntimeValue EvaluateDropExpression(DropExpression dropExpression, Scope scope, TypeEnvironment typeEnvironment)
    {
        IRuntimeValue value = Evaluate(dropExpression.Identifier, scope, typeEnvironment);
        scope.Drop(dropExpression.Identifier);
        return value;
    }
    
    private static IRuntimeValue EvaluateProgram(Program program, Scope scope, TypeEnvironment typeEnvironment)
    {
        IRuntimeValue lastEvaluated = new EmptyProgramValue();

        foreach (Result<IStatement, string> statement in program.Body)
        {
            IRuntimeValue evaluated = lastEvaluated;

            lastEvaluated = statement.Match(
                ok: s => Evaluate(s, scope, typeEnvironment),
                error: e =>
                {
                    Console.WriteLine($"ERROR: {e}");
                    return evaluated;
                });
        }

        return lastEvaluated;
    }
}