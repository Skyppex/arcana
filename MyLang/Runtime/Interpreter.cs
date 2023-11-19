using Monads;

namespace MyLang.Runtime;

public class Interpreter
{
    public static IRuntimeValue Evaluate(IStatement statement, Scope scope)
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
                return EvaluateNumberUnaryExpression(unaryExpression, Evaluate(unaryExpression.Operand, scope));

            case UnaryExpression unaryExpression when unaryExpression is { Operator: TokenSymbol.LOGICAL_NOT or TokenSymbol.BITWISE_NOT }:
                return EvaluateBooleanUnaryExpression(unaryExpression, Evaluate(unaryExpression.Operand, scope));

            case BinaryExpression binaryExpression:
                return EvaluatePrimaryExpression(binaryExpression, scope);
            
            case Identifier identifier:
                return EvaluateIdentifier(identifier, scope);
            
            case AssignmentExpression assignmentExpression:
                return EvaluateAssignmentExpression(assignmentExpression, scope);
            
            case VariableDeclarationStatement variableDeclarationStatement:
                return EvaluateVariableDeclarationStatement(variableDeclarationStatement, scope);
            
            case VariableDeclarationExpression variableDeclarationExpression:
                return EvaluateVariableDeclarationExpression(variableDeclarationExpression, scope);
            
            case DropExpression dropExpression:
                return EvaluateDropExpression(dropExpression, scope);
            
            case Program program:
                return EvaluateProgram(program, scope);
            
            default:
                throw new InvalidProgramException($"The {statement.GetType()} Node has not been setup for interpretation.");
        }
    }

    private static IRuntimeValue EvaluatePrimaryExpression(BinaryExpression binaryExpression, Scope scope)
    {
        IRuntimeValue lhs = Evaluate(binaryExpression.Left, scope);
        IRuntimeValue rhs = Evaluate(binaryExpression.Right, scope);
        
        if (lhs is EmptyProgramValue || rhs is EmptyProgramValue)
            throw new InvalidProgramException("Cannot evaluate an empty program.");

        if (lhs is NumberValue && rhs is NumberValue)
            return EvaluateNumberBinaryExpression(binaryExpression, lhs, rhs);
        
        if (lhs is BooleanValue && rhs is BooleanValue)
            return EvaluateBooleanBinaryExpression(binaryExpression, lhs, rhs);
        
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

        throw new InvalidProgramException("Cannot do a unary operation on an expression that is not a number.");
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
                    throw new InvalidProgramException($"The operator {unaryExpression.Operator} is not supported.");
            }
        }

        throw new InvalidProgramException("Cannot do a unary operation on an expression that is not a number.");
    }

    private static IRuntimeValue EvaluateNumberBinaryExpression(BinaryExpression binaryExpression, IRuntimeValue lhs, IRuntimeValue rhs)
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

    private static IRuntimeValue EvaluateBooleanBinaryExpression(BinaryExpression binaryExpression, IRuntimeValue lhs, IRuntimeValue rhs)
    {
        if (lhs is BooleanValue lhsBool && rhs is BooleanValue rhsBool)
        {
            switch (binaryExpression.Operator)
            {
                case TokenSymbol.LOGICAL_AND:
                    return new BooleanValue(lhsBool.Value && rhsBool.Value);

                case TokenSymbol.LOGICAL_OR:
                    return new BooleanValue(lhsBool.Value || rhsBool.Value);

                default:
                    throw new InvalidProgramException($"The operator {binaryExpression.Operator} is not supported.");
            }
        }

        throw new InvalidProgramException("Cannot do a binary operation on an expression that is not a number.");
    }

    
    private static IRuntimeValue EvaluateIdentifier(Identifier identifier, Scope scope) => scope.Get(identifier);

    private static IRuntimeValue EvaluateAssignmentExpression(AssignmentExpression assignmentExpression, Scope scope)
    {
        IRuntimeValue value = Evaluate(assignmentExpression.Assignment, scope);
        scope.Assign(assignmentExpression.Identifier, value);
        return value;
    }
    
    private static IRuntimeValue EvaluateVariableDeclarationStatement(VariableDeclarationStatement variableDeclarationStatement, Scope scope)
    {
        scope.ResolveTypeName(variableDeclarationStatement.TypeName);
        scope.Declare(variableDeclarationStatement.Identifier, variableDeclarationStatement.Mutable, Uninitialized.Instance);
        return Uninitialized.Instance;
    }
    
    private static IRuntimeValue EvaluateVariableDeclarationExpression(VariableDeclarationExpression variableDeclarationExpression, Scope scope)
    {
        scope.ResolveTypeName(variableDeclarationExpression.TypeName);
        IRuntimeValue value = Evaluate(variableDeclarationExpression.Initializer, scope);
        scope.Declare(variableDeclarationExpression.Identifier, variableDeclarationExpression.Mutable, value);
        return value;
    }

    private static IRuntimeValue EvaluateDropExpression(DropExpression dropExpression, Scope scope)
    {
        IRuntimeValue value = Evaluate(dropExpression.Identifier, scope);
        scope.Drop(dropExpression.Identifier);
        return value;
    }
    
    private static IRuntimeValue EvaluateProgram(Program program, Scope scope)
    {
        IRuntimeValue lastEvaluated = new EmptyProgramValue();

        foreach (Result<IStatement, string> statement in program.Body)
        {
            IRuntimeValue evaluated = lastEvaluated;

            lastEvaluated = statement.Match(
                ok: s => Evaluate(s, scope),
                error: e =>
                {
                    Console.WriteLine($"ERROR: {e}");
                    return evaluated;
                });
        }

        return lastEvaluated;
    }
}