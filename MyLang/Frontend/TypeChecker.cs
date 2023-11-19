
namespace MyLang;

public class TypeChecker
{
    private static readonly Dictionary<string, List<Type>> s_operandTypesForOperator = new()
    {
        ["+"] = new List<Type> { Type.i32, Type.f32, Type.@string },
        ["-"] = new List<Type> { Type.i32, Type.f32 },
        ["*"] = new List<Type> { Type.i32, Type.f32 },
        ["/"] = new List<Type> { Type.i32, Type.f32 },
        ["%"] = new List<Type> { Type.i32, Type.f32 },
        ["&"] = new List<Type> { Type.@bool },
        ["|"] = new List<Type> { Type.@bool },
        ["^"] = new List<Type> { Type.@bool },
        ["~"] = new List<Type> { Type.i32 },
        ["<<"] = new List<Type> { Type.i32 },
        [">>"] = new List<Type> { Type.i32 },
        ["=="] = new List<Type> { Type.i32, Type.f32, Type.@string, Type.@bool },
        ["!="] = new List<Type> { Type.i32, Type.f32, Type.@string, Type.@bool },
        ["<"] = new List<Type> { Type.i32, Type.f32 },
        [">"] = new List<Type> { Type.i32, Type.f32 },
        ["<="] = new List<Type> { Type.i32, Type.f32 },
        [">="] = new List<Type> { Type.i32, Type.f32 },
        ["&&"] = new List<Type> { Type.@bool },
        ["||"] = new List<Type> { Type.@bool },
        ["!"] = new List<Type> { Type.@bool },
    };

    public Type CheckType(IExpression expression, TypeEnvironment typeEnvironment)
    {
        switch (expression)
        {
            case Int32Literal:
                return Type.i32;
            
            case Float32Literal:
                return Type.f32;
            
            case StringLiteral:
                return Type.@string;
            
            case BooleanLiteral:
                return Type.@bool;

            case UnaryExpression unaryExpression:
            {
                Type type = CheckType(unaryExpression.Operand, typeEnvironment);

                if (type != Type.i32 && type != Type.f32 && type != Type.@bool)
                    throw new InvalidOperationException($"Unary operator {unaryExpression.Operator} not supported for operand of type '{type}'");
                
                return type;
            }
            
            case BinaryExpression binaryExpression:
            {
                Type leftType = CheckType(binaryExpression.Left, typeEnvironment);
                Type rightType = CheckType(binaryExpression.Right, typeEnvironment);

                if (leftType != rightType)
                    throw new InvalidOperationException($"Operator '{binaryExpression.Operator}' not supported for operands of type '{leftType}' and '{rightType}'");

                List<Type> allowedTypes = s_operandTypesForOperator[binaryExpression.Operator];

                if (!allowedTypes.Contains(leftType) || !allowedTypes.Contains(rightType))
                {
                    throw new InvalidOperationException(
                        $"Operator {binaryExpression.Operator} not supported for operands of type '{leftType}' and '{rightType}'");
                }

                switch (leftType, rightType)
                {
                    case var (l, r) when (l, r) == (Type.i32, Type.i32):
                        return Type.i32;

                    case var (l, r) when (l, r) == (Type.f32, Type.f32):
                        return Type.f32;

                    case var (l, r) when (l, r) == (Type.@string, Type.@string):
                        return Type.@string;
                    
                    case var (l, r) when (l, r) == (Type.@bool, Type.@bool):
                        return Type.@bool;
                }

                break;
            }

            case Identifier identifier:
            {
                if (typeEnvironment.Lookup(identifier.Symbol, out Type? type))
                    return type;
                
                throw new InvalidOperationException($"Identifier '{identifier.Symbol}' is not defined");
            }
            
            case VariableDeclarationExpression variableDeclarationExpression:
            {
                Type type = CheckType(variableDeclarationExpression.Initializer, typeEnvironment);

                var expectedType = Type.FromString(variableDeclarationExpression.TypeName);
                
                if (type != expectedType)
                {
                    throw new InvalidOperationException(
                        $"Variable {variableDeclarationExpression.Identifier.Symbol} has type '{expectedType}' but was assigned '{type}'");
                }
                
                return typeEnvironment.Define(variableDeclarationExpression.Identifier.Symbol, type);
            }

            case AssignmentExpression assignmentExpression:
            {
                Type type = CheckType(assignmentExpression.Assignment, typeEnvironment);
                
                if (!typeEnvironment.Lookup(assignmentExpression.Identifier.Symbol, out Type? variableType))
                    throw new InvalidOperationException(
                        $"Identifier '{assignmentExpression.Identifier.Symbol}' is not defined");
                
                if (type != variableType)
                    throw new InvalidOperationException(
                        $"Variable {assignmentExpression.Identifier.Symbol} has type '{variableType}' but was assigned '{type}'");
                
                return type;
            }
        }

        throw new InvalidOperationException($"Cannot check type of {expression.GetType()}");
    }
}