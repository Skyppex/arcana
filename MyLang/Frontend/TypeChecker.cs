
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
    };

    public Type CheckType(IExpression expression, TypeEnvironment? typeEnvironment = null)
    {
        typeEnvironment ??= TypeEnvironment.Global;
        
        switch (expression)
        {
            case Int32Literal:
                return Type.i32;
            
            case Float32Literal:
                return Type.f32;
            
            case StringLiteral:
                return Type.@string;
            
            case BinaryExpression binaryExpression:
                Type leftType = CheckType(binaryExpression.Left, typeEnvironment);
                Type rightType = CheckType(binaryExpression.Right, typeEnvironment);

                List<Type> allowedTypes = s_operandTypesForOperator[binaryExpression.Operator];
                if (!allowedTypes.Contains(leftType) || !allowedTypes.Contains(rightType))
                {
                    throw new InvalidOperationException(
                        $"Operator {binaryExpression.Operator} does not support types {leftType} and {rightType}");
                }
                
                switch (leftType, rightType)
                {
                    case var (l, r) when (l, r) == (Type.i32, Type.i32):
                        return Type.i32;
                    
                    case var (l, r) when (l, r) == (Type.f32, Type.f32):
                        return Type.f32;
                    
                    case var (l, r) when (l, r) == (Type.@string, Type.@string):
                        return Type.@string;
                }

                break;

            case VariableDeclarationExpression variableDeclarationExpression:
            {
                Type type = CheckType(variableDeclarationExpression.Initializer, typeEnvironment);

                var expectedType = Type.FromString(variableDeclarationExpression.TypeName);
                
                if (type != expectedType)
                {
                    throw new InvalidOperationException(
                        $"Variable {variableDeclarationExpression.Identifier.Symbol} has type {type} but expected type {expectedType}");
                }
                
                return typeEnvironment.Define(variableDeclarationExpression.Identifier.Symbol, type);
            }

            case Identifier identifier:
            {
                if (typeEnvironment.Lookup(identifier.Symbol, out Type? type))
                    return type;
                
                throw new InvalidOperationException($"Identifier '{identifier.Symbol}' is not defined");
            }
        }

        throw new InvalidOperationException($"Cannot check type of {expression.GetType()}");
    }
}