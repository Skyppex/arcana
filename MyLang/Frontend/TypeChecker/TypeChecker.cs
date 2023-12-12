using MyLang.Frontend.Lexer;
using MyLang.Frontend.Parser;

namespace MyLang.Frontend.TypeChecker;

public class TypeChecker
{
    private static readonly Dictionary<string, List<Type>> s_operandTypesForOperator = new()
    {
        ["+"] = [Type.i32, Type.f32, Type.@string, Type.@char],
        ["-"] = [Type.i32, Type.f32],
        ["*"] = [Type.i32, Type.f32],
        ["/"] = [Type.i32, Type.f32],
        ["%"] = [Type.i32, Type.f32],
        ["&"] = [Type.@bool],
        ["|"] = [Type.@bool],
        ["^"] = [Type.@bool],
        ["~"] = [Type.i32],
        ["<<"] = [Type.i32],
        [">>"] = [Type.i32],
        ["=="] = [Type.i32, Type.f32, Type.@string, Type.@bool, Type.@char],
        ["!="] = [Type.i32, Type.f32, Type.@string, Type.@bool, Type.@char],
        ["<"] = [Type.i32, Type.f32],
        [">"] = [Type.i32, Type.f32],
        ["<="] = [Type.i32, Type.f32],
        [">="] = [Type.i32, Type.f32],
        ["&&"] = [Type.@bool],
        ["||"] = [Type.@bool],
        ["!"] = [Type.@bool],
    };

    public Type CheckType(IStatement statement, TypeEnvironment typeEnvironment)
    {
        switch (statement)
        {
            case Int32Literal:
                return Type.i32;
            
            case Float32Literal:
                return Type.f32;
            
            case StringLiteral:
                return Type.@string;
            
            case CharLiteral:
                return Type.@char;
            
            case BooleanLiteral:
                return Type.@bool;

            case StructLiteral structLiteral:
            {
                if (!typeEnvironment.LookupType(structLiteral.Identifier, out StructType? type))
                    throw new Exception($"Struct with name '{structLiteral.Identifier}' is not defined.");

                List<Type> fieldTypes = type!.Fields.Values.ToList();
                
                for (int i = 0; i < structLiteral.FieldInitializers.Count; i++)
                {
                    StructLiteral.FieldInitializer fieldInitializer = structLiteral.FieldInitializers[i];
                    
                    Type initializerType = CheckType(fieldInitializer.Initializer, typeEnvironment);

                    if (fieldTypes.Count < i)
                        throw new Exception($"Struct '{type.Name}' does not contain {i + 1} fields.");

                    if (fieldTypes[i] != initializerType)
                        throw new Exception($"Initializer for field '{type.Fields.Keys.ToList()[i]}' is not of type '{fieldTypes[i]}'.");
                }

                return type;
            }
            
            case UnionLiteral unionLiteral:
            {
                if (!typeEnvironment.LookupType(unionLiteral.Identifier, out UnionType? type))
                    throw new Exception($"Union with name '{unionLiteral.Identifier}' is not defined.");

                List<Type> fieldTypes = type!.Members[unionLiteral.Member].Select(pair => pair.Value).ToList();
                
                for (int i = 0; i < unionLiteral.FieldInitializers.Count; i++)
                {
                    UnionLiteral.FieldInitializer fieldInitializer = unionLiteral.FieldInitializers[i];
                    
                    Type initializerType = CheckType(fieldInitializer.Initializer, typeEnvironment);

                    if (fieldTypes.Count < i)
                        throw new Exception($"Union member '{unionLiteral.Identifier}{TokenSymbol.PERIOD}{unionLiteral.Member}' does not contain {i + 1} fields.");

                    if (fieldTypes[i] != initializerType)
                        throw new Exception($"Initializer for field '{type}.{fieldTypes[i]}' is not of type '{fieldTypes[i]}'.");
                }

                return type;
            }
            
            case UnaryExpression unaryExpression:
            {
                Type type = CheckType(unaryExpression.Operand, typeEnvironment);

                if (type != Type.i32 && type != Type.f32 && type != Type.@bool)
                    throw new InvalidOperationException($"Unary operator {unaryExpression.Operator} not supported for operand of type '{type}'");
                
                return type;
            }
            
            case TernaryExpression ternaryExpression:
            {
                Type conditionType = CheckType(ternaryExpression.Condition, typeEnvironment);
                Type trueType = CheckType(ternaryExpression.Then, typeEnvironment);
                Type falseType = CheckType(ternaryExpression.Else, typeEnvironment);

                if (conditionType != Type.@bool)
                    throw new InvalidOperationException($"Ternary operator condition must be of type '{Type.@bool}' but was '{conditionType}'");
                
                if (trueType != falseType)
                    throw new InvalidOperationException($"Ternary operator true and false expressions must be of the same type but were '{trueType}' and '{falseType}'");
                
                return trueType;
            }
            
            case DropExpression dropExpression:
            {
                return CheckType(dropExpression.Identifier, typeEnvironment);
            }
            
            case BinaryExpression binaryExpression:
            {
                Type leftType = CheckType(binaryExpression.Left, typeEnvironment);
                Type rightType = CheckType(binaryExpression.Right, typeEnvironment);
                
                List<Type> allowedTypes = s_operandTypesForOperator[binaryExpression.Operator];

                if (!allowedTypes.Contains(leftType) || !allowedTypes.Contains(rightType))
                {
                    throw new InvalidOperationException(
                        $"Operator {binaryExpression.Operator} not supported for operands of type '{leftType}' and '{rightType}'");
                }

                switch (binaryExpression.Operator)
                {
                    case TokenSymbol.LOGICAL_AND or TokenSymbol.LOGICAL_OR:
                    {
                        if (leftType != Type.@bool || rightType != Type.@bool)
                        {
                            throw new InvalidOperationException(
                                $"Operator {binaryExpression.Operator} not supported for operands of type '{leftType}' and '{rightType}'");
                        }

                        return Type.@bool;
                    }
                    
                    case TokenSymbol.LOGICAL_EQUAL or TokenSymbol.LOGICAL_NOT_EQUAL or TokenSymbol.GREATER or TokenSymbol.LESS or TokenSymbol.LOGICAL_GREATER_EQUAL or TokenSymbol.LOGICAL_LESS_EQUAL:
                    {
                        if (!Type.Numbers.Contains(leftType) || !Type.Numbers.Contains(rightType))
                        {
                            throw new InvalidOperationException(
                                $"Operator {binaryExpression.Operator} not supported for operands of type '{leftType}' and '{rightType}'");
                        }

                        return Type.@bool;
                    }
                    
                    case TokenSymbol.PLUS or TokenSymbol.MINUS or TokenSymbol.STAR or TokenSymbol.SLASH
                     or TokenSymbol.PERCENT:
                    {
                        switch (leftType, rightType)
                        {
                            case var (l, r) when (l, r) == (Type.i32, Type.i32):
                                return Type.i32;

                            case var (l, r) when (l, r) == (Type.f32, Type.f32):
                                return Type.f32;

                            case var (l, r) when (l, r) == (Type.@string, Type.@string):
                                return Type.@string;
                            
                            case var (l, r) when (l, r) == (Type.@char, Type.@char):
                                return Type.@string;

                            case var (l, r) when (l, r) == (Type.@string, Type.@char):
                                return Type.@string;

                            case var (l, r) when (l, r) == (Type.@char, Type.@string):
                                return Type.@string;

                            case var (l, r) when (l, r) == (Type.@bool, Type.@bool):
                                return Type.@bool;
                        }

                        break;
                    }
                }

                break;
            }
            
            case Identifier identifier:
            {
                if (typeEnvironment.LookupVariable(identifier.Symbol, out Type? type))
                    return type!;
                
                throw new InvalidOperationException($"Identifier '{identifier.Symbol}' doesn't exist in current context.");
            }

            case MemberAccessExpression memberExpression:
            {
                Type type = CheckType(memberExpression.Object, typeEnvironment);

                if (type is StructType structType)
                {
                    switch (memberExpression.Member)
                    {
                        case Identifier identifier:
                        {
                            if (!structType.Fields.ContainsKey(identifier.Symbol))
                                throw new InvalidOperationException($"Struct '{structType.Name}' doesn't contain field '{identifier.Symbol}'");
                            
                            Type fieldType = structType.Fields[identifier.Symbol];
                            
                            if (!typeEnvironment.LookupType<Type>(fieldType.Name, out _))
                                throw new InvalidOperationException($"Type '{fieldType.Name}' doesn't exist in current context.");

                            return fieldType;
                        }
                        
                        // case CallExpression callExpression:
                        //     return CheckType(callExpression, typeEnvironment);
                    }
                }

                break;
            }

            case CallExpression callExpression:
            {
                Type type = CheckType(callExpression.Caller, typeEnvironment);

                foreach (IExpression argument in callExpression.Arguments)
                    CheckType(argument, typeEnvironment);

                return type; // TODO: Return type of function instead of caller type
            }
            
            case VariableDeclarationExpression variableDeclarationExpression:
            {
                Type type = CheckType(variableDeclarationExpression.Initializer, typeEnvironment);

                if (!typeEnvironment.LookupType(variableDeclarationExpression.TypeName, out Type? expectedType))
                    throw new InvalidOperationException(
                        $"Type '{variableDeclarationExpression.TypeName}' doesn't exist in current context.");
                
                if (type != expectedType)
                {
                    throw new InvalidOperationException(
                        $"Variable {variableDeclarationExpression.Identifier.Symbol} has type '{expectedType}' but was assigned '{type}'");
                }
                
                return typeEnvironment.DefineVariable(variableDeclarationExpression.Identifier.Symbol, type);
            }
            
            case VariableDeclarationStatement variableDeclarationStatement:
            {
                if (!typeEnvironment.LookupType(variableDeclarationStatement.TypeName, out Type? type))
                    throw new InvalidOperationException(
                        $"Type '{variableDeclarationStatement.TypeName}' doesn't exist in current context.");
                
                return typeEnvironment.DefineVariable(variableDeclarationStatement.Identifier.Symbol, type!);
            }

            case AssignmentExpression assignmentExpression:
            {
                Type type = CheckType(assignmentExpression.Assignment, typeEnvironment);

                Type accessorRootType = CheckType(assignmentExpression.Member, typeEnvironment);

                if (!typeEnvironment.LookupVariable(assignmentExpression.Member.Symbol, out Type? variableType, accessorRootType))
                    throw new InvalidOperationException(
                        $"Identifier '{assignmentExpression.Member.Symbol}' doesn't exist in current context.");
                
                if (type != variableType)
                    throw new InvalidOperationException(
                        $"Variable {assignmentExpression.Member.Symbol} has type '{variableType}' but was assigned '{type}'");
                
                return type;
            }

            case BlockExpression blockExpression:
            {
                Type lastType = Type.never;
                
                foreach (IStatement stmt in blockExpression.Statements)
                    lastType = CheckType(stmt, new TypeEnvironment(typeEnvironment));
                
                return lastType;
            }

            case StructDeclarationStatement structDeclarationStatement:
            {
                if (typeEnvironment.IsTypeDefined(structDeclarationStatement.TypeName))
                    throw new Exception($"Type '{structDeclarationStatement.TypeName}' already exists in current context.");

                return typeEnvironment.DefineType(new StructType(structDeclarationStatement.TypeName,
                    structDeclarationStatement.Fields.ToDictionary(f => f.Identifier, f =>
                    {
                        if (typeEnvironment.LookupType(f.TypeName, out Type? type))
                            return type!;
                        
                        throw new Exception($"Type '{f.TypeName}' doesn't exist in current context.");
                    })));
            }

            case UnionDeclarationStatement unionDeclarationStatement:
            {
                if (typeEnvironment.IsTypeDefined(unionDeclarationStatement.TypeName))
                    throw new Exception($"Type '{unionDeclarationStatement.TypeName}' already exists in current context.");
                
                unionDeclarationStatement.Members.ToList()
                    .ForEach(m =>
                    {
                        m.Fields.ForEach(f =>
                        {
                            if (typeEnvironment.IsTypeDefined(f.TypeName))
                                return;
                            
                            throw new Exception($"Type '{f.TypeName}' doesn't exist in current context.");
                        });
                    });
                
                return typeEnvironment.DefineType(new UnionType(unionDeclarationStatement.TypeName, unionDeclarationStatement.Members
                    .ToDictionary(m => m.Identifier, m => m.Fields
                        .ToDictionary(f => f.Identifier, f =>
                        {
                            if (typeEnvironment.LookupType(f.TypeName, out Type? type))
                                return type;

                            return Type.never;
                        }))!));
            }
            
            case Program:
            case IfStatement:
                return Type.statement;
        }

        throw new InvalidOperationException($"Cannot check type of {statement.GetType()}");
    }
}