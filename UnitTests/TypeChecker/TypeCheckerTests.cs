using MyLang.Parser;
using MyLang.TypeChecker;

using Type = MyLang.TypeChecker.Type;

namespace MyLang.Tests.TypeChecker;

public class TypeCheckerTests
{
    [Test]
    public void i32_expression_should_have_type_i32()
    {
        // Arrange
        var typeChecker = new MyLang.TypeChecker.TypeChecker();
        var expression = new Int32Literal(0);
        
        // Act
        Type type = typeChecker.CheckType(expression, TypeEnvironment.Create());
        
        // Assert
        type.Should().Be(Type.i32);
    }
    
    [Test]
    public void f32_expression_should_have_type_f32()
    {
        // Arrange
        var typeChecker = new MyLang.TypeChecker.TypeChecker();
        var expression = new Float32Literal(0f);
        
        // Act
        Type type = typeChecker.CheckType(expression, TypeEnvironment.Create());
        
        // Asserts
        type.Should().Be(Type.f32);
    }

    [Test]
    public void string_expression_should_have_type_string()
    {
        // Arrange
        var typeChecker = new MyLang.TypeChecker.TypeChecker();
        var expression = new StringLiteral("");
        
        // Act
        Type type = typeChecker.CheckType(expression, TypeEnvironment.Create());
        
        // Assert
        type.Should().Be(Type.@string);
    }
    
    [Test]
    public void binary_expression_should_have_type_i32_when_both_expressions_are_i32(
        [Values("+", "-", "*", "/", "%")] string @operator)
    {
        // Arrange
        var typeChecker = new MyLang.TypeChecker.TypeChecker();
        var left = new Int32Literal(0);
        var right = new Int32Literal(0);
        var expression = new BinaryExpression(left, @operator, right);
        
        // Act
        Type type = typeChecker.CheckType(expression, TypeEnvironment.Create());
        
        // Assert
        type.Should().Be(Type.i32);
    }
    
    [Test]
    public void binary_expression_should_have_type_f32_when_both_expressions_are_f32(
        [Values("+", "-", "*", "/", "%")] string @operator)
    {
        // Arrange
        var typeChecker = new MyLang.TypeChecker.TypeChecker();
        var left = new Float32Literal(0f);
        var right = new Float32Literal(0f);
        var expression = new BinaryExpression(left, @operator, right);
        
        
        // Act
        Type type = typeChecker.CheckType(expression, TypeEnvironment.Create());
        
        // Assert
        type.Should().Be(Type.f32);
    }
    
    [Test]
    public void binary_expression_should_have_type_string_when_both_expressions_are_string_and_operator_is_plus()
    {
        // Arrange
        const string OPERATOR = "+";
        var typeChecker = new MyLang.TypeChecker.TypeChecker();
        var left = new StringLiteral("");
        var right = new StringLiteral("");
        var expression = new BinaryExpression(left, OPERATOR, right);
        
        // Act
        Type type = typeChecker.CheckType(expression, TypeEnvironment.Create());
        
        // Assert
        type.Should().Be(Type.@string);
    }
    
    [Test]
    public void binary_expression_should_throw_error_when_both_expressions_are_string_and_operator_is_invalid(
        [Values("-", "*", "/", "%")] string @operator)
    {
        // Arrange
        var typeChecker = new MyLang.TypeChecker.TypeChecker();
        var left = new StringLiteral("");
        var right = new StringLiteral("");
        var expression = new BinaryExpression(left, @operator, right);
        
        // Act
        Func<Type> type = () => typeChecker.CheckType(expression, TypeEnvironment.Create());
        
        // Assert
        type.Should().Throw<InvalidOperationException>();
    }
    
    [Test]
    public void variable_declaration_expression_should_have_type_i32_when_initializer_is_i32()
    {
        // Arrange
        int value = 0;
        var typeChecker = new MyLang.TypeChecker.TypeChecker();
        var initializer = new Int32Literal(value);
        var identifier = new Identifier("x");
        var expression = new VariableDeclarationExpression(Type.i32.Name, false, identifier, initializer);
        
        // Act
        Type type = typeChecker.CheckType(expression, TypeEnvironment.Create());
        
        // Assert
        type.Should().Be(Type.i32);
    }
    
    [Test]
    public void identifier_expression_defined_as_i32_should_have_type_i32()
    {
        // Arrange
        var typeChecker = new MyLang.TypeChecker.TypeChecker();
        var identifier = new Identifier("x");
        var typeEnvironment = new TypeEnvironment();
        typeEnvironment.DefineVariable(identifier.Symbol, Type.i32);
        
        // Act
        Type type = typeChecker.CheckType(identifier, typeEnvironment);
        
        // Assert
        type.Should().Be(Type.i32);
    }
}