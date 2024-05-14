mod common;

use common::{create_typed_ast, StatementExt, VecStatementExt};

use interpreter::{value::Number, Value};
use shared::type_checker::{
    ast::{BinaryOperator, Typed, TypedExpression},
    Type,
};

use crate::common::{create_env, evaluate_expression};

#[test]
fn add_is_add() {
    // Arrange
    let input = "1 + 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Binary { operator: BinaryOperator::Add, .. }));
}

#[test]
fn subtract_is_subtract() {
    // Arrange
    let input = "1 - 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Binary { operator: BinaryOperator::Subtract, .. }));
}

#[test]
fn multiply_is_multiply() {
    // Arrange
    let input = "1 * 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Binary { operator: BinaryOperator::Multiply, .. }));
}

#[test]
fn divide_is_divide() {
    // Arrange
    let input = "1 / 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Binary { operator: BinaryOperator::Divide, .. }));
}

#[test]
fn modulo_is_modulo() {
    // Arrange
    let input = "1 % 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Binary { operator: BinaryOperator::Modulo, .. }));
}

#[test]
fn bitwise_and_is_bitwise_and() {
    // Arrange
    let input = "1 & 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Binary { operator: BinaryOperator::BitwiseAnd, .. }));
}

#[test]
fn bitwise_or_is_bitwise_or() {
    // Arrange
    let input = "1 | 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Binary { operator: BinaryOperator::BitwiseOr, .. }));
}

#[test]
fn bitwise_xor_is_bitwise_xor() {
    // Arrange
    let input = "1 ^ 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(expression, TypedExpression::Binary { operator: BinaryOperator::BitwiseXor, .. }));
}

#[test]
fn bitwise_left_shift_is_bitwise_left_shift() {
    // Arrange
    let input = "1 << 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(
        expression,
        TypedExpression::Binary { operator: BinaryOperator::BitwiseLeftShift, .. }
    ));
}

#[test]
fn bitwise_right_shift_is_bitwise_right_shift() {
    // Arrange
    let input = "1 >> 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert!(matches!(
        expression,
        TypedExpression::Binary { operator: BinaryOperator::BitwiseRightShift, .. }
    ));
}

#[test]
fn logical_and_is_logical_and() {
    // Arrange
    let input = "true && false";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    if let TypedExpression::Binary { operator, .. } = expression {
        assert_eq!(operator, BinaryOperator::LogicalAnd);
    } else {
        panic!("Expected Binary expression");
    }
}

#[test]
fn logical_or_is_logical_or() {
    // Arrange
    let input = "true || false";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    if let TypedExpression::Binary { operator, .. } = expression {
        assert_eq!(operator, BinaryOperator::LogicalOr);
    } else {
        panic!("Expected Binary expression");
    }
}

#[test]
fn equal_is_equal() {
    // Arrange
    let input = "1 == 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    if let TypedExpression::Binary { operator, .. } = expression {
        assert_eq!(operator, BinaryOperator::Equal);
    } else {
        panic!("Expected Binary expression");
    }
}

#[test]
fn not_equal_is_not_equal() {
    // Arrange
    let input = "1 != 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    if let TypedExpression::Binary { operator, .. } = expression {
        assert_eq!(operator, BinaryOperator::NotEqual);
    } else {
        panic!("Expected Binary expression");
    }
}

#[test]
fn less_than_is_less_than() {
    // Arrange
    let input = "1 < 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    if let TypedExpression::Binary { operator, .. } = expression {
        assert_eq!(operator, BinaryOperator::LessThan);
    } else {
        panic!("Expected Binary expression");
    }
}

#[test]
fn less_than_or_equal_is_less_than_or_equal() {
    // Arrange
    let input = "1 <= 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    if let TypedExpression::Binary { operator, .. } = expression {
        assert_eq!(operator, BinaryOperator::LessThanOrEqual);
    } else {
        panic!("Expected Binary expression");
    }
}

#[test]
fn greater_than_is_greater_than() {
    // Arrange
    let input = "1 > 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    if let TypedExpression::Binary { operator, .. } = expression {
        assert_eq!(operator, BinaryOperator::GreaterThan);
    } else {
        panic!("Expected Binary expression");
    }
}

#[test]
fn greater_than_or_equal_is_greater_than_or_equal() {
    // Arrange
    let input = "1 >= 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    if let TypedExpression::Binary { operator, .. } = expression {
        assert_eq!(operator, BinaryOperator::GreaterThanOrEqual);
    } else {
        panic!("Expected Binary expression");
    }
}

#[test]
fn add_has_correct_type() {
    // Arrange
    let input = "1 + 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
fn subtract_has_correct_type() {
    // Arrange
    let input = "1 - 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
fn multiply_has_correct_type() {
    // Arrange
    let input = "1 * 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
fn divide_has_correct_type() {
    // Arrange
    let input = "1 / 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
fn modulo_has_correct_type() {
    // Arrange
    let input = "1 % 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
fn bitwise_and_has_correct_type() {
    // Arrange
    let input = "1 & 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
fn bitwise_or_has_correct_type() {
    // Arrange
    let input = "1 | 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
fn bitwise_xor_has_correct_type() {
    // Arrange
    let input = "1 ^ 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
fn bitwise_left_shift_has_correct_type() {
    // Arrange
    let input = "1 << 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
fn bitwise_right_shift_has_correct_type() {
    // Arrange
    let input = "1 >> 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Int);
}

#[test]
fn logical_and_has_correct_type() {
    // Arrange
    let input = "true && false";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Bool);
}

#[test]
fn logical_or_has_correct_type() {
    // Arrange
    let input = "true || false";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Bool);
}

#[test]
fn equal_has_correct_type() {
    // Arrange
    let input = "1 == 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Bool);
}

#[test]
fn not_equal_has_correct_type() {
    // Arrange
    let input = "1 != 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Bool);
}

#[test]
fn less_than_has_correct_type() {
    // Arrange
    let input = "1 < 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Bool);
}

#[test]
fn less_than_or_equal_has_correct_type() {
    // Arrange
    let input = "1 <= 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Bool);
}

#[test]
fn greater_than_has_correct_type() {
    // Arrange
    let input = "1 > 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Bool);
}

#[test]
fn greater_than_or_equal_has_correct_type() {
    // Arrange
    let input = "1 >= 2";

    // Act
    let typed_ast = create_typed_ast(input);

    // Assert
    let expression = typed_ast
        .unwrap_program()
        .nth_statement(0)
        .unwrap_expression();

    assert_eq!(expression.get_type(), Type::Bool);
}

#[test]
fn add_returns_sum() {
    // Arrange
    let input = "1 + 2";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(3)));
}

#[test]
fn subtract_returns_difference() {
    // Arrange
    let input = "1 - 2";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(-1)));
}

#[test]
fn multiply_returns_product() {
    // Arrange
    let input = "2 * 3";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(6)));
}

#[test]
fn divide_returns_quotient() {
    // Arrange
    let input = "6 / 3";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(2)));
}

#[test]
fn modulo_returns_remainder() {
    // Arrange
    let input = "7 % 3";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(1)));
}

#[test]
fn bitwise_and_returns_bitwise_and() {
    // Arrange
    let input = "5 & 3";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(1)));
}

#[test]
fn bitwise_or_returns_bitwise_or() {
    // Arrange
    let input = "5 | 3";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(7)));
}

#[test]
fn bitwise_xor_returns_bitwise_xor() {
    // Arrange
    let input = "5 ^ 3";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(6)));
}

#[test]
fn bitwise_left_shift_returns_bitwise_left_shift() {
    // Arrange
    let input = "1 << 2";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(4)));
}

#[test]
fn bitwise_right_shift_returns_bitwise_right_shift() {
    // Arrange
    let input = "4 >> 2";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Number(Number::Int(1)));
}

#[test]
fn logical_and_returns_logical_and() {
    // Arrange
    let input = "true && false";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(false));
}

#[test]
fn logical_or_returns_logical_or() {
    // Arrange
    let input = "true || false";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(true));
}

#[test]
fn equal_returns_true_if_equal() {
    // Arrange
    let input = "1 == 1";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(true));
}

#[test]
fn equal_returns_false_if_not_equal() {
    // Arrange
    let input = "1 == 2";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(false));
}

#[test]
fn not_equal_returns_true_if_not_equal() {
    // Arrange
    let input = "1 != 2";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(true));
}

#[test]
fn not_equal_returns_false_if_equal() {
    // Arrange
    let input = "1 != 1";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(false));
}

#[test]
fn less_than_returns_true_if_less_than() {
    // Arrange
    let input = "1 < 2";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(true));
}

#[test]
fn less_than_returns_false_if_not_less_than() {
    // Arrange
    let input = "2 < 1";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(false));
}

#[test]
fn less_than_or_equal_returns_true_if_less_than() {
    // Arrange
    let input = "1 <= 2";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(true));
}

#[test]
fn less_than_or_equal_returns_true_if_equal() {
    // Arrange
    let input = "1 <= 1";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(true));
}

#[test]
fn less_than_or_equal_returns_false_if_not_less_than() {
    // Arrange
    let input = "2 <= 1";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(false));
}

#[test]
fn greater_than_returns_true_if_greater_than() {
    // Arrange
    let input = "2 > 1";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(true));
}

#[test]
fn greater_than_returns_false_if_not_greater_than() {
    // Arrange
    let input = "1 > 2";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(false));
}

#[test]
fn greater_than_or_equal_returns_true_if_greater_than() {
    // Arrange
    let input = "2 >= 1";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(true));
}

#[test]
fn greater_than_or_equal_returns_true_if_equal() {
    // Arrange
    let input = "1 >= 1";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(true));
}

#[test]
fn greater_than_or_equal_returns_false_if_not_greater_than() {
    // Arrange
    let input = "1 >= 2";

    // Act
    let value = evaluate_expression(input, create_env(), false);

    // Assert
    assert_eq!(value, Value::Bool(false));
}
