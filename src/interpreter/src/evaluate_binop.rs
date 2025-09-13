use shared::type_checker::model::{BinaryOperator, TypedExpression};

use crate::{environment::Rcrc, evaluator::evaluate_expression, value::Enum, Environment};

use super::value::{Number, Value};

pub(crate) fn evaluate_binop(
    left: TypedExpression,
    operator: BinaryOperator,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    match operator {
        BinaryOperator::Add => evaluate_add(left, right, environment),
        BinaryOperator::Subtract => evaluate_subtract(left, right, environment),
        BinaryOperator::Multiply => evaluate_multiply(left, right, environment),
        BinaryOperator::Divide => evaluate_divide(left, right, environment),
        BinaryOperator::Modulo => evaluate_modulo(left, right, environment),
        BinaryOperator::BitwiseAnd => evaluate_bitwise_and(left, right, environment),
        BinaryOperator::BitwiseOr => evaluate_bitwise_or(left, right, environment),
        BinaryOperator::BitwiseXor => evaluate_bitwise_xor(left, right, environment),
        BinaryOperator::BitwiseLeftShift => evaluate_bitwise_left_shift(left, right, environment),
        BinaryOperator::BitwiseRightShift => evaluate_bitwise_right_shift(left, right, environment),
        BinaryOperator::LogicalAnd => evaluate_boolean_logical_and(left, right, environment),
        BinaryOperator::LogicalOr => evaluate_boolean_logical_or(left, right, environment),
        BinaryOperator::Equal => evaluate_equal(left, right, environment),
        BinaryOperator::NotEqual => evaluate_not_equal(left, right, environment),
        BinaryOperator::LessThan => evaluate_less_than(left, right, environment),
        BinaryOperator::LessThanOrEqual => evaluate_less_than_or_equal(left, right, environment),
        BinaryOperator::GreaterThan => evaluate_greater_than(left, right, environment),
        BinaryOperator::GreaterThanOrEqual => {
            evaluate_greater_than_or_equal(left, right, environment)
        }
        BinaryOperator::Range => evaluate_range(left, right, false, environment),
        BinaryOperator::RangeInclusive => evaluate_range(left, right, true, environment),
    }
}

fn evaluate_add(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Number(Number::Int(left + right))),
            (Number::UInt(left), Number::UInt(right)) => {
                Ok(Value::Number(Number::UInt(left + right)))
            }
            (Number::Float(left), Number::Float(right)) => {
                Ok(Value::Number(Number::Float(left + right)))
            }
            (left, right) => Err(format!("Cannot add {:?} and {:?}", left, right)),
        },
        (Value::String(left), Value::String(right)) => Ok(Value::String(left + &right)),
        (Value::Array(left), Value::Array(right)) => {
            Ok(Value::Array(left.into_iter().chain(right).collect()))
        }
        (Value::Array(left), right) => Ok(Value::Array(
            left.into_iter().chain(std::iter::once(right)).collect(),
        )),
        (left, right) => Err(format!("Cannot add {:?} and {:?}", left, right)),
    }
}

fn evaluate_subtract(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Number(Number::Int(left - right))),
            (Number::UInt(left), Number::UInt(right)) => {
                Ok(Value::Number(Number::UInt(left - right)))
            }
            (Number::Float(left), Number::Float(right)) => {
                Ok(Value::Number(Number::Float(left - right)))
            }
            (left, right) => Err(format!("Cannot subtract {:?} and {:?}", left, right)),
        },
        (left, right) => Err(format!("Cannot subtract {:?} and {:?}", left, right)),
    }
}

fn evaluate_multiply(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Number(Number::Int(left * right))),
            (Number::UInt(left), Number::UInt(right)) => {
                Ok(Value::Number(Number::UInt(left * right)))
            }
            (Number::Float(left), Number::Float(right)) => {
                Ok(Value::Number(Number::Float(left * right)))
            }
            (left, right) => Err(format!("Cannot multiply {:?} and {:?}", left, right)),
        },
        (left, right) => Err(format!("Cannot multiply {:?} and {:?}", left, right)),
    }
}

fn evaluate_divide(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Number(Number::Int(left / right))),
            (Number::UInt(left), Number::UInt(right)) => {
                Ok(Value::Number(Number::UInt(left / right)))
            }
            (Number::Float(left), Number::Float(right)) => {
                Ok(Value::Number(Number::Float(left / right)))
            }
            (left, right) => Err(format!("Cannot divide {:?} and {:?}", left, right)),
        },
        (left, right) => Err(format!("Cannot divide {:?} and {:?}", left, right)),
    }
}

fn evaluate_modulo(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Number(Number::Int(left % right))),
            (Number::UInt(left), Number::UInt(right)) => {
                Ok(Value::Number(Number::UInt(left % right)))
            }
            (Number::Float(left), Number::Float(right)) => {
                Ok(Value::Number(Number::Float(left % right)))
            }
            (left, right) => Err(format!("Cannot modulo {:?} and {:?}", left, right)),
        },
        (left, right) => Err(format!("Cannot modulo {:?} and {:?}", left, right)),
    }
}

fn evaluate_bitwise_and(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Number(Number::Int(left & right))),
            (Number::UInt(left), Number::UInt(right)) => {
                Ok(Value::Number(Number::UInt(left & right)))
            }
            (left, right) => Err(format!("Cannot bitwise and {:?} and {:?}", left, right)),
        },
        (left, right) => Err(format!("Cannot bitwise and {:?} and {:?}", left, right)),
    }
}

fn evaluate_bitwise_or(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Number(Number::Int(left | right))),
            (Number::UInt(left), Number::UInt(right)) => {
                Ok(Value::Number(Number::UInt(left | right)))
            }
            (left, right) => Err(format!("Cannot bitwise or {:?} and {:?}", left, right)),
        },
        (left, right) => Err(format!("Cannot bitwise or {:?} and {:?}", left, right)),
    }
}

fn evaluate_bitwise_xor(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Number(Number::Int(left ^ right))),
            (Number::UInt(left), Number::UInt(right)) => {
                Ok(Value::Number(Number::UInt(left ^ right)))
            }
            (left, right) => Err(format!("Cannot bitwise xor {:?} and {:?}", left, right)),
        },
        (left, right) => Err(format!("Cannot bitwise xor {:?} and {:?}", left, right)),
    }
}

fn evaluate_bitwise_left_shift(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => {
                Ok(Value::Number(Number::Int(left << right)))
            }
            (Number::UInt(left), Number::UInt(right)) => {
                Ok(Value::Number(Number::UInt(left << right)))
            }
            (left, right) => Err(format!(
                "Cannot bitwise left shift {:?} and {:?}",
                left, right
            )),
        },
        (left, right) => Err(format!(
            "Cannot bitwise left shift {:?} and {:?}",
            left, right
        )),
    }
}

fn evaluate_bitwise_right_shift(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => {
                Ok(Value::Number(Number::Int(left >> right)))
            }
            (Number::UInt(left), Number::UInt(right)) => {
                Ok(Value::Number(Number::UInt(left >> right)))
            }
            (left, right) => Err(format!(
                "Cannot bitwise right shift {:?} and {:?}",
                left, right
            )),
        },
        (left, right) => Err(format!(
            "Cannot bitwise right shift {:?} and {:?}",
            left, right
        )),
    }
}

fn evaluate_boolean_logical_and(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;

    if let Value::Bool(false) = left {
        return Ok(Value::Bool(false));
    }

    let right = evaluate_expression(right, environment)?;

    match right {
        Value::Bool(false) => Ok(Value::Bool(false)),
        Value::Bool(true) => Ok(left),
        _ => Err(format!(
            "Cannot boolean logical and {:?} and {:?}",
            left, right
        )),
    }
}

fn evaluate_boolean_logical_or(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;

    if let Value::Bool(true) = left {
        return Ok(Value::Bool(true));
    }

    let right = evaluate_expression(right, environment)?;

    match right {
        Value::Bool(true) => Ok(Value::Bool(true)),
        Value::Bool(false) => Ok(left),
        _ => Err(format!(
            "Cannot boolean logical or {:?} and {:?}",
            left, right
        )),
    }
}

fn evaluate_equal(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Unit, Value::Unit) => Ok(Value::Bool(true)),
        (Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(left == right)),
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Bool(left == right)),
            (Number::UInt(left), Number::UInt(right)) => Ok(Value::Bool(left == right)),
            (Number::Float(left), Number::Float(right)) => Ok(Value::Bool(left == right)),
            (left, right) => Err(format!("Cannot equal {:?} and {:?}", left, right)),
        },
        (Value::Rune(left), Value::Rune(right)) => Ok(Value::Bool(left == right)),
        (Value::String(left), Value::String(right)) => Ok(Value::Bool(left == right)),
        (
            Value::Enum(Enum {
                enum_member: left_enum_member,
                ..
            }),
            Value::Enum(Enum {
                enum_member: right_enum_member,
                ..
            }),
        ) => Ok(Value::Bool(left_enum_member == right_enum_member)),
        (left, right) => Err(format!("Cannot equal {:?} and {:?}", left, right)),
    }
}

fn evaluate_not_equal(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Unit, Value::Unit) => Ok(Value::Bool(false)),
        (Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(left != right)),
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Bool(left != right)),
            (Number::UInt(left), Number::UInt(right)) => Ok(Value::Bool(left != right)),
            (Number::Float(left), Number::Float(right)) => Ok(Value::Bool(left != right)),
            (left, right) => Err(format!("Cannot not equal {:?} and {:?}", left, right)),
        },
        (Value::Rune(left), Value::Rune(right)) => Ok(Value::Bool(left != right)),
        (Value::String(left), Value::String(right)) => Ok(Value::Bool(left != right)),
        (left, right) => Err(format!("Cannot not equal {:?} and {:?}", left, right)),
    }
}

fn evaluate_less_than(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Bool(left < right)),
            (Number::UInt(left), Number::UInt(right)) => Ok(Value::Bool(left < right)),
            (Number::Float(left), Number::Float(right)) => Ok(Value::Bool(left < right)),
            (left, right) => Err(format!("Cannot less than {:?} and {:?}", left, right)),
        },
        (left, right) => Err(format!("Cannot less than {:?} and {:?}", left, right)),
    }
}

fn evaluate_less_than_or_equal(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Bool(left <= right)),
            (Number::UInt(left), Number::UInt(right)) => Ok(Value::Bool(left <= right)),
            (Number::Float(left), Number::Float(right)) => Ok(Value::Bool(left <= right)),
            (left, right) => Err(format!(
                "Cannot less than or equal {:?} and {:?}",
                left, right
            )),
        },
        (left, right) => Err(format!(
            "Cannot less than or equal {:?} and {:?}",
            left, right
        )),
    }
}

fn evaluate_greater_than(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Bool(left > right)),
            (Number::UInt(left), Number::UInt(right)) => Ok(Value::Bool(left > right)),
            (Number::Float(left), Number::Float(right)) => Ok(Value::Bool(left > right)),
            (left, right) => Err(format!("Cannot greater than {:?} and {:?}", left, right)),
        },
        (left, right) => Err(format!("Cannot greater than {:?} and {:?}", left, right)),
    }
}

fn evaluate_greater_than_or_equal(
    left: TypedExpression,
    right: TypedExpression,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Bool(left >= right)),
            (Number::UInt(left), Number::UInt(right)) => Ok(Value::Bool(left >= right)),
            (Number::Float(left), Number::Float(right)) => Ok(Value::Bool(left >= right)),
            (left, right) => Err(format!(
                "Cannot greater than or equal {:?} and {:?}",
                left, right
            )),
        },
        (left, right) => Err(format!(
            "Cannot greater than or equal {:?} and {:?}",
            left, right
        )),
    }
}

fn evaluate_range(
    left: TypedExpression,
    right: TypedExpression,
    inclusive: bool,
    environment: Rcrc<Environment>,
) -> Result<Value, String> {
    let left = evaluate_expression(left, environment.clone())?;
    let right = evaluate_expression(right, environment)?;

    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right, inclusive) {
            (Number::Int(left), Number::Int(right), false) => Ok(Value::Array(
                (left..right)
                    .map(|v| Value::Number(Number::Int(v)))
                    .collect(),
            )),
            (Number::Int(left), Number::Int(right), true) => Ok(Value::Array(
                (left..=right)
                    .map(|v| Value::Number(Number::Int(v)))
                    .collect(),
            )),
            (Number::UInt(left), Number::UInt(right), false) => Ok(Value::Array(
                (left..right)
                    .map(|v| Value::Number(Number::UInt(v)))
                    .collect(),
            )),
            (Number::UInt(left), Number::UInt(right), true) => Ok(Value::Array(
                (left..right)
                    .map(|v| Value::Number(Number::UInt(v)))
                    .collect(),
            )),
            (left, right, _) => Err(format!("Cannot range {:?} and {:?}", left, right)),
        },
        (Value::Rune(left), Value::Rune(right)) => {
            if !inclusive {
                Ok(Value::Array((left..right).map(Value::Rune).collect()))
            } else {
                Ok(Value::Array((left..=right).map(Value::Rune).collect()))
            }
        }
        (left, right) => Err(format!("Cannot range {:?} and {:?}", left, right)),
    }
}
