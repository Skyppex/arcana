use shared::type_checker::ast::BinaryOperator;

use super::value::{Number, Value};

pub(crate) fn evaluate_binop<'a>(
    left: Value,
    operator: BinaryOperator,
    right: Value,
) -> Result<Value, String> {
    match operator {
        BinaryOperator::Add => evaluate_add(left, right),
        BinaryOperator::Subtract => evaluate_subtract(left, right),
        BinaryOperator::Multiply => evaluate_multiply(left, right),
        BinaryOperator::Divide => evaluate_divide(left, right),
        BinaryOperator::Modulo => evaluate_modulo(left, right),
        BinaryOperator::BitwiseAnd => evaluate_bitwise_and(left, right),
        BinaryOperator::BitwiseOr => evaluate_bitwise_or(left, right),
        BinaryOperator::BitwiseXor => evaluate_bitwise_xor(left, right),
        BinaryOperator::BitwiseLeftShift => evaluate_bitwise_left_shift(left, right),
        BinaryOperator::BitwiseRightShift => evaluate_bitwise_right_shift(left, right),
        BinaryOperator::LogicalAnd => evaluate_boolean_logical_and(left, right),
        BinaryOperator::LogicalOr => evaluate_boolean_logical_or(left, right),
        BinaryOperator::Equal => evaluate_equal(left, right),
        BinaryOperator::NotEqual => evaluate_not_equal(left, right),
        BinaryOperator::LessThan => evaluate_less_than(left, right),
        BinaryOperator::LessThanOrEqual => evaluate_less_than_or_equal(left, right),
        BinaryOperator::GreaterThan => evaluate_greater_than(left, right),
        BinaryOperator::GreaterThanOrEqual => evaluate_greater_than_or_equal(left, right),
    }
}

fn evaluate_add<'a>(left: Value, right: Value) -> Result<Value, String> {
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
        (left, right) => Err(format!("Cannot add {:?} and {:?}", left, right)),
    }
}

fn evaluate_subtract<'a>(left: Value, right: Value) -> Result<Value, String> {
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

fn evaluate_multiply<'a>(left: Value, right: Value) -> Result<Value, String> {
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

fn evaluate_divide<'a>(left: Value, right: Value) -> Result<Value, String> {
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

fn evaluate_modulo<'a>(left: Value, right: Value) -> Result<Value, String> {
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

fn evaluate_bitwise_and<'a>(left: Value, right: Value) -> Result<Value, String> {
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

fn evaluate_bitwise_or<'a>(left: Value, right: Value) -> Result<Value, String> {
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

fn evaluate_bitwise_xor<'a>(left: Value, right: Value) -> Result<Value, String> {
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

fn evaluate_bitwise_left_shift<'a>(left: Value, right: Value) -> Result<Value, String> {
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

fn evaluate_bitwise_right_shift<'a>(left: Value, right: Value) -> Result<Value, String> {
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

fn evaluate_boolean_logical_and<'a>(left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(left && right)),
        (left, right) => Err(format!(
            "Cannot boolean logical and {:?} and {:?}",
            left, right
        )),
    }
}

fn evaluate_boolean_logical_or<'a>(left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(left || right)),
        (left, right) => Err(format!(
            "Cannot boolean logical or {:?} and {:?}",
            left, right
        )),
    }
}

fn evaluate_equal<'a>(left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Bool(left == right)),
            (Number::UInt(left), Number::UInt(right)) => Ok(Value::Bool(left == right)),
            (Number::Float(left), Number::Float(right)) => Ok(Value::Bool(left == right)),
            (left, right) => Err(format!("Cannot equal {:?} and {:?}", left, right)),
        },
        (Value::String(left), Value::String(right)) => Ok(Value::Bool(left == right)),
        (Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(left == right)),
        (Value::Unit, Value::Unit) => Ok(Value::Bool(true)),
        (left, right) => Err(format!("Cannot equal {:?} and {:?}", left, right)),
    }
}

fn evaluate_not_equal<'a>(left: Value, right: Value) -> Result<Value, String> {
    match (left, right) {
        (Value::Number(left), Value::Number(right)) => match (left, right) {
            (Number::Int(left), Number::Int(right)) => Ok(Value::Bool(left != right)),
            (Number::UInt(left), Number::UInt(right)) => Ok(Value::Bool(left != right)),
            (Number::Float(left), Number::Float(right)) => Ok(Value::Bool(left != right)),
            (left, right) => Err(format!("Cannot not equal {:?} and {:?}", left, right)),
        },
        (Value::String(left), Value::String(right)) => Ok(Value::Bool(left != right)),
        (Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(left != right)),
        (Value::Unit, Value::Unit) => Ok(Value::Bool(false)),
        (left, right) => Err(format!("Cannot not equal {:?} and {:?}", left, right)),
    }
}

fn evaluate_less_than<'a>(left: Value, right: Value) -> Result<Value, String> {
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

fn evaluate_less_than_or_equal<'a>(left: Value, right: Value) -> Result<Value, String> {
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

fn evaluate_greater_than<'a>(left: Value, right: Value) -> Result<Value, String> {
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

fn evaluate_greater_than_or_equal<'a>(left: Value, right: Value) -> Result<Value, String> {
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
