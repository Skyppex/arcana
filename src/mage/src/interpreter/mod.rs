pub(super) mod environment;
pub(super) mod value;
pub(super) mod evaluator;
pub(super) mod evaluate_binop;
pub(super) mod scope;

pub use evaluator::evaluate;