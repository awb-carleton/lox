use crate::parser::Expr;
use crate::token::TokenType;
use crate::token::Literal;
use crate::parser::expr_to_str;
use std::fmt;
use std::fmt::Display;

#[derive(PartialEq)]
pub enum Value {
    Number(f64),
    String(Box<str>),
    Boolean(bool),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    pub expr: String,
    pub message: String,
}

pub fn evaluate(expr: &Expr) -> Result<Value, RuntimeError> {
    match expr {
        Expr::Literal(t) => {
            match &t.literal {
                Some(Literal::String(s)) => Ok(Value::String(s.clone())),
                Some(Literal::Number(n)) => Ok(Value::Number(*n)),
                _ => Err(RuntimeError {expr: expr_to_str(expr), message: "Invalid literal expression".to_string()})
            }
        }
        Expr::Grouping(ex) => evaluate(ex),
        Expr::Unary(op, ex) => {
            let right = evaluate(ex)?;
            match (right, op.token_type) {
                (Value::Number(num), TokenType::Minus) => Ok(Value::Number(-num)),
                (val, TokenType::Bang) => Ok(Value::Boolean(!is_truthy(val))),
                _ => Err(RuntimeError {
                    expr: expr_to_str(expr),
                    message: "Invalid unary expression".to_string(),
                }),
            }
        }
        Expr::Binary(left_ex, op, right_ex) => {
            let left = evaluate(left_ex)?;
            let right = evaluate(right_ex)?;
            match (left, op.token_type, right) {
                (Value::Number(l), TokenType::Star, Value::Number(r)) => Ok(Value::Number(l * r)),
                (Value::Number(l), TokenType::Slash, Value::Number(r)) => Ok(Value::Number(l / r)),
                (Value::Number(l), TokenType::Minus, Value::Number(r)) => Ok(Value::Number(l - r)),
                (Value::Number(l), TokenType::Plus, Value::Number(r)) => Ok(Value::Number(l + r)),
                (Value::String(l), TokenType::Plus, Value::String(r)) => {
                    let s = (l.to_string() + &*r).into_boxed_str();
                    Ok(Value::String(s))
                }
                (Value::Number(l), TokenType::Greater, Value::Number(r)) => {
                    Ok(Value::Boolean(l > r))
                }
                (Value::Number(l), TokenType::GreaterEqual, Value::Number(r)) => {
                    Ok(Value::Boolean(l >= r))
                }
                (Value::Number(l), TokenType::Less, Value::Number(r)) => Ok(Value::Boolean(l < r)),
                (Value::Number(l), TokenType::LessEqual, Value::Number(r)) => {
                    Ok(Value::Boolean(l <= r))
                }
                (l, TokenType::EqualEqual, r) => Ok(Value::Boolean(l == r)),
                (l, TokenType::BangEqual, r) => Ok(Value::Boolean(l != r)),
                _ => Err(RuntimeError {
                    expr: expr_to_str(expr),
                    message: "Invalid binary expression".to_string(),
                }),
            }
        }
    }
}

fn is_truthy(val: Value) -> bool {
    match val {
        Value::Nil => false,
        Value::Boolean(b) if !b => false,
        _ => true,
    }
}
