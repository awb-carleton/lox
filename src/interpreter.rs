use crate::environment::Environment;
use crate::parser::expr_to_str;
use crate::parser::Expr;
use crate::parser::Stmt;
use crate::token::TokenType;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Display;
use std::rc::Rc;

#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    Number(f64),
    String(Rc<str>),
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
    pub line: u32,
    pub message: String,
}

pub fn interpret(
    stmts: &Vec<Stmt>,
    env: Rc<RefCell<Environment>>,
) -> Result<(), Vec<RuntimeError>> {
    let mut errors = vec![];
    for stmt in stmts {
        match execute(stmt, Rc::clone(&env)) {
            Ok(_) => {}
            Err(err) => errors.push(err),
        }
    }
    if errors.len() > 0 {
        Err(errors)
    } else {
        Ok(())
    }
}

fn execute(stmt: &Stmt, env: Rc<RefCell<Environment>>) -> Result<(), RuntimeError> {
    log::debug!(
        "execute {:?} with env {:?} ref count: {}",
        stmt,
        env,
        Rc::strong_count(&env)
    );
    match stmt {
        Stmt::Block(stmts) => {
            let block_env = Rc::new(RefCell::new(Environment::new(Some(env.clone()))));
            for stmt in stmts.iter() {
                match execute(stmt, Rc::clone(&block_env)) {
                    Ok(_) => {}
                    Err(err) => {
                        return Err(err);
                    }
                }
            }
            Ok(())
        }
        Stmt::Expression(expr) => match evaluate(expr, env) {
            Ok(_) => Ok(()),
            Err(err) => Err(err),
        },
        Stmt::Print(expr) => match evaluate(expr, env) {
            Ok(val) => {
                println!("{}", val);
                Ok(())
            }
            Err(err) => Err(err),
        },
        Stmt::Declaration(name, init) => match init {
            Some(expr) => match evaluate(expr, env.clone()) {
                Ok(val) => {
                    env.borrow_mut().define(name, val);
                    Ok(())
                }
                Err(err) => Err(err),
            },
            None => {
                env.borrow_mut().define(name, Value::Nil);
                Ok(())
            }
        },
        Stmt::If(condition, then_branch, else_branch) => match evaluate(&condition, env.clone()) {
            Ok(val) => {
                if is_truthy(&val) {
                    execute(then_branch, env)
                } else {
                    match else_branch {
                        Some(s) => execute(s, env),
                        None => Ok(()),
                    }
                }
            }
            Err(err) => Err(err),
        },
        Stmt::While(condition, body) => loop {
            match evaluate(&condition, env.clone()) {
                Ok(val) => {
                    if is_truthy(&val) {
                        match execute(body, env.clone()) {
                            Ok(()) => (),
                            Err(err) => return Err(err),
                        }
                    } else {
                        return Ok(());
                    }
                }
                Err(err) => return Err(err),
            }
        },
    }
}

fn evaluate(expr: &Expr, env: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
    log::debug!(
        "evaluate {:?} with env {:?} ref count: {}",
        expr,
        env,
        Rc::strong_count(&env)
    );
    match expr {
        Expr::Literal(v) => Ok(v.clone()),
        Expr::Variable(name) => env.borrow().get(name),
        Expr::Assign(name, expr) => match evaluate(expr, env.clone()) {
            Ok(val) => env.borrow_mut().assign(name, val.clone()),
            Err(err) => Err(err),
        },
        Expr::Grouping(ex) => evaluate(ex, env),
        Expr::Unary(op, ex) => {
            let right = evaluate(ex, env)?;
            match (right, op.token_type) {
                (Value::Number(num), TokenType::Minus) => Ok(Value::Number(-num)),
                (val, TokenType::Bang) => Ok(Value::Boolean(!is_truthy(&val))),
                _ => Err(RuntimeError {
                    expr: expr_to_str(expr),
                    line: op.line,
                    message: "Invalid unary expression".to_string(),
                }),
            }
        }
        Expr::Logical(left_ex, op, right_ex) => {
            let left = evaluate(left_ex, env.clone())?;
            match (is_truthy(&left), op.token_type) {
                (true, TokenType::Or) | (false, TokenType::And) => Ok(left),
                (false, TokenType::Or) | (true, TokenType::And) => {
                    let right = evaluate(right_ex, env.clone())?;
                    Ok(right)
                }
                _ => Err(RuntimeError {
                    expr: expr_to_str(expr),
                    line: op.line,
                    message: "Invalid logical expression".to_string(),
                }),
            }
        }
        Expr::Binary(left_ex, op, right_ex) => {
            let left = evaluate(left_ex, env.clone())?;
            let right = evaluate(right_ex, env.clone())?;
            match (left, op.token_type, right) {
                (Value::Number(l), TokenType::Star, Value::Number(r)) => Ok(Value::Number(l * r)),
                (Value::Number(l), TokenType::Slash, Value::Number(r)) => Ok(Value::Number(l / r)),
                (Value::Number(l), TokenType::Minus, Value::Number(r)) => Ok(Value::Number(l - r)),
                (Value::Number(l), TokenType::Plus, Value::Number(r)) => Ok(Value::Number(l + r)),
                (Value::String(l), TokenType::Plus, Value::String(r)) => {
                    let s = (l.to_string() + &*r).into();
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
                    line: op.line,
                    message: "Invalid binary expression".to_string(),
                }),
            }
        }
    }
}

fn is_truthy(val: &Value) -> bool {
    match val {
        Value::Nil => false,
        Value::Boolean(b) if !b => false,
        _ => true,
    }
}
