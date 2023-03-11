use crate::interpreter::RuntimeError;
use crate::interpreter::Value;
use crate::token::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Environment {
    variables: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(enclosing: Option<Rc<RefCell<Environment>>>) -> Environment {
        Environment {
            variables: HashMap::new(),
            enclosing: enclosing,
        }
    }

    pub fn define(&mut self, name: &Token, v: Value) -> () {
        self.variables.insert(name.lexeme.clone(), v);
    }

    pub fn assign(&mut self, name: &Token, v: Value) -> Result<Value, RuntimeError> {
        match self.variables.get(&name.lexeme) {
            Some(_) => {
                self.variables.insert(name.lexeme.clone(), v.clone());
                Ok(v)
            }
            None => match &self.enclosing {
                None => Err(RuntimeError {
                    expr: name.lexeme.clone(),
                    line: name.line,
                    message: format!("Undefined variable '{}'.", name.lexeme),
                }),
                Some(env) => env.borrow_mut().assign(name, v),
            },
        }
    }

    pub fn get(&self, name: &Token) -> Result<Value, RuntimeError> {
        match self.variables.get(&name.lexeme) {
            Some(v) => Ok(v.clone()),
            None => match &self.enclosing {
                None => Err(RuntimeError {
                    expr: name.lexeme.clone(),
                    line: name.line,
                    message: format!("Undefined variable '{}'.", name.lexeme),
                }),
                Some(env) => env.borrow().get(name),
            },
        }
    }
}
