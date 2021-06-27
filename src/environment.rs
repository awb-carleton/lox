use std::collections::HashMap;
use crate::interpreter::Value;
use crate::interpreter::RuntimeError;
use crate::token::Token;

pub struct Environment {
    variables: HashMap<String, Value>
}

impl Environment {
    pub fn new() -> Environment {
        Environment { variables: HashMap::new() }
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
            None => Err(RuntimeError {expr: name.lexeme.clone(), line: name.line, 
                message: format!("Undefined variable '{}'.", name.lexeme)})
        }
    }

    pub fn get(&self, name: &Token) -> Result<Value, RuntimeError> {
        match self.variables.get(&name.lexeme) {
            Some(v) => Ok(v.clone()),
            None => Err(RuntimeError {expr: name.lexeme.clone(), line: name.line, 
                message: format!("Undefined variable '{}'.", name.lexeme)})
        }
    }
}