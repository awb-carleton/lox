use crate::token::Literal;
use crate::token::Token;
use crate::token::TokenType;

pub enum Expr<'a> {
    Literal(&'a Token),
    Grouping(Box<Expr<'a>>),
    Unary(&'a Token, Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, &'a Token, Box<Expr<'a>>),
}

#[derive(Debug)]
pub enum ParseError {
    Incomplete(String),
    Invalid
}

macro_rules! parenthesize {
    ($name:expr, $($es:expr),+) => {
        {
            let mut paren_str = String::new();
            paren_str.push('(');
            paren_str.push_str(&$name);
            $(
                paren_str.push(' ');
                paren_str.push_str(&$es);
            )+
            paren_str.push(')');
            paren_str
        }
    }
}

pub fn expr_to_str(expr: Expr) -> String {
    let mut s = String::new();
    match expr {
        Expr::Literal(t) => {
            match t.token_type {
                TokenType::String | TokenType::Number => match &t.literal {
                    None => panic!("String token has no literal"),
                    Some(Literal::String(ss)) => s.push_str(&ss),
                    Some(Literal::Number(n)) => s.push_str(&n.to_string()),
                },
                //TokenType::True | TokenType::False | TokenType::Nil => {
                _ => s.push_str(&format!("{:?}", t.token_type)),
            }
        }
        Expr::Grouping(expr) => {
            s.push_str(&parenthesize!(" group", expr_to_str(*expr)))
        }
        Expr::Unary(t, expr) => {
            s.push_str(&parenthesize!(t.lexeme, expr_to_str(*expr)))
        }
        Expr::Binary(left, op, right) => {
            s.push_str(&parenthesize!(op.lexeme, expr_to_str(*left), expr_to_str(*right)))
        }
    }
    s
}

pub fn parse(tokens: &[Token]) -> Result<Expr, ParseError> {
    let (expr, _) = expression(tokens)?;
    Ok(expr)
}

fn expression(tokens: &[Token]) -> Result<(Expr, &[Token]), ParseError> {
    equality(tokens)
}

fn equality(tokens: &[Token]) -> Result<(Expr, &[Token]), ParseError> {
    // match COMPARISON (!= | == COMPARISON)*

    let (mut expr, mut remaining) = comparison(tokens)?;

    loop {
        match remaining.first() {
            Some(token @ Token {token_type: TokenType::BangEqual, ..}) |
            Some(token @ Token {token_type: TokenType::EqualEqual, ..}) => {
                if let Some((_, rest)) = remaining.split_first() {
                    let temp = comparison(rest)?;
                    remaining = temp.1;
                    expr = Expr::Binary(Box::new(expr), token, Box::new(temp.0));
                } else {
                    return Err(ParseError::Incomplete(format!("found equality with no right-hand expression {:?}", tokens)));
                }
            }
            _ => { return Ok((expr, remaining)); }
        }
    }
}

fn comparison(tokens: &[Token]) -> Result<(Expr, &[Token]), ParseError> {
    // match TERM (< | <= | > | >= TERM)*

    let (mut expr, mut remaining) = term(tokens)?;

    loop {
        match remaining.first() {
            Some(token @ Token {token_type: TokenType::Less, ..}) |
            Some(token @ Token {token_type: TokenType::LessEqual, ..}) |
            Some(token @ Token {token_type: TokenType::Greater, ..}) |
            Some(token @ Token {token_type: TokenType::GreaterEqual, ..}) => {
                if let Some((_, rest)) = remaining.split_first() {
                    let temp = term(rest)?;
                    remaining = temp.1;
                    expr = Expr::Binary(Box::new(expr), token, Box::new(temp.0));
                } else {
                    return Err(ParseError::Incomplete(format!("found comparison with no right-hand expression {:?}", tokens)));
                }
            }
            _ => { return Ok((expr, remaining)); }
        }
    }
}

fn term(tokens: &[Token]) -> Result<(Expr, &[Token]), ParseError> {
    // match FACTOR (+ | - FACTOR)*

    let (mut expr, mut remaining) = factor(tokens)?;

    loop {
        match remaining.first() {
            Some(token @ Token {token_type: TokenType::Minus, ..}) |
            Some(token @ Token {token_type: TokenType::Plus, ..}) => {
                if let Some((_, rest)) = remaining.split_first() {
                    let temp = factor(rest)?;
                    remaining = temp.1;
                    expr = Expr::Binary(Box::new(expr), token, Box::new(temp.0));
                } else {
                    return Err(ParseError::Incomplete(format!("found term with no right-hand expression {:?}", tokens)));
                }
            }
            _ => { return Ok((expr, remaining)); }
        }
    }
}

fn factor(tokens: &[Token]) -> Result<(Expr, &[Token]), ParseError> {
    // match UNARY (* | / UNARY)*

    let (mut expr, mut remaining) = unary(tokens)?;

    loop {
        match remaining.first() {
            Some(token @ Token {token_type: TokenType::Star, ..}) |
            Some(token @ Token {token_type: TokenType::Slash, ..}) => {
                if let Some((_, rest)) = remaining.split_first() {
                    let temp = unary(rest)?;
                    remaining = temp.1;
                    expr = Expr::Binary(Box::new(expr), token, Box::new(temp.0));
                } else {
                    return Err(ParseError::Incomplete(format!("found factor with no right-hand expression {:?}", tokens)));
                }
            }
            _ => { return Ok((expr, remaining)); }
        }
    }
}

fn unary(tokens: &[Token]) -> Result<(Expr, &[Token]), ParseError> {
    // match ! | - (UNARY | PRIMARY)

    match tokens.first() {
        Some(token @ Token {token_type: TokenType::Bang, ..}) |
        Some(token @ Token {token_type: TokenType::Minus, ..}) => {
            if let Some((_, rest)) = tokens.split_first() {
                let (right, remaining) = unary(rest)?;
                return Ok((Expr::Unary(token, Box::new(right)), remaining));
            } else {
                return Err(ParseError::Incomplete(format!("found factor with no right-hand expression {:?}", tokens)));
            }
        }
        _ => { return primary(tokens); }
    }
}

fn primary(tokens: &[Token]) -> Result<(Expr, &[Token]), ParseError> {
    // match NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"

    match tokens.split_first() {
        Some((token @ Token {token_type: TokenType::False, ..}, rest))  |
        Some((token @ Token {token_type: TokenType::True, ..}, rest))   |
        Some((token @ Token {token_type: TokenType::Nil, ..}, rest))    |
        Some((token @ Token {token_type: TokenType::Number, ..}, rest)) |
        Some((token @ Token {token_type: TokenType::String, ..}, rest)) => Ok((Expr::Literal(token), rest)),
        Some((Token {token_type: TokenType::LeftParen, ..}, rest)) => {
            let (expr, remaining) = expression(rest)?;
            if let Some((next, remaining_trimmed)) = remaining.split_first() {
                if matches!(next.token_type, TokenType::RightParen) {
                    Ok((Expr::Grouping(Box::new(expr)), remaining_trimmed))
                } else {
                    Err(ParseError::Incomplete(format!("Expected ')', found {:?}", next)))
                }
            } else {
                Err(ParseError::Incomplete(format!("No closing ')' found {:?}", remaining)))
            }
        }
        _ => Err(ParseError::Invalid)
    }
}