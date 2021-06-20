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
pub enum ParseErrorType {
    Incomplete,
    Invalid
}

#[derive(Debug)]
pub struct ParseError<'a> {
    pub cause: ParseErrorType,
    pub token: Option<&'a Token>,
    pub message: String
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
    // TODO handle error and consume tokens until next statement
    let (expr, _) = expression(tokens)?;
    Ok(expr)
}

fn expression(tokens: &[Token]) -> Result<(Expr, &[Token]), ParseError> {
    equality(tokens)
}

macro_rules! parse_binary_expr {
    ($tokens:ident, $cur_fn:expr, $precedent_fn:ident, $token_type:path, $($token_types:path),*) => {
        {
            let (mut expr, mut remaining) = $precedent_fn($tokens)?;

            loop {
                match remaining.first() {
                    $( Some(token @ Token {token_type: $token_types, ..}) | )*
                    Some(token @ Token {token_type: $token_type, ..}) => {
                        let (_, rest) = remaining.split_first().unwrap();
                        if let Ok(temp) = $precedent_fn(rest) {
                            remaining = temp.1;
                            expr = Expr::Binary(Box::new(expr), token, Box::new(temp.0));
                        } else {
                            return Err(ParseError {
                                cause: ParseErrorType::Incomplete, 
                                token: Some(token),
                                message: format!("could not parse right hand side of {}", $cur_fn)
                            });
                        }
                    }
                    _ => { return Ok((expr, remaining)); }
                }
            }
        }
    };
}

fn equality(tokens: &[Token]) -> Result<(Expr, &[Token]), ParseError> {
    // match COMPARISON (!= | == COMPARISON)*

    parse_binary_expr!(tokens, "equality", comparison, TokenType::BangEqual, TokenType::EqualEqual)
}

fn comparison(tokens: &[Token]) -> Result<(Expr, &[Token]), ParseError> {
    // match TERM (< | <= | > | >= TERM)*

    parse_binary_expr!(tokens, "comparison", term, TokenType::Less, TokenType::LessEqual, 
        TokenType::Greater, TokenType::GreaterEqual)
}

fn term(tokens: &[Token]) -> Result<(Expr, &[Token]), ParseError> {
    // match FACTOR (+ | - FACTOR)*

    parse_binary_expr!(tokens, "term", factor, TokenType::Minus, TokenType::Plus)    
}

fn factor(tokens: &[Token]) -> Result<(Expr, &[Token]), ParseError> {
    // match UNARY (* | / UNARY)*

    parse_binary_expr!(tokens, "factor", unary, TokenType::Star, TokenType::Slash)
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
                return Err(ParseError {
                    cause: ParseErrorType::Incomplete, 
                    token: Some(token),
                    message: format!("found unary with no right-hand expression")
                });
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
        Some((token @ Token {token_type: TokenType::LeftParen, ..}, rest)) => {
            let (expr, remaining) = expression(rest)?;
            if let Some((next, remaining_trimmed)) = remaining.split_first() {
                if matches!(next.token_type, TokenType::RightParen) {
                    Ok((Expr::Grouping(Box::new(expr)), remaining_trimmed))
                } else {
                    Err(ParseError {
                        cause: ParseErrorType::Incomplete, 
                        token: Some(next),
                        message: format!("Expected ')', found {:?}", next)
                    })
                }
            } else {
                Err(ParseError {
                    cause: ParseErrorType::Incomplete, 
                    token: Some(token),
                    message: format!("No closing ')' found")
                })
            }
        }
        Some((token, _)) => Err(ParseError {
            cause: ParseErrorType::Invalid, 
            token: Some(token),
            message: format!("Invalid token {:?}", token)
        }),
        None => Err(ParseError {
            cause: ParseErrorType::Invalid, 
            token: None,
            message: format!("Reached the end of file while parsing")
        })
    }
}