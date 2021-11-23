use crate::token::Literal;
use crate::token::Token;
use crate::token::TokenType;

#[derive(Debug)]
pub enum Expr<'a> {
    Literal(&'a Token),
    Grouping(Box<Expr<'a>>),
    Unary(&'a Token, Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, &'a Token, Box<Expr<'a>>),
    Variable(&'a Token),
    Assign(&'a Token, Box<Expr<'a>>)
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Block(Vec<Stmt<'a>>),
    Expression(Box<Expr<'a>>),
    Print(Box<Expr<'a>>),
    Declaration(&'a Token, Option<Box<Expr<'a>>>),
}

#[derive(Debug)]
pub struct ParseError<'a> {
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

#[allow(dead_code)]
pub fn expr_to_str(expr: &Expr) -> String {
    let mut s = String::new();
    match expr {
        Expr::Literal(t) => {
            match t.token_type {
                TokenType::String | TokenType::Number => match &t.literal {
                    None => panic!("String token has no literal"),
                    Some(Literal::String(ss)) => s.push_str(&ss),
                    Some(Literal::Number(n)) => s.push_str(&n.to_string()),
                    Some(Literal::Boolean(b)) => s.push_str(&b.to_string()),
                },
                //TokenType::True | TokenType::False | TokenType::Nil => {
                _ => s.push_str(&format!("{:?}", t.token_type)),
            }
        }
        Expr::Grouping(expr) => {
            s.push_str(&parenthesize!(" group", expr_to_str(expr)))
        }
        Expr::Unary(t, expr) => {
            s.push_str(&parenthesize!(t.lexeme, expr_to_str(expr)))
        }
        Expr::Binary(left, op, right) => {
            s.push_str(&parenthesize!(op.lexeme, expr_to_str(left), expr_to_str(right)))
        }
        Expr::Variable(t) => {
            s.push_str(t.lexeme.as_str())
        }
        Expr::Assign(t, expr) => {
            s.push_str(&parenthesize!(t.lexeme, " = ", expr_to_str(expr)))
        }
    }
    s
}

pub fn parse(tokens: &[Token]) -> Result<Vec<Stmt>, Vec<ParseError>> {
    let mut stmts = vec![];
    let mut errors = vec![];
    let mut remaining = tokens;
    while !remaining.is_empty() {
        // this feels very hacky, but otherwise we get a parse error on blank lines at the end
        if let Some(Token {token_type: TokenType::EOF, ..}) = remaining.first() {
            break;
        }
        match statement(remaining) {
            Ok((stmt, rest)) => {
                stmts.push(stmt);
                remaining = rest;
                // println!("{:?}", stmts);
            } 
            Err((err, rest)) => {
                // println!("{:?}", err);
                remaining = synchronize(rest);
                errors.push(err)
            }
        }
    }
    if errors.len() > 0 {
        Err(errors)
    } else {
        Ok(stmts)
    }
}

fn synchronize(tokens: &[Token]) -> &[Token] {
    // println!("parser in panic mode {:?}", tokens);
    match tokens.first() {
        Some(Token {token_type: TokenType::Semicolon, ..}) => tokens.split_first().unwrap().1,
        Some(Token {token_type: TokenType::Class, ..}) |
        Some(Token {token_type: TokenType::Fun, ..}) |
        Some(Token {token_type: TokenType::Var, ..}) |
        Some(Token {token_type: TokenType::For, ..}) |
        Some(Token {token_type: TokenType::If, ..}) |
        Some(Token {token_type: TokenType::While, ..}) |
        Some(Token {token_type: TokenType::Return, ..}) |
        Some(Token {token_type: TokenType::Print, ..}) => tokens,
        Some(_) => synchronize(tokens.split_first().unwrap().1),
        None => &[]
    }
}

// parse a series of statements contained within curly braces
// assumes the opening brace has already been matched (but not consumed)
fn block(tokens: &[Token]) -> Result<(Vec<Stmt>, &[Token]), (ParseError, &[Token])> {
    let (_, mut remaining) = tokens.split_first().unwrap();
    let mut stmts = vec![];
    loop {
        // check for a } ending the block or the end of file
        match remaining.split_first() {
            Some((Token {token_type: TokenType::RightBrace, ..}, rest)) => {
                return Ok((stmts, rest));
            }
            None => {
                return Err((ParseError { token: None, 
                    message: "EOF reached without closing }".to_string()}, remaining));
            }
            // block continues
            _ => {
                match statement(remaining) {
                    Ok((stmt, rest)) => {
                        stmts.push(stmt);
                        remaining = rest;
                    } 
                    Err((err, rest)) => {
                        return Err((err, rest));
                    }
                }
            }
        }
    }
}

fn statement(tokens: &[Token]) -> Result<(Stmt, &[Token]), (ParseError, &[Token])> {
    match tokens.first() {
        // block
        Some(Token {token_type: TokenType::LeftBrace, ..}) => {
            match block(tokens) {
                Ok((stmts, rest)) => Ok((Stmt::Block(stmts), rest)),
                Err((err, rest)) => Err((err, rest))
            }
        }
        // variable declaration
        Some(Token {token_type: TokenType::Var, ..}) => { // match var keyword
            let (_, remaining) = tokens.split_first().unwrap();
            match remaining.split_first() {
                Some((ident @ Token {token_type: TokenType::Identifier, ..}, rest)) => { // var must be followed by an identifier
                    match rest.split_first() {
                        Some((Token {token_type: TokenType::Equal, ..}, rest)) => { // then an '='
                            // and finally an expression followed by a ';'
                            let (expr, remaining) = expression(rest)?;
                            if let Some(Token {token_type: TokenType::Semicolon, ..}) = remaining.first() {
                                Ok((Stmt::Declaration(ident, Some(Box::new(expr))), remaining.split_first().unwrap().1))
                            } else {
                                Err((ParseError { token: rest.first(), 
                                     message: "Expected ';' after variable declaration".to_string()}, remaining))
                            }
                        }
                        Some((Token {token_type: TokenType::Semicolon, ..}, rest)) => { // or a ';'
                            Ok((Stmt::Declaration(ident, None), rest))
                        }
                        _ => Err((ParseError { token: remaining.first(), 
                            message: "Expected '=' or ';' after variable name in declaration".to_string()}, rest))
                    }
                }
                Some((t, _)) => Err((ParseError { token: Some(t), 
                            message: "Expected an identifier after 'var'".to_string()}, remaining)),
                None => Err((ParseError { token: tokens.first(), 
                            message: "Expected an identifier after 'var'".to_string()}, remaining)),
            }
        }
        // print statement
        Some(Token {token_type: TokenType::Print, ..}) => {
            let (_, rest) = tokens.split_first().unwrap();
            let (expr, remaining) = expression(rest)?;
            if let Some(Token {token_type: TokenType::Semicolon, ..}) = remaining.first() {
                Ok((Stmt::Print(Box::new(expr)), remaining.split_first().unwrap().1))
            } else {
                Err((ParseError { token: rest.first(), 
                    message: "Expected ';' after value".to_string()}, remaining))
            }
        }
        // expression statement
        Some(_) => {
            let (expr, remaining) = expression(tokens)?;
            if let Some(Token {token_type: TokenType::Semicolon, ..}) = remaining.first() {
                Ok((Stmt::Expression(Box::new(expr)), remaining.split_first().unwrap().1))
            } else {
                Err((ParseError { token: tokens.first(), 
                    message: "Expected ';' after expression".to_string()}, remaining))
            }
        }
        None => panic!("parser::statement called on empty slice of tokens")
    }
}

fn expression(tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    assignment(tokens)
}

fn assignment(tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    let (expr, remaining) = equality(tokens)?; // left-hand side
    
    match remaining.first() {
        Some(Token {token_type: TokenType::Equal, ..}) => { // then a '='
            match expr {
                Expr::Variable(name) => { // left hand side must be a variable (TODO: object field)
                    if let Some((_, rest)) = remaining.split_first() {
                        let (right, remaining) = assignment(rest)?;
                        return Ok((Expr::Assign(name, Box::new(right)), remaining));
                    } else {
                        return Err((ParseError {
                            token: Some(name),
                            message: format!("found assignment with no right-hand expression.")
                        }, remaining));
                    }
                }
                _ => Err((ParseError {
                    token: remaining.first(),
                    message: format!("{:?} invalid assignment target", expr)
                }, remaining))
            }
        }
        _ => { return Ok((expr, remaining)) }
    }
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
                            return Err((ParseError {
                                token: Some(token),
                                message: format!("could not parse right hand side of {}", $cur_fn)
                            }, remaining));
                        }
                    }
                    _ => { return Ok((expr, remaining)); }
                }
            }
        }
    };
}

fn equality(tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    // match COMPARISON (!= | == COMPARISON)*

    parse_binary_expr!(tokens, "equality", comparison, TokenType::BangEqual, TokenType::EqualEqual)
}

fn comparison(tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    // match TERM (< | <= | > | >= TERM)*

    parse_binary_expr!(tokens, "comparison", term, TokenType::Less, TokenType::LessEqual, 
        TokenType::Greater, TokenType::GreaterEqual)
}

fn term(tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    // match FACTOR (+ | - FACTOR)*

    parse_binary_expr!(tokens, "term", factor, TokenType::Minus, TokenType::Plus)    
}

fn factor(tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    // match UNARY (* | / UNARY)*

    parse_binary_expr!(tokens, "factor", unary, TokenType::Star, TokenType::Slash)
}

fn unary(tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    // match ! | - (UNARY | PRIMARY)

    match tokens.first() {
        Some(token @ Token {token_type: TokenType::Bang, ..}) |
        Some(token @ Token {token_type: TokenType::Minus, ..}) => {
            if let Some((_, rest)) = tokens.split_first() {
                let (right, remaining) = unary(rest)?;
                return Ok((Expr::Unary(token, Box::new(right)), remaining));
            } else {
                return Err((ParseError {
                    token: Some(token),
                    message: format!("found unary with no right-hand expression")
                }, tokens));
            }
        }
        _ => { return primary(tokens); }
    }
}

fn primary(tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    // match NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"

    match tokens.split_first() {
        Some((token @ Token {token_type: TokenType::False, ..}, rest))  |
        Some((token @ Token {token_type: TokenType::True, ..}, rest))   |
        Some((token @ Token {token_type: TokenType::Nil, ..}, rest))    |
        Some((token @ Token {token_type: TokenType::Number, ..}, rest)) |
        Some((token @ Token {token_type: TokenType::String, ..}, rest)) => Ok((Expr::Literal(token), rest)),
        Some((token @ Token {token_type: TokenType::Identifier, ..}, rest)) => Ok((Expr::Variable(token), rest)),
        Some((token @ Token {token_type: TokenType::LeftParen, ..}, rest)) => {
            let (expr, remaining) = expression(rest)?;
            if let Some((next, remaining_trimmed)) = remaining.split_first() {
                if matches!(next.token_type, TokenType::RightParen) {
                    Ok((Expr::Grouping(Box::new(expr)), remaining_trimmed))
                } else {
                    Err((ParseError {
                        token: Some(next),
                        message: format!("Expected ')', found {:?}", next)
                    }, remaining))
                }
            } else {
                Err((ParseError {
                    token: Some(token),
                    message: format!("No closing ')' found")
                }, remaining))
            }
        }
        Some((token, _)) => Err((ParseError {
            token: Some(token),
            message: format!("Invalid token {:?}", token)
        }, tokens)),
        None => Err((ParseError {
            token: None,
            message: format!("Reached the end of file while parsing")
        }, tokens))
    }
}