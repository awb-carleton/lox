use std::rc::Rc;

use crate::interpreter::Value;
use crate::token::Token;
use crate::token::TokenType;

#[derive(Debug)]
pub enum Expr {
    Literal(Value),
    Grouping(Rc<Expr>),
    Unary(Rc<Token>, Rc<Expr>),
    Logical(Rc<Expr>, Rc<Token>, Rc<Expr>),
    Binary(Rc<Expr>, Rc<Token>, Rc<Expr>),
    Call(Rc<Expr>, Rc<Token>, Vec<Expr>),
    Variable(Rc<Token>),
    Assign(Rc<Token>, Rc<Expr>),
}

#[derive(Debug)]
pub enum Stmt {
    Block(Vec<Stmt>),
    Expression(Rc<Expr>),
    Print(Rc<Expr>),
    Declaration(Rc<Token>, Option<Rc<Expr>>),
    If(Rc<Expr>, Rc<Stmt>, Option<Rc<Stmt>>),
    While(Rc<Expr>, Rc<Stmt>),
}

#[derive(Debug)]
pub struct ParseError<'a> {
    pub token: Option<&'a Token>,
    pub message: String,
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
        Expr::Literal(v) => match v {
            Value::String(ss) => s.push_str(&ss),
            Value::Number(n) => s.push_str(&n.to_string()),
            Value::Boolean(b) => s.push_str(&b.to_string()),
            Value::Nil => s.push_str("nil"),
        },
        Expr::Grouping(expr) => s.push_str(&parenthesize!(" group", expr_to_str(expr))),
        Expr::Unary(t, expr) => s.push_str(&parenthesize!(t.lexeme, expr_to_str(expr))),
        Expr::Logical(left, op, right) => s.push_str(&parenthesize!(
            op.lexeme,
            expr_to_str(left),
            expr_to_str(right)
        )),
        Expr::Binary(left, op, right) => s.push_str(&parenthesize!(
            op.lexeme,
            expr_to_str(left),
            expr_to_str(right)
        )),
        Expr::Variable(t) => s.push_str(t.lexeme.as_str()),
        Expr::Assign(t, expr) => s.push_str(&parenthesize!(t.lexeme, " = ", expr_to_str(expr))),
        Expr::Call(f, _, args) => s.push_str(&parenthesize!(
            expr_to_str(f),
            "(",
            format!(
                "{:?}",
                args.into_iter()
                    .map(|arg| expr_to_str(arg))
                    .collect::<Vec<String>>()
            ),
            ")"
        )),
    }
    s
}

fn next(type_: TokenType, tokens: &[Token]) -> Result<(&Token, &[Token]), (ParseError, &[Token])> {
    log::debug!("next {}", type_);
    match tokens.split_first() {
        Some((token @ Token { token_type: tt, .. }, rest)) if (*tt) == type_ => Ok((token, rest)),
        Some((t, rest)) => Err((
            ParseError {
                token: Some(t),
                message: format!("Expected {}", type_),
            },
            rest,
        )),
        None => make_unexpected_eof(tokens),
    }
}

fn consume(type_: TokenType, tokens: &[Token]) -> Result<&[Token], (ParseError, &[Token])> {
    let (_, rest) = next(type_, tokens)?;
    Ok(rest)
}

fn maybe_consume(type_: TokenType, tokens: &[Token]) -> &[Token] {
    match consume(type_, tokens) {
        Ok(tokens) => tokens,
        Err((_, tokens)) => tokens,
    }
}

enum PeekResult<'a> {
    Match(&'a [Token]),
    NoMatch,
}

fn peek(type_: TokenType, tokens: &[Token]) -> Option<PeekResult> {
    match tokens.split_first() {
        Some((Token { token_type: tt, .. }, rest)) if (*tt) == type_ => {
            Some(PeekResult::Match(rest))
        }
        Some(_) => Some(PeekResult::NoMatch),
        None => None,
    }
}

fn match_(type_: TokenType, tokens: &[Token]) -> bool {
    match peek(type_, tokens) {
        Some(PeekResult::Match(_)) => true,
        _ => false,
    }
}

fn make_unexpected_eof<T>(tokens: &[Token]) -> Result<T, (ParseError, &[Token])> {
    Err((
        ParseError {
            token: None,
            message: "EOF reached without closing }".to_string(),
        },
        tokens,
    ))
}

pub fn parse(tokens: &[Token]) -> Result<Vec<Stmt>, Vec<ParseError>> {
    let mut stmts = vec![];
    let mut errors = vec![];
    let mut remaining = tokens;
    while !remaining.is_empty() {
        // this feels very hacky, but otherwise we get a parse error on blank lines at the end
        if let Some(Token {
            token_type: TokenType::EOF,
            ..
        }) = remaining.first()
        {
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
        Some(Token {
            token_type: TokenType::Semicolon,
            ..
        }) => tokens.split_first().unwrap().1,
        Some(Token {
            token_type: TokenType::Class,
            ..
        })
        | Some(Token {
            token_type: TokenType::Fun,
            ..
        })
        | Some(Token {
            token_type: TokenType::Var,
            ..
        })
        | Some(Token {
            token_type: TokenType::For,
            ..
        })
        | Some(Token {
            token_type: TokenType::If,
            ..
        })
        | Some(Token {
            token_type: TokenType::While,
            ..
        })
        | Some(Token {
            token_type: TokenType::Return,
            ..
        })
        | Some(Token {
            token_type: TokenType::Print,
            ..
        }) => tokens,
        Some(_) => synchronize(tokens.split_first().unwrap().1),
        None => &[],
    }
}

// parse a series of statements contained within curly braces
// assumes the opening brace has already been matched (but not consumed)
fn block(tokens: &[Token]) -> Result<(Vec<Stmt>, &[Token]), (ParseError, &[Token])> {
    let mut tokens = consume(TokenType::LeftBrace, tokens)?;
    let mut stmts = vec![];
    loop {
        // check for a } ending the block or the end of file
        match peek(TokenType::RightBrace, tokens) {
            Some(PeekResult::Match(rest)) => {
                return Ok((stmts, rest));
            }
            None => {
                return Err((
                    ParseError {
                        token: None,
                        message: "EOF reached without closing }".to_string(),
                    },
                    tokens,
                ));
            }
            // block continues
            Some(PeekResult::NoMatch) => match statement(tokens) {
                Ok((stmt, rest)) => {
                    stmts.push(stmt);
                    tokens = rest;
                }
                Err((err, rest)) => {
                    return Err((err, rest));
                }
            },
        }
    }
}

fn parse_block(tokens: &[Token]) -> Result<(Stmt, &[Token]), (ParseError, &[Token])> {
    log::debug!("parse_block");
    match block(tokens) {
        Ok((stmts, rest)) => Ok((Stmt::Block(stmts), rest)),
        Err((err, rest)) => Err((err, rest)),
    }
}

fn parse_variable_declaration(
    tokens: &[Token],
) -> Result<(Stmt, &[Token]), (ParseError, &[Token])> {
    // match var keyword
    log::debug!("parse_variable_declaration");
    let tokens = consume(TokenType::Var, tokens)?;
    let (ident, tokens) = next(TokenType::Identifier, tokens)?;
    match peek(TokenType::Semicolon, tokens) {
        Some(PeekResult::Match(tokens)) => {
            Ok((Stmt::Declaration(Rc::new(ident.clone()), None), tokens))
        }
        Some(PeekResult::NoMatch) => {
            let tokens = consume(TokenType::Equal, tokens)?;
            let (expr, tokens) = expression(tokens)?;
            let tokens = consume(TokenType::Semicolon, tokens)?;
            Ok((
                Stmt::Declaration(Rc::new(ident.clone()), Some(Rc::new(expr))),
                tokens,
            ))
        }
        None => make_unexpected_eof(tokens),
    }
}

fn parse_print(tokens: &[Token]) -> Result<(Stmt, &[Token]), (ParseError, &[Token])> {
    log::debug!("parse_print");
    let tokens = consume(TokenType::Print, tokens)?;
    let (expr, tokens) = expression(tokens)?;
    let tokens = consume(TokenType::Semicolon, tokens)?;
    Ok((Stmt::Print(Rc::new(expr)), tokens))
}

fn parse_if(tokens: &[Token]) -> Result<(Stmt, &[Token]), (ParseError, &[Token])> {
    log::debug!("parse_if");
    let tokens = consume(TokenType::If, tokens)?;
    let tokens = consume(TokenType::LeftParen, tokens)?;
    let (condition, tokens) = expression(tokens)?;
    let tokens = consume(TokenType::RightParen, tokens)?;
    let (then_stmt, tokens) = statement(tokens)?;
    match peek(TokenType::Else, tokens) {
        None => make_unexpected_eof(tokens),
        Some(PeekResult::Match(tokens)) => {
            let (else_stmt, tokens) = statement(tokens)?;
            Ok((
                Stmt::If(
                    Rc::new(condition),
                    Rc::new(then_stmt),
                    Some(Rc::new(else_stmt)),
                ),
                tokens,
            ))
        }
        Some(PeekResult::NoMatch) => Ok((
            Stmt::If(Rc::new(condition), Rc::new(then_stmt), None),
            tokens,
        )),
    }
}

fn parse_while(tokens: &[Token]) -> Result<(Stmt, &[Token]), (ParseError, &[Token])> {
    log::debug!("parse_while");
    let tokens = consume(TokenType::While, tokens)?;
    let tokens = consume(TokenType::LeftParen, tokens)?;
    let (condition, tokens) = expression(tokens)?;
    let tokens = consume(TokenType::RightParen, tokens)?;
    let (body, tokens) = statement(tokens)?;
    Ok((Stmt::While(Rc::new(condition), Rc::new(body)), tokens))
}

fn parse_expr_or_declaration(tokens: &[Token]) -> Result<(Stmt, &[Token]), (ParseError, &[Token])> {
    log::debug!("parse_expr_or_declaration");
    match peek(TokenType::Var, tokens) {
        // parse_variable_declaration expects tokens to start with Var, so don't pass the trimmed version
        Some(PeekResult::Match(_)) => parse_variable_declaration(tokens),
        Some(PeekResult::NoMatch) => {
            let (expr, tokens) = expression(tokens)?;
            let tokens = consume(TokenType::Semicolon, tokens)?;
            Ok((Stmt::Expression(Rc::new(expr)), tokens))
        }
        None => return make_unexpected_eof(tokens),
    }
}

fn parse_for(tokens: &[Token]) -> Result<(Stmt, &[Token]), (ParseError, &[Token])> {
    log::debug!("parse_for");
    let tokens = consume(TokenType::For, tokens)?;
    let tokens = consume(TokenType::LeftParen, tokens)?;
    let (initializer, tokens) = match peek(TokenType::Semicolon, tokens) {
        Some(PeekResult::Match(tokens)) => (None, tokens),
        Some(PeekResult::NoMatch) => {
            let (expr, tokens) = parse_expr_or_declaration(tokens)?;
            (Some(expr), tokens)
        }
        None => return make_unexpected_eof(tokens),
    };
    let (condition, tokens) = match peek(TokenType::Semicolon, tokens) {
        Some(PeekResult::Match(tokens)) => (None, tokens),
        Some(PeekResult::NoMatch) => {
            let (expr, tokens) = expression(tokens)?;
            let tokens = consume(TokenType::Semicolon, tokens)?;
            (Some(expr), tokens)
        }
        None => return make_unexpected_eof(tokens),
    };
    let (increment, tokens) = match peek(TokenType::RightParen, tokens) {
        Some(PeekResult::Match(tokens)) => (None, tokens),
        Some(PeekResult::NoMatch) => {
            let (expr, tokens) = expression(tokens)?;
            let tokens = consume(TokenType::RightParen, tokens)?;
            (Some(expr), tokens)
        }
        None => return make_unexpected_eof(tokens),
    };
    let (body, tokens) = statement(tokens)?;
    let body = match increment {
        Some(increment) => Stmt::Block(vec![body, Stmt::Expression(Rc::new(increment))]),
        None => body,
    };
    let condition = match condition {
        Some(condition) => condition,
        None => Expr::Literal(Value::Boolean(true)),
    };
    let body = Stmt::While(Rc::new(condition), Rc::new(body));
    let body = match initializer {
        Some(initializer) => Stmt::Block(vec![initializer, body]),
        None => body,
    };
    Ok((body, tokens))
}

fn parse_expr(tokens: &[Token]) -> Result<(Stmt, &[Token]), (ParseError, &[Token])> {
    log::debug!("parse_expr");
    let (expr, tokens) = expression(tokens)?;
    let tokens = consume(TokenType::Semicolon, tokens)?;
    Ok((Stmt::Expression(Rc::new(expr)), tokens))
}

fn statement(tokens: &[Token]) -> Result<(Stmt, &[Token]), (ParseError, &[Token])> {
    log::debug!("statement");
    match tokens.first() {
        // block
        Some(Token {
            token_type: TokenType::LeftBrace,
            ..
        }) => parse_block(tokens),
        // variable declaration
        Some(Token {
            token_type: TokenType::Var,
            ..
        }) => parse_variable_declaration(tokens),
        // print statement
        Some(Token {
            token_type: TokenType::Print,
            ..
        }) => parse_print(tokens),
        // if statement
        Some(Token {
            token_type: TokenType::If,
            ..
        }) => {
            // match the if
            parse_if(tokens)
        }
        // while statement
        Some(Token {
            token_type: TokenType::While,
            ..
        }) => parse_while(tokens),
        // for statement
        Some(Token {
            token_type: TokenType::For,
            ..
        }) => parse_for(tokens),
        // expression statement
        Some(_) => parse_expr(tokens),
        None => panic!("parser::statement called on empty slice of tokens"),
    }
}

fn expression(tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    log::debug!("expression");
    assignment(tokens)
}

fn assignment(tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    let (expr, tokens) = or(tokens)?; // left-hand side

    match peek(TokenType::Equal, tokens) {
        Some(PeekResult::Match(tokens)) => {
            // then a '='
            match expr {
                Expr::Variable(name) => {
                    // left hand side must be a variable (TODO: object field)
                    let (right, tokens) = assignment(tokens)?;
                    Ok((Expr::Assign(name, Rc::new(right)), tokens))
                }
                _ => Err((
                    ParseError {
                        token: tokens.first(),
                        message: format!("{:?} invalid assignment target", expr),
                    },
                    tokens,
                )),
            }
        }
        _ => return Ok((expr, tokens)),
    }
}

macro_rules! parse_logical_expr {
    ($tokens:ident, $cur_fn:expr, $precedent_fn:ident, $token_type:path) => {{
        let (mut expr, mut remaining) = $precedent_fn($tokens)?;

        loop {
            match remaining.first() {
                Some(
                    token @ Token {
                        token_type: $token_type,
                        ..
                    },
                ) => {
                    let (_, rest) = remaining.split_first().unwrap();
                    if let Ok(temp) = $precedent_fn(rest) {
                        remaining = temp.1;
                        expr =
                            Expr::Logical(Rc::new(expr), Rc::new(token.clone()), Rc::new(temp.0));
                    } else {
                        return Err((
                            ParseError {
                                token: Some(token),
                                message: format!("could not parse right hand side of {}", $cur_fn),
                            },
                            remaining,
                        ));
                    }
                }
                _ => {
                    return Ok((expr, remaining));
                }
            }
        }
    }};
}

fn or(tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    // match LOGIC_AND (or LOGIC_AND)*
    parse_logical_expr!(tokens, "or", and, TokenType::Or)
}

fn and(tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    // match EQUALITY (and EQUALITY)*
    parse_logical_expr!(tokens, "and", equality, TokenType::And)
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
                            expr = Expr::Binary(Rc::new(expr), Rc::new(token.clone()), Rc::new(temp.0));
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

    parse_binary_expr!(
        tokens,
        "equality",
        comparison,
        TokenType::BangEqual,
        TokenType::EqualEqual
    )
}

fn comparison(tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    // match TERM (< | <= | > | >= TERM)*

    parse_binary_expr!(
        tokens,
        "comparison",
        term,
        TokenType::Less,
        TokenType::LessEqual,
        TokenType::Greater,
        TokenType::GreaterEqual
    )
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
        Some(
            token @ Token {
                token_type: TokenType::Bang,
                ..
            },
        )
        | Some(
            token @ Token {
                token_type: TokenType::Minus,
                ..
            },
        ) => {
            if let Some((_, rest)) = tokens.split_first() {
                let (right, remaining) = unary(rest)?;
                return Ok((
                    Expr::Unary(Rc::new(token.clone()), Rc::new(right)),
                    remaining,
                ));
            } else {
                return Err((
                    ParseError {
                        token: Some(token),
                        message: format!("found unary with no right-hand expression"),
                    },
                    tokens,
                ));
            }
        }
        _ => {
            return call(tokens);
        }
    }
}

fn call(tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    // match PRIMARY ( "(" arguments? ")" )*

    let (expr, tokens) = primary(tokens)?;
    loop {
        if match_(TokenType::LeftParen, tokens) {
            return finish_call(expr, tokens);
        } else {
            break;
        }
    }
    Ok((expr, tokens))
}

fn finish_call(expr: Expr, tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    let mut tokens = consume(TokenType::LeftParen, tokens)?;
    let mut args = vec![];
    while !match_(TokenType::RightParen, tokens) {
        let tuple = expression(tokens)?;
        tokens = tuple.1;
        args.push(tuple.0);
        tokens = maybe_consume(TokenType::Comma, tokens);
    }
    let (right_paren, tokens) = next(TokenType::RightParen, tokens)?;
    if args.len() > 255 {
        Err((
            ParseError {
                token: Some(right_paren),
                message: format!("Can't have more than 255 arguments"),
            },
            tokens,
        ))
    } else {
        Ok((
            Expr::Call(Rc::new(expr), Rc::new(right_paren.clone()), args),
            tokens,
        ))
    }
}

fn primary(tokens: &[Token]) -> Result<(Expr, &[Token]), (ParseError, &[Token])> {
    // match NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"

    let (token, rest) = tokens.split_first().ok_or_else(|| {
        (
            ParseError {
                token: None,
                message: format!("Reached the end of file while parsing"),
            },
            tokens,
        )
    })?;
    match &token.token_type {
        TokenType::False => Ok((Expr::Literal(Value::Boolean(false)), rest)),
        TokenType::True => Ok((Expr::Literal(Value::Boolean(true)), rest)),
        TokenType::Nil => Ok((Expr::Literal(Value::Nil), rest)),
        TokenType::Number(n) => Ok((Expr::Literal(Value::Number(*n)), rest)),
        TokenType::String(s) => Ok((Expr::Literal(Value::String((**s).into())), rest)),
        TokenType::Identifier => Ok((Expr::Variable(Rc::new(token.clone())), rest)),
        TokenType::LeftParen => {
            let (expr, remaining) = expression(rest)?;
            if let Some((next, remaining_trimmed)) = remaining.split_first() {
                if matches!(next.token_type, TokenType::RightParen) {
                    Ok((Expr::Grouping(Rc::new(expr)), remaining_trimmed))
                } else {
                    Err((
                        ParseError {
                            token: Some(next),
                            message: format!("Expected ')', found {:?}", next),
                        },
                        remaining,
                    ))
                }
            } else {
                Err((
                    ParseError {
                        token: Some(token),
                        message: format!("No closing ')' found"),
                    },
                    remaining,
                ))
            }
        }
        _ => Err((
            ParseError {
                token: Some(token),
                message: format!("Invalid token {:?}", token),
            },
            tokens,
        )),
    }
}
