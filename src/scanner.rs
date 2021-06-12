use crate::token::Literal;
use crate::token::Token;
use crate::token::TokenType;
use std::str::Chars;

#[derive(Debug)]
struct ScanState<'a> {
    current: usize,
    start: usize,
    line: i32,
    source: &'a str,
    iter: &'a mut Chars<'a>,
}

#[derive(Debug)]
pub enum ScanErrorType {
    BadChar,
}

#[derive(Debug)]
pub struct ScanError {
    pub cause: ScanErrorType,
    pub line: i32,
    pub position: usize,
    pub c: char,
}

pub fn scan_tokens(source: &str) -> Result<Vec<Token>, ScanError> {
    let mut state = ScanState {
        current: 0,
        start: 0,
        line: 1,
        source: source,
        iter: &mut source.chars(),
    };
    let mut tokens: Vec<Token> = vec![];

    while state.iter.peekable().peek() != None {
        state.start = state.current;
        match scan_token(&mut state) {
            Ok(token) => tokens.push(token),
            Err(ScanError {
                cause,
                line,
                position,
                c,
            }) => {
                return Err(ScanError {
                    cause,
                    line,
                    position,
                    c,
                })
            }
        }
    }
    let t = Token {
        token_type: TokenType::EOF,
        lexeme: String::new(),
        literal: None,
        line: 0,
    };
    tokens.push(t);
    return Ok(tokens);
}

fn scan_token(state: &mut ScanState) -> Result<Token, ScanError> {
    state.current += 1;
    match state.iter.next() {
        None => {
            panic!("scan_token should only be called on an iterator that has characters remaining")
        }
        Some(c) => match c {
            '(' => Ok(make_token(TokenType::LeftParen, None, state)),
            ')' => Ok(make_token(TokenType::RightParen, None, state)),
            '{' => Ok(make_token(TokenType::LeftBrace, None, state)),
            '}' => Ok(make_token(TokenType::RightBrace, None, state)),
            ',' => Ok(make_token(TokenType::Comma, None, state)),
            '.' => Ok(make_token(TokenType::Dot, None, state)),
            '-' => Ok(make_token(TokenType::Minus, None, state)),
            '+' => Ok(make_token(TokenType::Plus, None, state)),
            ';' => Ok(make_token(TokenType::Semicolon, None, state)),
            '*' => Ok(make_token(TokenType::Star, None, state)),
            _ => Err(ScanError {
                cause: ScanErrorType::BadChar,
                line: state.line,
                position: state.current,
                c,
            }),
        },
    }
}

fn make_token(token_type: TokenType, literal: Option<Literal>, state: &ScanState) -> Token {
    let lexeme = match state.source.get(state.start..state.current) {
        Some(s) => String::from(s),
        None => panic!("Bad make_token({:?}, {:?}", token_type, state),
    };
    let line = state.line;
    Token {
        token_type,
        lexeme,
        literal,
        line,
    }
}

// fn advance(iter: &Chars, state: &ScanState) -> char {
//     state.current += 1;
//     return
// }
