use crate::token::Literal;
use crate::token::Token;
use crate::token::TokenType;
use std::str::Chars;

#[derive(Debug)]
struct ScanState<'a> {
    current: usize,
    line: i32,
    source: &'a str,
    iter: &'a mut std::iter::Peekable<Chars<'a>>,
    lexeme: String,
}

impl<'a> ScanState<'a> {
    fn advance(&mut self) -> Option<char> {
        match self.iter.next() {
            None => None,
            Some(c) => {
                self.current += 1;
                self.lexeme.push(c);
                Some(c)
            }
        }
    }

    fn peek(&mut self) -> Option<char> {
        match self.iter.peek() {
            None => None,
            Some(c) => Some(*c),
        }
    }

    fn new_lexeme(&mut self) -> () {
        self.lexeme.clear();
    }
}

#[derive(Debug)]
pub enum ScanErrorType {
    BadChar(char),
    UnterminatedString(String),
    NumberParseError(String, std::num::ParseFloatError),
}

#[derive(Debug)]
pub struct ScanError {
    pub cause: ScanErrorType,
    pub line: i32,
    pub position: usize,
}

pub fn scan_tokens(source: &str) -> Result<Vec<Token>, ScanError> {
    let mut state = ScanState {
        current: 0,
        line: 1,
        source: source,
        iter: &mut source.chars().peekable(),
        lexeme: String::new(),
    };
    let mut tokens: Vec<Token> = vec![];

    while state.peek() != None {
        match scan_token(&mut state) {
            Ok(token) => tokens.push(token),
            Err(e) => {
                return Err(e);
            }
        }
        state.new_lexeme();
    }
    return Ok(tokens);
}

fn scan_token(state: &mut ScanState) -> Result<Token, ScanError> {
    // println!("scanning {}", state.source.get(state.current..).unwrap());
    match state.advance() {
        None => Ok(make_token(TokenType::EOF, None, state)),
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
            '=' => match match_next('=', state) {
                true => Ok(make_token(TokenType::EqualEqual, None, state)),
                false => Ok(make_token(TokenType::Equal, None, state)),
            },
            '!' => match match_next('=', state) {
                true => Ok(make_token(TokenType::BangEqual, None, state)),
                false => Ok(make_token(TokenType::Bang, None, state)),
            },
            '<' => match match_next('=', state) {
                true => Ok(make_token(TokenType::LessEqual, None, state)),
                false => Ok(make_token(TokenType::Less, None, state)),
            },
            '>' => match match_next('=', state) {
                true => Ok(make_token(TokenType::GreaterEqual, None, state)),
                false => Ok(make_token(TokenType::Greater, None, state)),
            },
            '/' => match match_next('/', state) {
                true => {
                    while let Some(i) = state.peek() {
                        match i {
                            '\n' => {
                                state.advance();
                                return scan_token(state);
                            }
                            _ => {
                                state.advance();
                            }
                        }
                    }
                    Ok(make_token(TokenType::EOF, None, state))
                }
                false => Ok(make_token(TokenType::Slash, None, state)),
            },
            ' ' | '\r' | '\t' => scan_token(state),
            '\n' => {
                state.line += 1;
                scan_token(state)
            }
            '"' => parse_string(state),
            '0'..='9' => parse_number(c, state),
            'a'..='Z' | '_' => parse_ident(c, state),
            _ => Err(ScanError {
                cause: ScanErrorType::BadChar(c),
                line: state.line,
                position: state.current,
            }),
        },
    }
}

fn parse_string(state: &mut ScanState) -> Result<Token, ScanError> {
    let mut s = String::new();
    while let Some(i) = state.peek() {
        match i {
            '"' => {
                state.advance();
                return Ok(make_token(
                    TokenType::String,
                    Some(Literal::String(s)),
                    state,
                ));
            }
            '\n' => {
                state.line += 1;
                s.push('\n');
            }
            _ => {
                s.push(i);
            }
        }
        state.advance();
    }
    Err(ScanError {
        cause: ScanErrorType::UnterminatedString(s),
        line: state.line,
        position: state.current,
    })
}

fn parse_number(c: char, state: &mut ScanState) -> Result<Token, ScanError> {
    let mut s = String::from(c);
    loop {
        match state.peek() {
            Some(i) => match i {
                '.' | '0'..='9' => s.push(i),
                _ => {
                    break;
                }
            }
            None => {
                break;
            }
        }
        state.advance();
    }
    match s.parse::<f64>() {
        Ok(n) => Ok(make_token(
            TokenType::Number,
            Some(Literal::Number(n)),
            state,
        )),
        Err(e) => Err(ScanError {
            cause: ScanErrorType::NumberParseError(s, e),
            line: state.line,
            position: state.current,
        }),
    }
}

fn parse_ident(c: char, state: &mut ScanState) -> Result<Token, ScanError> {
    let mut s = String::from(c);
    while let Some(i) = state.peek() {
        match i {
            'a'..='Z' | '0'..='9' | '_' => s.push(i),
            _ => {
                // TODO look up s in statis map of reserved words
                // return the associated token or an identifier token
            }
        }
        state.advance();
    }
}

fn make_token(token_type: TokenType, literal: Option<Literal>, state: &ScanState) -> Token {
    let line = state.line;
    let lexeme = state.lexeme.clone();
    Token {
        token_type,
        lexeme,
        literal,
        line,
    }
}

fn match_next(c: char, state: &mut ScanState) -> bool {
    match state.peek() {
        None => false,
        Some(next) => {
            if next == c {
                state.advance();
                true
            } else {
                false
            }
        }
    }
}
