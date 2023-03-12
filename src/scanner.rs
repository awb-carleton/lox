use crate::token::Token;
use crate::token::TokenType;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::str::Chars;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut map = HashMap::new();
        map.insert("and", TokenType::And);
        map.insert("class", TokenType::Class);
        map.insert("else", TokenType::Else);
        map.insert("false", TokenType::False);
        map.insert("for", TokenType::For);
        map.insert("fun", TokenType::Fun);
        map.insert("if", TokenType::If);
        map.insert("nil", TokenType::Nil);
        map.insert("or", TokenType::Or);
        map.insert("print", TokenType::Print);
        map.insert("return", TokenType::Return);
        map.insert("super", TokenType::Super);
        map.insert("this", TokenType::This);
        map.insert("true", TokenType::True);
        map.insert("var", TokenType::Var);
        map.insert("while", TokenType::While);
        map
    };
}

#[derive(Debug)]
struct ScanState<'a> {
    current: usize,
    line: u32,
    _source: &'a str,
    iter: &'a mut std::iter::Peekable<Chars<'a>>,
    lexeme: String,
}

impl ScanState<'_> {
    fn advance(&mut self) -> Option<char> {
        match self.iter.next() {
            None => None,
            Some(c) => {
                self.current += 1;
                if !c.is_whitespace() {
                    self.lexeme.push(c);
                }
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
    pub line: u32,
    pub position: usize,
}

pub fn scan_tokens(source: &str) -> Result<Vec<Token>, ScanError> {
    let mut state = ScanState {
        current: 0,
        line: 1,
        _source: source,
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
        None => Ok(make_token(TokenType::EOF, state)),
        Some(c) => match c {
            '(' => Ok(make_token(TokenType::LeftParen, state)),
            ')' => Ok(make_token(TokenType::RightParen, state)),
            '{' => Ok(make_token(TokenType::LeftBrace, state)),
            '}' => Ok(make_token(TokenType::RightBrace, state)),
            ',' => Ok(make_token(TokenType::Comma, state)),
            '.' => Ok(make_token(TokenType::Dot, state)),
            '-' => Ok(make_token(TokenType::Minus, state)),
            '+' => Ok(make_token(TokenType::Plus, state)),
            ';' => Ok(make_token(TokenType::Semicolon, state)),
            '*' => Ok(make_token(TokenType::Star, state)),
            '=' => match match_next('=', state) {
                true => Ok(make_token(TokenType::EqualEqual, state)),
                false => Ok(make_token(TokenType::Equal, state)),
            },
            '!' => match match_next('=', state) {
                true => Ok(make_token(TokenType::BangEqual, state)),
                false => Ok(make_token(TokenType::Bang, state)),
            },
            '<' => match match_next('=', state) {
                true => Ok(make_token(TokenType::LessEqual, state)),
                false => Ok(make_token(TokenType::Less, state)),
            },
            '>' => match match_next('=', state) {
                true => Ok(make_token(TokenType::GreaterEqual, state)),
                false => Ok(make_token(TokenType::Greater, state)),
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
                    Ok(make_token(TokenType::EOF, state))
                }
                false => Ok(make_token(TokenType::Slash, state)),
            },
            ' ' | '\r' | '\t' => scan_token(state),
            '\n' => {
                state.line += 1;
                scan_token(state)
            }
            '"' => parse_string(state),
            '0'..='9' => parse_number(c, state),
            '_' | 'A'..='z' => parse_ident(c, state),
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
                return Ok(make_token(TokenType::String(s.into()), state));
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
            },
            None => {
                break;
            }
        }
        state.advance();
    }
    match s.parse::<f64>() {
        Ok(n) => Ok(make_token(TokenType::Number(n), state)),
        Err(e) => Err(ScanError {
            cause: ScanErrorType::NumberParseError(s, e),
            line: state.line,
            position: state.current,
        }),
    }
}

fn parse_ident(c: char, state: &mut ScanState) -> Result<Token, ScanError> {
    let mut s = String::from(c);
    loop {
        match state.peek() {
            Some(i) => match i {
                '_' | 'A'..='z' | '0'..='9' => s.push(i),
                _ => {
                    break;
                }
            },
            None => {
                break;
            }
        }
        state.advance();
    }
    match KEYWORDS.contains_key(s.as_str()) {
        true => match s.as_str() {
            "true" => Ok(make_token(TokenType::True, state)),
            "false" => Ok(make_token(TokenType::True, state)),
            _ => Ok(make_token(KEYWORDS.get(s.as_str()).unwrap().clone(), state)),
        },
        false => Ok(make_token(TokenType::Identifier, state)),
    }
}

fn make_token(token_type: TokenType, state: &ScanState) -> Token {
    let line = state.line;
    let lexeme = state.lexeme.clone();
    Token {
        token_type,
        lexeme,
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
