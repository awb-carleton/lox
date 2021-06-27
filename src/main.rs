use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
mod interpreter;
pub mod parser;
mod scanner;
pub mod token;
pub mod environment;
use crate::interpreter::interpret;
use crate::environment::Environment;

struct LoxError {
    exit_code: i32,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        println!("Usage: jlox [script]");
        std::process::exit(64);
    } else if args.len() == 2 {
        run_file(&args[1]);
    } else {
        run_prompt();
    }
}

fn run_file(path_str: &str) -> () {
    let path = Path::new(path_str);
    let display = path.display();

    let mut file = match File::open(&path) {
        Err(why) => panic!("couldn't open {}: {}", path_str, why),
        Ok(file) => file,
    };

    let mut s = String::new();
    let mut env = Environment::new();
    match file.read_to_string(&mut s) {
        Err(why) => panic!("couldn't read {}: {}", display, why),
        Ok(_) => match run(&s, &mut env) {
            Ok(_) => (),
            Err(err) => std::process::exit(err.exit_code),
        },
    }
}

fn run_prompt() -> () {
    let mut line = 1;
    loop {
        print!("[{}] ", line);
        match std::io::stdout().flush() {
            Ok(_) => {}
            Err(_) => panic!("flushing stdout resulted in an error, aborting"),
        }
        let mut input = String::new();
        let mut env = Environment::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(0) => {
                break;
            }
            Ok(_) => match run(&input, &mut env) {
                Ok(_) | Err(_) => line += 1,
            },
            Err(error) => println!("error: {}", error),
        }
    }
}

fn run(source: &str, env: &mut Environment) -> Result<(), LoxError> {
    match scanner::scan_tokens(source) {
        Ok(tokens) => {
            // for token in &tokens[..] {
            //     println!("{:?}", token);
            // }
            match parser::parse(&tokens[..]) {
                Ok(stmts) => match interpret(&stmts, env) {
                    Ok(_) => {}
                    Err(errs) => {
                        for interpreter::RuntimeError {
                            expr,
                            line,
                            message,
                        } in errs
                        {
                            println!("{} [line {}]: {}", message, line, expr);
                        }
                        return Err(LoxError { exit_code: 70 });
                    }
                },
                Err(parser::ParseError { token, message }) => {
                    match token {
                        Some(token) => error(
                            token.line,
                            format!("parser error on {:?}: {}", token, message),
                        ),
                        None => error(0, format!("parser error on {:?}: {}", token, message)),
                    }
                    return Err(LoxError { exit_code: 65 });
                }
            }
        }
        Err(scanner::ScanError {
            cause,
            line,
            position,
        }) => {
            match cause {
                scanner::ScanErrorType::BadChar(c) => {
                    error(line, format!("Unexpected character {} at {}", c, position))
                }
                scanner::ScanErrorType::UnterminatedString(s) => {
                    error(line, format!("Unterminated string {} at {}", s, position))
                }
                scanner::ScanErrorType::NumberParseError(s, e) => error(
                    line,
                    format!("Could not parse {} as a number at {} ({})", s, position, e),
                ),
            }
            return Err(LoxError { exit_code: 65 });
        }
    }
    return Ok(());
}

pub fn error(line: u32, message: String) -> () {
    report(line, "", message);
}

pub fn report(line: u32, location: &str, message: String) -> () {
    println!("[line {}] Error {}: {}", line, location, message);
}
