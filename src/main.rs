use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
mod scanner;
pub mod token;

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
    match file.read_to_string(&mut s) {
        Err(why) => panic!("couldn't read {}: {}", display, why),
        Ok(_) => {
            if !run(&s) {
                std::process::exit(65);
            }
        }
    }
}

fn run_prompt() -> () {
    loop {
        let mut input = String::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(0) => {
                break;
            }
            Ok(_) => {
                run(&input);
            }
            Err(error) => println!("error: {}", error),
        }
    }
}

fn run(source: &str) -> bool {
    match scanner::scan_tokens(source) {
        Ok(tokens) => {
            for token in tokens {
                println!("{}", token);
            }
        }
        Err(scanner::ScanError {
            cause,
            line,
            position,
            c,
        }) => {
            match cause {
                scanner::ScanErrorType::BadChar => {
                    error(line, format!("Unexpected character {} at {}", c, position))
                }
            }
            return false;
        }
    }
    return true;
}

pub fn error(line: i32, message: String) -> () {
    report(line, "", message);
}

pub fn report(line: i32, location: &str, message: String) -> () {
    println!("[line {}] Error {}: {}", line, location, message);
}
