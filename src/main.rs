use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io;

mod parser;

use parser::tokenize::tokenize;
use parser::tokens::Token;
use parser::location::Location;

fn main() -> io::Result<()> {
    let mut error = false;
    for name in env::args().skip(1) {
        println!("name: {}", name);
        let f = File::open(name)?;
        let mut reader = io::BufReader::new(f);
        let mut contents = String::new();
        reader.read_to_string(&mut contents)?;

        let tokens = tokenize(contents.as_str());
        if has_unknown(&tokens) {
            error = true;
        }
    }

    if error {
        std::process::exit(1);
    }
    Ok(())
}

fn has_unknown(tokens: &Vec<(Token, Location)>) -> bool {
    for (t, _) in tokens {
        if let Token::Unknown(_) = t {
            println!("unknown token: {}", t);
            return true;
        }
    }
    false
}
