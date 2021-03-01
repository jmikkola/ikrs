use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io;

mod parser;

use parser::tokenize::tokenize;
use parser::parser::parse;

fn main() -> io::Result<()> {
    let tok_only = tokenize_only();
    if tok_only {
        println!("only tokenizing");
    }

    let mut error = false;
    for name in env::args().skip(1) {
        println!("name: {}", name);
        let f = File::open(name)?;
        let mut reader = io::BufReader::new(f);
        let mut contents = String::new();
        reader.read_to_string(&mut contents)?;

        let tokens = tokenize(contents.as_str());
        if tokens.has_unknown() {
            error = true;
            continue;
        }
        // println!("tokens:  {:?}", tokens.just_tokens());
        if !tok_only {
            let syntax = parse(&tokens);
            if syntax.has_errors() {
                error = true;
                for e in syntax.errors.iter() {
                    println!("{}", e);
                }
                continue;
            }
        }
    }

    if error {
        std::process::exit(1);
    }
    Ok(())
}

fn tokenize_only() -> bool {
    for (key, value) in std::env::vars() {
        if key == "TOK_ONLY" {
            return true;
        }
    }

    false
}
