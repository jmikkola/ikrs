use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io;

mod parser;

use parser::tokenize::tokenize;
use parser::parser::parse;

fn main() -> io::Result<()> {
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
        let syntax = parse(&tokens);
        if syntax.has_errors() {
            error = true;
            for e in syntax.errors.iter() {
                println!("{}", e);
            }
            continue;
        }
    }

    if error {
        std::process::exit(1);
    }
    Ok(())
}
