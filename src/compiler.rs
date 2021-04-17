use std::fs::File;
use std::io::prelude::*;
use std::io;

use super::parser::tokenize::tokenize;
use super::parser::parser::parse;

// submodules
pub mod first_pass;

// TODO: Create a set of files to work with, and topologically sort them.
// Parse them and processes the AST in that order.

// Ideally, this would be able to write out the optimized AST (or even
// assembly?) so that it could compile large programs without keeping everything
// in memory, but there won't be any point for a long time.

pub fn compile(paths: Vec<String>, tokenize_only: bool) -> io::Result<()> {
    if tokenize_only {
        println!("only tokenizing");
    }

    let mut error = false;
    for path in paths.iter() {
        let contents = read_path(path)?;

        let tokens = tokenize(contents.as_str());
        if tokens.has_unknown() {
            error = true;
            continue;
        }

        if tokenize_only {
            continue;
        }

        let syntax = parse(&tokens);
        if syntax.has_errors() {
            error = true;
            for e in syntax.errors.iter() {
                println!("{}", e);
            }
            continue;
        }

        first_pass::check(&syntax)?;
    }

    if error {
        let err = io::Error::new(io::ErrorKind::Other, "error");
        return Err(err);
    }

    Ok(())
}

fn read_path(path: &String) -> io::Result<String> {
    let f = File::open(path)?;
    let mut reader = io::BufReader::new(f);
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;
    Ok(contents)
}
