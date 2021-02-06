use std::io;
use std::io::prelude::*;
use std::fs::File;

mod parser;

use parser::tokenize::tokenize;

fn main() -> io::Result<()> {
    let f = File::open("examples/odd_even.ik")?;
    let mut reader = io::BufReader::new(f);
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;

    let tokens = tokenize(contents.as_str());
    println!("tokens: {:?}", tokens);
    Ok(())
}
