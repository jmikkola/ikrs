use std::env;
use std::io;

mod compiler;
mod parser;

fn main() -> io::Result<()> {
    let paths = env::args().skip(1).map(|s| s.to_string()).collect();
    let result = compiler::compile(paths);
    if let Err(_error) = result {
        std::process::exit(1);
    }

    Ok(())
}
