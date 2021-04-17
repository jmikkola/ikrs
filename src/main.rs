use std::io;
use std::path::Path;

extern crate argparse;

extern crate walkdir;
use walkdir::WalkDir;

mod compiler;
mod parser;

fn main() -> io::Result<()> {
    let args = Args::parse();
    let paths = expand_paths(args.paths);

    let result = compiler::compile(paths, args.tokenize_only);
    if let Err(error) = result {
        eprintln!("{}", error);
        std::process::exit(1);
    }

    Ok(())
}

fn expand_paths(roots: Vec<String>) -> Vec<String> {
    // TODO: Try to ensure that the caller doesn't pass more than one directory,
    // and only passes .ik files if no directory is passed.
    let mut file_paths = Vec::new();

    for root in roots {
        let path = Path::new(root.as_str());
        if path.is_dir() {
            let paths_under_dir = find_ik_files_in_directory(&root);
            if paths_under_dir.is_empty() {
                eprintln!("no files found under {}", root);
                std::process::exit(1);
            }
            file_paths.extend(paths_under_dir);
        } else {
            file_paths.push(root);
        }
    }

    file_paths
}

// when passed a project directory, walk that directory tree to find the .ik
// files in there.
fn find_ik_files_in_directory(root: &String) -> Vec<String> {
    let mut paths: Vec<String> = Vec::new();
    for entry in WalkDir::new(root) {
        match entry {
            Ok(ent) => {
                let path = ent.path();
                if !path.is_file() {
                    continue;
                }
                let extension = path.extension().unwrap();
                if extension == "ik" {
                    paths.push(path.to_str().unwrap().to_owned());
                }
            },
            Err(e) => {
                eprintln!("error: {}", e);
            },
        }
    }
    paths
}

struct Args {
    paths: Vec<String>,
    tokenize_only: bool,
}

impl Args {
    fn new() -> Self {
        Args{
            paths: Vec::new(),
            tokenize_only: false,
        }
    }

    // Parse CLI arguments. This may exit.
    fn parse() -> Self {
        use argparse::{ArgumentParser, Collect, StoreTrue};

        let mut args = Args::new();

        {
            let mut ap = ArgumentParser::new();
            ap.set_description("ikrs compiler");
            ap.refer(&mut args.tokenize_only).add_option(
                &["--tokenize-only"],
                StoreTrue,
                "Only tokenize");
            ap.refer(&mut args.paths).add_argument(
                "paths",
                Collect,
                "path to a directory containing a project or a single .ik file");
            ap.parse_args_or_exit();
        }

        if args.paths.is_empty() {
            eprintln!("pass at least one path");
            std::process::exit(1);
        }

        args
    }
}
