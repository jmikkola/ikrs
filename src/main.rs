use std::io;
use std::path::Path;

extern crate argparse;
use argparse::{ArgumentParser, Collect};

extern crate walkdir;
use walkdir::WalkDir;

mod compiler;
mod parser;

fn main() -> io::Result<()> {
    let root_paths = parse_args();
    let paths = expand_paths(root_paths);

    let result = compiler::compile(paths);
    if let Err(error) = result {
        eprintln!("{}", error);
        std::process::exit(1);
    }

    Ok(())
}

fn expand_paths(roots: Vec<String>) -> Vec<String> {
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

fn parse_args() -> Vec<String> {
    let mut paths = Vec::new();
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("ikrs compiler");
        ap.refer(&mut paths)
            .add_argument("paths", Collect, "path to a directory containing a project or a single .ik file");
        ap.parse_args_or_exit();
    }

    if paths.is_empty() {
        eprintln!("pass at least one path");
        std::process::exit(1);
    }

    paths
}
