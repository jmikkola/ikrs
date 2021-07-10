use std::path::Path;

extern crate anyhow;
extern crate argparse;
extern crate walkdir;
use walkdir::WalkDir;

mod args;
mod compiler;
// mod inference;
mod parser;
mod util;

fn main() {
    let args = args::Args::parse();
    let (paths, base_path) = expand_paths(args.path.as_ref());

    let result = compiler::compile(paths, &base_path, &args);
    if let Err(error) = result {
        eprintln!("{}", error);
        std::process::exit(1);
    }
}

/// Given a single file or a directory, this will return a list of all the files in the project and
/// the base directory of the project.
fn expand_paths(root: &str) -> (Vec<String>, String) {
    let mut file_paths = Vec::new();

    let path = Path::new(root);
    if path.is_dir() {
        let paths_under_dir = find_ik_files_in_directory(root);
        if paths_under_dir.is_empty() {
            eprintln!("no files found under {}", root);
            std::process::exit(1);
        }
        file_paths.extend(paths_under_dir);
        (file_paths, root.to_string())
    } else {
        let base_dir = path.parent().unwrap().to_str().unwrap().to_string();
        file_paths.push(root.to_string());
        (file_paths, base_dir)
    }
}

/// when passed a project directory, walk that directory tree to find the .ik
/// files in there.
fn find_ik_files_in_directory(root: &str) -> Vec<String> {
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
            }
            Err(e) => {
                eprintln!("error: {}", e);
            }
        }
    }
    paths
}
