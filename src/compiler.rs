use std::fs::File;
use std::io::prelude::*;
use std::io;
use std::path;

use super::parser::tokenize::tokenize;
use super::parser::parser::parse;

// submodules
pub mod first_pass;
#[cfg(test)]
mod test;


// TODO: Create a set of files to work with, and topologically sort them.
// Parse them and processes the AST in that order.

// Ideally, this would be able to write out the optimized AST (or even
// assembly?) so that it could compile large programs without keeping everything
// in memory, but there won't be any point for a long time.

pub fn compile(paths: Vec<String>, tokenize_only: bool) -> io::Result<()> {
    if tokenize_only {
        println!("only tokenizing");
    }

    let mut packages = CompileJob::gather_files(&paths)?;
    packages.parse_files(tokenize_only)?;

    if tokenize_only {
        return Ok(());
    }

    packages.check_declared_names()?;
    packages.check_import_cycles()?;
    packages.first_pass()?;
    // infer types
    // lower
    // codegen

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

#[derive(Debug)]
struct CompileJob {
    packaged_files: Vec<Package>,
}

#[derive(Debug)]
struct Package {
    package_name: String,
    file_paths: Vec<String>,
}

impl CompileJob {
    fn new() -> Self {
        CompileJob{
            packaged_files: vec![],
        }
    }

    fn gather_files(paths: &Vec<String>) -> io::Result<Self> {
        let mut grouped = Self::new();

        for filename in paths.iter() {
            let package_path = get_package_path(filename);
            grouped.add_file_to_package(package_path, filename);
        }

        Ok(grouped)
    }

    fn add_file_to_package(&mut self, package_path: String, filename: &String) {
        for pck in self.packaged_files.iter_mut() {
            if pck.package_name == package_path {
                pck.file_paths.push(filename.clone());
                return;
            }
        }

        self.packaged_files.push(Package{
            package_name: package_path,
            file_paths: vec![filename.clone()],
        });
    }

    fn parse_files(&mut self, tokenize_only: bool) -> io::Result<()> {
        // TODO
        Ok(())
    }

    fn check_declared_names(&mut self) -> io::Result<()> {
        Ok(())
    }

    fn check_import_cycles(&mut self) -> io::Result<()> {
        Ok(())
    }

    fn first_pass(&mut self) -> io::Result<()> {
        Ok(())
    }
}

fn get_package_path(path: &String) -> String {
    let mut parts: Vec<String> = path::Path::new(path).components()
        .filter_map(|component| {
            if let path::Component::Normal(osstr) = component {
                Some(osstr.to_str().unwrap().to_owned())
            } else {
                None
            }
        })
        .collect();
    // Remove the filename
    parts.pop();
    if parts.is_empty() {
        return "main".to_owned();
    }
    parts.join(".")
}

fn read_path(path: &String) -> io::Result<String> {
    let f = File::open(path)?;
    let mut reader = io::BufReader::new(f);
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;
    Ok(contents)
}
