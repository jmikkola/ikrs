use std::fs::File;
use std::io::prelude::*;
use std::io;
use std::path;

use super::parser::tokenize::tokenize;
use super::parser::parser::parse;
use super::parser::ast;

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

        let syntax = parse(path.clone(), &tokens);
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
    packages: Vec<Package>,
}

#[derive(Debug)]
struct Package {
    package_name: String,
    file_paths: Vec<String>,
    syntaxes: Vec<ast::Syntax>,
}

impl Package {
    fn new(package_name: String) -> Self {
        Package{
            package_name: package_name,
            file_paths: Vec::new(),
            syntaxes: Vec::new(),
        }
    }

    fn add_file_path(&mut self, filename: String) {
        self.file_paths.push(filename.clone());
    }

    fn parse_files(&mut self, tokenize_only: bool) -> io::Result<()> {
        assert!(self.syntaxes.is_empty());
        for file_path in self.file_paths.iter() {
            let contents = read_path(file_path)?;

            let tokens = tokenize(contents.as_str());
            if tokens.has_unknown() {
                // TODO: Handle errors in one file and continue to the next
                eprintln!("cannot parse {}, found unknown tokens", file_path);
                let err = io::Error::new(io::ErrorKind::Other, "error");
                return Err(err);
            }

            if tokenize_only {
                continue;
            }

            let syntax = parse(file_path.clone(), &tokens);
            if syntax.has_errors() {
                // TODO: Handle errors in one file and continue to the next
                for e in syntax.errors.iter() {
                    eprintln!("{}", e);
                }
                let err = io::Error::new(io::ErrorKind::Other, "error");
                return Err(err);
            }

            self.syntaxes.push(syntax);
        }
        Ok(())
    }

    fn check_declared_names(&self) -> io::Result<()> {
        let allowed_to_skip_package_decl = self.package_name == "main";

        for syntax in self.syntaxes.iter() {
            let first_decl = if let Some(decl) = syntax.declarations.get(0) {
                decl
            } else {
                // ignore empty files
                continue
            };

            if let ast::Declaration::PackageDecl(name) = first_decl {
                if *name != self.package_name {
                    eprintln!(
                        "file {} should declare package {} but declares package {} instead",
                        syntax.filename, self.package_name, name);
                    let err = io::Error::new(io::ErrorKind::Other, "error");
                    return Err(err);
                }
            } else if !allowed_to_skip_package_decl {
                eprintln!(
                    "file {} should declare package {} but doesn't",
                    syntax.filename, self.package_name);
                let err = io::Error::new(io::ErrorKind::Other, "error");
                return Err(err);
            }
        }
        Ok(())
    }
}

impl CompileJob {
    fn new() -> Self {
        CompileJob{
            packages: vec![],
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
        for pck in self.packages.iter_mut() {
            if pck.package_name == package_path {
                pck.file_paths.push(filename.clone());
                return;
            }
        }

        let mut pkg = Package::new(package_path);
        pkg.add_file_path(filename.clone());
        self.packages.push(pkg);
    }

    fn parse_files(&mut self, tokenize_only: bool) -> io::Result<()> {
        for pkg in self.packages.iter_mut() {
            pkg.parse_files(tokenize_only)?;
        }
        Ok(())
    }

    fn check_declared_names(&self) -> io::Result<()> {
        for pkg in self.packages.iter() {
            pkg.check_declared_names()?;
        }
        Ok(())
    }

    fn check_import_cycles(&mut self) -> io::Result<()> {
        Ok(())
    }

    fn first_pass(&mut self) -> io::Result<()> {
        Ok(())
    }
}

// TODO: Support finding base paths to the project (e.g. strip off the passed in directory)
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
