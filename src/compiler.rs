use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path;

use anyhow::{anyhow, bail, Result};

use super::args::Args;
use super::parser::ast;
use super::parser::parser::parse;
use super::parser::tokenize::{tokenize, Tokens};
use super::util::graph::Graph;

// submodules
pub mod first_pass;
#[cfg(test)]
mod test;

pub fn compile(paths: Vec<String>, base_path: &str, args: &Args) -> Result<()> {
    if args.tokenize_only {
        println!("only tokenizing");
    } else if args.parse_only {
        println!("only parsing");
    }

    let mut packages = CompileJob::gather_files(&paths, base_path);
    packages.parse_files(args.tokenize_only)?;

    if args.tokenize_only || args.parse_only {
        return Ok(());
    }

    packages.check_declared_names()?;
    packages.check_import_cycles()?;
    packages.check_modules_in_order()?;
    // lower
    // codegen

    Ok(())
}

#[derive(Debug)]
struct Package {
    package_name: String,
    file_paths: Vec<String>,
    syntaxes: Vec<ast::Syntax>,
}

impl Package {
    fn new(package_name: String) -> Self {
        Package {
            package_name,
            file_paths: Vec::new(),
            syntaxes: Vec::new(),
        }
    }

    fn add_file_path(&mut self, filename: String) {
        self.file_paths.push(filename);
    }

    fn parse_files(&mut self, tokenize_only: bool) -> Result<()> {
        assert!(self.syntaxes.is_empty());
	let mut has_error = false;
        for file_path in self.file_paths.iter() {
	    let result = parse_file(&mut self.syntaxes, tokenize_only, file_path);
	    has_error = has_error || result.is_err();
        }

	if has_error {
	    bail!("parse error");
	}
        Ok(())
    }

    fn check_declared_names(&self) -> Result<()> {
        let allowed_to_skip_package_decl = self.package_name == "main";

        let expected_name = self.package_name.split('.').last().unwrap();

        for syntax in self.syntaxes.iter() {
            let first_decl = if let Some(decl) = syntax.declarations.get(0) {
                decl
            } else {
                // ignore empty files
                continue;
            };

            if let ast::Declaration::PackageDecl(name) = first_decl {
                if *name != expected_name {
                    eprintln!(
                        "file {} should declare package {} but declares package {} instead",
                        syntax.filename, expected_name, name
                    );
		    bail!("wrong package error");
                }
            } else if !allowed_to_skip_package_decl {
                eprintln!(
                    "file {} should declare package {} but doesn't",
                    syntax.filename, expected_name
                );
		bail!("no package error");
            }
        }
        Ok(())
    }

    fn add_import_edges(&self, graph: &mut Graph<String>) {
        for syntax in self.syntaxes.iter() {
            for decl in syntax.declarations.iter() {
                if let ast::Declaration::ImportDecl(names) = decl {
                    let name = names.join(".");
                    graph.add_edge(self.package_name.clone(), name);
                }
            }
        }
    }

    fn first_pass(&self) -> Result<()> {
	// TODO: Also check whole-module properties, e.g. that two files don't declare the same
	// name, or that one file imports a name that the other declares.
	// First pass should also resolve imported names to their final (fully-qualified?) name, and
	// return the updated structures. The type inference pass should be able to pick out the
	// already-resolved types for the imported names.
        for syntax in self.syntaxes.iter() {
            first_pass::check(syntax)?;
        }
        Ok(())
    }
}

fn parse_file(syntaxes: &mut Vec<ast::Syntax>, tokenize_only: bool, file_path: &str) -> Result<()> {
    let contents = read_path(file_path)?;
    let tokens = tokenize_with_errors(file_path, contents.as_str())?;

    if tokenize_only {
	return Ok(());
    }

    let syntax = parse_with_errors(file_path, contents.as_str(), &tokens)?;
    syntaxes.push(syntax);

    Ok(())
}

// tokenize `contents` and print any errors
fn tokenize_with_errors(file_path: &str, contents: &str) -> Result<Tokens> {
    let tokens = tokenize(contents);
    let unknown = tokens.display_unknown();
    if !unknown.is_empty() {
        eprintln!("Error parsing {}, found unknown tokens", file_path);

	for selection in unknown.iter() {
	    eprintln!("\n{}", selection.render_selection(&contents));
	}

	tokens.get_error()?;
    }

    Ok(tokens)
}

// parse `tokens` and print any errors
fn parse_with_errors(file_path: &str, file: &str, tokens: &Tokens) -> Result<ast::Syntax> {
    let syntax = parse(file_path, &tokens);
    if syntax.has_errors() {
	eprintln!("Error parsing {}, syntax error", file_path);
	eprintln!("{}", syntax.render_errors(file));
        // for e in syntax.errors.iter() {
        //     eprintln!(".... {}", e);
        // }
	bail!("parse error");
    }

    Ok(syntax)
}

#[derive(Debug)]
struct CompileJob {
    packages: Vec<Package>,
    // This contains a topological sort of packages based on imports, so that the compiler can wait
    // to typecheck a module until all the models it depends on have been typechecked.
    package_ordering: Vec<Vec<String>>,
}

impl CompileJob {
    fn new() -> Self {
        CompileJob {
            packages: vec![],
            package_ordering: vec![],
        }
    }

    fn gather_files(paths: &[String], base_path: &str) -> Self {
        let mut grouped = Self::new();

        for filename in paths.iter() {
            let package_path = get_package_path(filename, base_path);
            grouped.add_file_to_package(package_path, filename);
        }

	grouped
    }

    fn add_file_to_package(&mut self, package_path: String, filename: &str) {
        for pck in self.packages.iter_mut() {
            if pck.package_name == package_path {
                pck.file_paths.push(filename.to_string());
                return;
            }
        }

        let mut pkg = Package::new(package_path);
        pkg.add_file_path(filename.to_string());
        self.packages.push(pkg);
    }

    fn parse_files(&mut self, tokenize_only: bool) -> Result<()> {
        for pkg in self.packages.iter_mut() {
            pkg.parse_files(tokenize_only)?;
        }
        Ok(())
    }

    fn check_declared_names(&self) -> Result<()> {
        for pkg in self.packages.iter() {
            pkg.check_declared_names()?;
        }
        Ok(())
    }

    fn check_import_cycles(&mut self) -> Result<()> {
        let graph = self.build_import_graph();

        if let Some(ordering) = graph.get_topo_ordering() {
            self.package_ordering = ordering;
        } else {
            let cycles = graph.find_cycles();
            eprintln!("found cycles in the import graph");
            for cycle in cycles.iter() {
                eprintln!("  cycle: {:?}", cycle);
            }
            return Err(anyhow!("found cycles in import graph"));
        }

        Ok(())
    }

    fn build_import_graph(&self) -> Graph<String> {
        let mut graph = Graph::new();
        for pkg in self.packages.iter() {
            graph.add_node(pkg.package_name.clone());
            pkg.add_import_edges(&mut graph);
        }
        graph
    }

    fn get_package_by_name(&self, name: &str) -> Option<&Package> {
        for pkg in self.packages.iter() {
            if pkg.package_name == name {
                return Some(pkg);
            }
        }
        eprintln!("cannot find package named {}", name);
        None
    }

    fn check_modules_in_order(&self) -> Result<()> {
        for group in self.package_ordering.iter() {
            // You could in theory check the items in `group` in parallel
            for module_name in group.iter() {
                let package = self.get_package_by_name(module_name).unwrap();
                package.first_pass()?;
                // TODO: Also infer types
            }
        }
        Ok(())
    }
}

fn get_package_path(path: &str, base_path: &str) -> String {
    let mut parts: Vec<String> = path::Path::new(path)
        .strip_prefix(base_path)
        .unwrap()
        .components()
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

fn read_path(path: &str) -> Result<String> {
    let f = File::open(path)?;
    let mut reader = io::BufReader::new(f);
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;
    Ok(contents)
}
