use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path;

use anyhow::{anyhow, Result};

use super::args::Args;
use super::parser::ast;
use super::parser::parser::parse;
use super::parser::tokenize::tokenize;
use super::util::graph::Graph;

// submodules
pub mod first_pass;
#[cfg(test)]
mod test;

pub fn compile(paths: Vec<String>, base_path: &String, args: &Args) -> Result<()> {
    if args.tokenize_only {
        println!("only tokenizing");
    } else if args.parse_only {
        println!("only parsing");
    }

    let mut packages = CompileJob::gather_files(&paths, base_path)?;
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

    fn parse_files(&mut self, tokenize_only: bool) -> anyhow::Result<()> {
        assert!(self.syntaxes.is_empty());
        for file_path in self.file_paths.iter() {
            let contents = read_path(file_path)?;

            let tokens = tokenize(contents.as_str());
	    let unknown = tokens.display_unknown();
	    if !unknown.is_empty() {
                eprintln!("cannot parse {}, found unknown tokens", file_path);

		for selection in unknown.iter() {
		    eprintln!("\n{}", selection.render_selection(&contents));
		}

                // TODO: Handle errors in one file and continue to the next
		tokens.get_error()?;
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
                return Err(anyhow!("error"));
            }

            self.syntaxes.push(syntax);
        }
        Ok(())
    }

    fn check_declared_names(&self) -> io::Result<()> {
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
                    let err = io::Error::new(io::ErrorKind::Other, "error");
                    return Err(err);
                }
            } else if !allowed_to_skip_package_decl {
                eprintln!(
                    "file {} should declare package {} but doesn't",
                    syntax.filename, expected_name
                );
                let err = io::Error::new(io::ErrorKind::Other, "error");
                return Err(err);
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

    fn first_pass(&self) -> io::Result<()> {
        for syntax in self.syntaxes.iter() {
            first_pass::check(syntax)?;
        }
        Ok(())
    }
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

    fn gather_files(paths: &Vec<String>, base_path: &String) -> io::Result<Self> {
        let mut grouped = Self::new();

        for filename in paths.iter() {
            let package_path = get_package_path(filename, base_path);
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

    fn parse_files(&mut self, tokenize_only: bool) -> Result<()> {
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
        let graph = self.build_import_graph();

        if let Some(ordering) = graph.get_topo_ordering() {
            self.package_ordering = ordering;
        } else {
            let cycles = graph.find_cycles();
            eprintln!("found cycles in the import graph");
            for cycle in cycles.iter() {
                eprintln!("  cycle: {:?}", cycle);
            }
            let err = io::Error::new(io::ErrorKind::Other, "error");
            return Err(err);
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

    fn get_package_by_name(&self, name: &String) -> Option<&Package> {
        for pkg in self.packages.iter() {
            if &pkg.package_name == name {
                return Some(pkg);
            }
        }
        eprintln!("cannot find package named {}", name);
        None
    }

    fn check_modules_in_order(&self) -> io::Result<()> {
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

fn get_package_path(path: &String, base_path: &String) -> String {
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

fn read_path(path: &String) -> io::Result<String> {
    let f = File::open(path)?;
    let mut reader = io::BufReader::new(f);
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;
    Ok(contents)
}
