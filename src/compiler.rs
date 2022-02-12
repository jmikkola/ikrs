use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path;
use std::collections::HashMap;

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

    let project_files = GroupedFiles::new(&paths, base_path);
    if args.tokenize_only {
	return project_files.tokenize_files();
    }

    let parsed_packages = project_files.parse_files()?;
    if args.parse_only {
        return Ok(());
    }

    let ordered_packages = parsed_packages.check_import_order()?;
    let _checked_packages = ordered_packages.check_modules()?;
    // Now something like let typed_packages = checked_packages.infer_types()?;
    // lower
    // codegen

    Ok(())
}

/// This holds the file names in the project, grouped by which package they belong to.
/// This also provider an API to start parsing those files.
struct GroupedFiles {
    packages: Vec<PackageFiles>,
}
/// This holds one package and lists the files it contains
struct PackageFiles {
    package_name: String,
    file_paths: Vec<String>,
}

impl GroupedFiles {
    fn new(paths: &[String], base_path: &str) -> Self {
	let mut grouped = GroupedFiles{packages: vec![]};

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

        let mut pkg = PackageFiles::new(package_path);
        pkg.add_file_path(filename.to_string());
        self.packages.push(pkg);
    }

    // Tokenize files without parsing them.
    // This is only used when the right CLI argument is passed.
    fn tokenize_files(self) -> Result<()> {
	let is_err: bool = self.packages.iter()
	    .map(|p| p.tokenize().is_err())
	    .any(|err| err);
	if is_err {
	    bail!("tokenizer error");
	}
	Ok(())
    }

    // Take the next step in compiling: parse the files.
    fn parse_files(self) -> Result<Parsed> {
	let package_results: Vec<Result<ParsedPackage>> = self.packages.into_iter()
	    .map(|p| p.parse())
	    .collect();
	// Split out error handling so that it doesn't short-circuit parsing
	// at the first package with an error
	let packages = package_results.into_iter()
	    .collect::<Result<Vec<ParsedPackage>>>()?;
	Ok(Parsed{packages})
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


impl PackageFiles {
    fn new(package_name: String) -> Self {
	let file_paths = vec![];
	PackageFiles {package_name, file_paths}
    }

    fn add_file_path(&mut self, filename: String) {
        self.file_paths.push(filename);
    }

    // Tokenize files without parsing them
    fn tokenize(&self) -> Result<()> {
	let has_err = self.file_paths.iter()
	    .map(|file_path| {
		let contents = read_path(file_path)?;
		tokenize_with_errors(file_path, contents.as_str())?;
		Ok(())
	    })
	    .any(|result: Result<()>| result.is_err());
	if has_err {
	    bail!("tokenize error");
	}
	Ok(())
    }

    fn parse(self) -> Result<ParsedPackage> {
	let parsed_files: Vec<Result<ParsedFile>> = self.file_paths.into_iter()
	    .map(|name| {
		let syntax = parse_file_path(name.as_str())?;
		Ok(ParsedFile{syntax})
	    })
	    .collect();
	// Split out error handling so that it doesn't short-circuit parsing
	// at the first file with an error
	let files = parsed_files.into_iter()
	    .collect::<Result<Vec<ParsedFile>>>()?;
	Ok(ParsedPackage{
	    package_name: self.package_name,
	    files,
	})
    }
}


/// tokenizes and parses a file, printing any errors
fn parse_file_path(file_path: &str) -> Result<ast::Syntax> {
    let contents = read_path(file_path)?;
    let tokens = tokenize_with_errors(file_path, contents.as_str())?;
    parse_with_errors(file_path, contents.as_str(), &tokens)
}

/// tokenize `contents` and print any errors
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

/// parse `tokens` and print any errors
fn parse_with_errors(file_path: &str, file: &str, tokens: &Tokens) -> Result<ast::Syntax> {
    let syntax = parse(file_path, &tokens);
    if syntax.has_errors() {
	eprintln!("Error parsing {}, syntax error", file_path);
	eprintln!("{}", syntax.render_errors(file));
	bail!("parse error");
    }

    Ok(syntax)
}

/// Parsed holds the results of parsing packages.
///
/// It provides an API to then do a topological sort of the packages by import order. For example,
/// if package A imports package B, then B comes first in the ordering. That way, after processing
/// B, the compiler has the information necessary to process A.
struct Parsed {
    packages: Vec<ParsedPackage>,
}

struct ParsedPackage  {
    package_name: String,
    files: Vec<ParsedFile>,
}

struct ParsedFile {
    // name: String,
    syntax: ast::Syntax,
}


impl Parsed {
    fn check_import_order(self) -> Result<Ordered> {
        let graph = self.build_import_graph();

        if let Some(ordering) = graph.get_topo_ordering() {
	    let package_groups = group_packages_by_ordering(ordering, self.packages);
	    Ok(Ordered{package_groups})
        } else {
            let cycles = graph.find_cycles();
            eprintln!("found cycles in the import graph");
            for cycle in cycles.iter() {
                eprintln!("  cycle: {:?}", cycle);
            }
            Err(anyhow!("found cycles in import graph"))
        }
    }


    fn build_import_graph(&self) -> Graph<String> {
        let mut graph = Graph::new();
        for pkg in self.packages.iter() {
            graph.add_node(pkg.package_name.clone());
            pkg.add_import_edges(&mut graph);
        }
        graph
    }
}

fn group_packages_by_ordering(
    ordering: Vec<Vec<String>>,
    packages: Vec<ParsedPackage>
) -> Vec<Vec<ParsedPackage>> {
    // Create empty Vec<>s for each group in the ordering
    let mut groups: Vec<Vec<ParsedPackage>> = ordering
	.iter().map(|_| Vec::new()).collect();
    // Figure out which group each package name belongs to
    let mut name_to_group = HashMap::new();
    for (n, group) in ordering.into_iter().enumerate() {
	for name in group {
	    name_to_group.insert(name, n);
	}
    }
    // Assign each package to its group
    for package in packages {
	let group_n = name_to_group.get(&package.package_name).unwrap();
	groups[*group_n].push(package);
    }

    groups
}

impl ParsedPackage {
    fn add_import_edges(&self, graph: &mut Graph<String>) {
        for file in self.files.iter() {
	    file.add_import_edges(self.package_name.as_str(), graph);
        }
    }

    fn first_pass(&self) -> Result<()> {
	// TODO: Also check whole-module properties, e.g. that two files don't declare the same
	// name, or that one file imports a name that the other declares.
	// First pass should also resolve imported names to their final (fully-qualified?) name, and
	// return the updated structures. The type inference pass should be able to pick out the
	// already-resolved types for the imported names.
	for file in self.files.iter() {
	    first_pass::check(&file.syntax)?;
	}
	Ok(())
    }

    fn check_declared_names(&self) -> Result<()> {
        let allowed_to_skip_package_decl = self.package_name == "main";

        let expected_name = self.package_name.split('.').last().unwrap();

	let mut declared_names = HashMap::new();
	let mut result = Ok(());

        for file in self.files.iter() {
	    let syntax = &file.syntax;
	    let filename = &syntax.filename;
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

	    for declaration in syntax.declarations.iter() {
		use ast::Declaration::*;

		let declared_name = match declaration {
		    PackageDecl(_) => None,
		    ImportDecl(name_parts) => {
			// TODO: It's OK for both files to import the same name. It just can't be
			// imported twice in one file, nor overlap with the name of something
			// declared in any file in the module.
			let imported_name = name_parts.last().unwrap().clone();
			Some(imported_name)
		    },
		    TypeDecl(defined_type, _definition) => {
			let t = syntax.get_type(*defined_type);

			use ast::Type::*;

			let declared_name = match t {
			    TypeName(name) => name.clone(),
			    Generic(name, _) => name.clone(),
			    _ => panic!("Compiler error: invalid type for LHS of type declaration {:?}", t),
			};
			Some(declared_name)
		    },
		    FunctionDecl(func_decl) => {
			Some(func_decl.name.clone())
		    },
		    InstanceDecl(_inst_decl) => {
			None // TODO deal with duplicate instance declarations later
		    },
		    DeclarationParseError => {
			panic!("Compiler error: Saw a DeclarationParseError");
		    },
		};

		if let Some(name) = declared_name {
		    if let Some(file) = declared_names.get(&name) {
			eprintln!("duplicate declaration of {} in {} and {}", name, file, filename);
			result = Err(anyhow!("duplicate declaration of {}", name));
		    } else {
			declared_names.insert(name, filename.clone());
		    }
		}
	    }
        }

        result
    }
}

impl ParsedFile {
    fn add_import_edges(&self, package_name: &str, graph: &mut Graph<String>) {
        for decl in self.syntax.declarations.iter() {
            if let ast::Declaration::ImportDecl(names) = decl {
                let name = names.join(".");
                graph.add_edge(package_name.to_string(), name);
            }
        }
    }
}


struct Ordered {
    package_groups: Vec<Vec<ParsedPackage>>,
}

impl Ordered {
    fn check_modules(self) -> Result<Checked> {
	for group in self.package_groups.iter() {
	    // In theory, the packages within a group could be processed in parallel since they have
	    // no interdependencies
	    for package in group.iter() {
		// TODO: Should this be part of first pass?
		package.check_declared_names()?;
		package.first_pass()?;
	    }
	}
	Ok(Checked{package_groups: self.package_groups})
    }
}

struct Checked {
    #[allow(dead_code)]
    package_groups: Vec<Vec<ParsedPackage>>,
}

impl Checked {
}
