use std::path::Path;

use super::*;
use crate::args::Args;

use tempdir;

fn get_package<'a>(gp: &'a CompileJob, name: &str) -> Option<&'a Package> {
    for pf in gp.packages.iter() {
        if pf.package_name == name {
            return Some(pf);
        }
    }
    None
}

#[test]
fn test_get_package_path() {
    let cases = vec![
        ("foo.ik", "", "main"),
        ("main.ik", "", "main"),
        ("./main.ik", "", "main"),
        ("foo/main.ik", "", "foo"),
        ("foo/main.ik", "foo/", "main"),
        ("foo/bar/baz/x.ik", "", "foo.bar.baz"),
        ("./foo/bar/baz/x.ik", "", "foo.bar.baz"),
        ("./foo/bar/baz/x.ik", "./foo", "bar.baz"),
        ("./foo/bar/baz/x.ik", "./foo/bar", "baz"),
    ];

    for (input, base, expected) in cases {
        assert!(expected == get_package_path(&input.to_string(), &base.to_string()));
    }
}

#[test]
fn test_grouping_files() {
    let paths: Vec<String> = vec![
        "main.ik",
        "args.ik",
        "util/helpers.ik",
        "app/server/health.ik",
        "app/server/index.ik",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect();

    let result = CompileJob::gather_files(&paths, &"".to_owned());

    assert!(result.packages.len() == 3);

    let main_pkg = get_package(&result, "main").unwrap();
    assert!(main_pkg.file_paths.contains(&"main.ik".to_string()));
    assert!(main_pkg.file_paths.contains(&"args.ik".to_string()));

    let util_pkg = get_package(&result, "util").unwrap();
    assert!(util_pkg.file_paths.contains(&"util/helpers.ik".to_string()));

    let server_pkg = get_package(&result, "app.server").unwrap();
    assert!(server_pkg
        .file_paths
        .contains(&"app/server/health.ik".to_string()));
    assert!(server_pkg
        .file_paths
        .contains(&"app/server/index.ik".to_string()));
}

fn setup_test_dir(
    name_and_contents: Vec<(&str, &str)>,
) -> io::Result<(tempdir::TempDir, String, Vec<String>)> {
    let tmp_dir = tempdir::TempDir::new("ikrs-tests")?;
    let base_dir = path_to_string(tmp_dir.path());

    let mut file_paths = Vec::new();
    for (name, content) in name_and_contents {
        let file_path = tmp_dir.path().join(name);
        ensure_parent_exists(&file_path)?;

        let file_path_str = path_to_string(&file_path);

        let mut file = File::create(file_path)?;
        write!(file, "{}", content)?;

        file_paths.push(file_path_str);
    }

    Ok((tmp_dir, base_dir, file_paths))
}

fn path_to_string(path: &Path) -> String {
    path.to_str().unwrap().to_string()
}

fn ensure_parent_exists(path: &Path) -> io::Result<()> {
    if let Some(parent) = path.parent() {
        ensure_parent_exists(&parent)?;
        if !parent.exists() {
            std::fs::create_dir(parent)?;
        }
    }
    Ok(())
}

#[test]
fn test_building_single_main_file() {
    let main = r#"
fn main():
  print("hello world")
"#;
    let setup = vec![("main.ik", main)];
    let (_tmp_dir, base_path, file_paths) = setup_test_dir(setup).unwrap();

    let result = compile(file_paths, &base_path, &Args::new());
    assert!(result.is_ok(), "{:?}", result);
}

#[test]
fn test_building_multi_module_project() {
    let main = r#"
package main

fn main():
  say_hello()
"#;

    let hello = r#"
import message

fn say_hello():
  print(message.hello_message())
"#;

    let message = r#"
package message

fn hello_message():
  return "Hello"
"#;

    let setup = vec![
        ("main.ik", main),
        ("hello.ik", hello),
        ("message/message.ik", message),
    ];
    let (_tmp_dir, base_path, file_paths) = setup_test_dir(setup).unwrap();

    let result = compile(file_paths, &base_path, &Args::new());
    assert!(result.is_ok(), "{:?}", result);
}

#[test]
fn test_fails_on_import_cycle() {
    let main = r#"
package main

import a

fn main():
  a.f()
"#;

    let a = r#"
package a

import b

fn f():
  b.g()
"#;

    let b = r#"
package b

import a

fn g():
  a.f()
"#;

    let setup = vec![("main.ik", main), ("a/a.ik", a), ("b/b.ik", b)];
    let (_tmp_dir, base_path, file_paths) = setup_test_dir(setup).unwrap();

    let result = compile(file_paths, &base_path, &Args::new());
    // TODO: Make an assertion about which error it is once that's possible
    assert!(result.is_err(), "{:?}", result);
}
