use super::*;

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
        "main.ik", "args.ik", "util/helpers.ik", "app/server/health.ik", "app/server/index.ik"
    ]
        .iter()
        .map(|s| s.to_string())
        .collect();

    let result = CompileJob::gather_files(&paths, &"".to_owned()).unwrap();

    assert!(result.packages.len() == 3);

    let main_pkg = get_package(&result, "main").unwrap();
    assert!(main_pkg.file_paths.contains(&"main.ik".to_string()));
    assert!(main_pkg.file_paths.contains(&"args.ik".to_string()));

    let util_pkg = get_package(&result, "util").unwrap();
    assert!(util_pkg.file_paths.contains(&"util/helpers.ik".to_string()));

    let server_pkg = get_package(&result, "app.server").unwrap();
    assert!(server_pkg.file_paths.contains(&"app/server/health.ik".to_string()));
    assert!(server_pkg.file_paths.contains(&"app/server/index.ik".to_string()));

}

#[test]
fn test_building_single_main_file() {
    let main = r#"
fn main():
  print("hello world")
"#;

    let tmp_dir = tempdir::TempDir::new("ikrs-tests").unwrap();
    let main_path = tmp_dir.path().join("main.ik");

    let base_path = tmp_dir.path().to_str().unwrap().to_string();
    let main_str = main_path.to_str().unwrap().to_string();
    {
        let mut tmp_file = File::create(main_path).unwrap();
        write!(tmp_file, "{}", main).unwrap();
    }

    let result = compile(vec![main_str], &base_path, false);
    assert!(result.is_ok());
}
