use super::*;

fn get_package<'a>(gp: &'a CompileJob, name: &str) -> Option<&'a Package> {
    for pf in gp.packaged_files.iter() {
        if pf.package_name == name {
            return Some(pf);
        }
    }
    None
}

#[test]
fn test_grouping_files() {
    let paths: Vec<String> = vec![
        "main.ik", "args.ik", "util/helpers.ik", "app/server/health.ik", "app/server/index.ik"
    ]
        .iter()
        .map(|s| s.to_string())
        .collect();

    let result = CompileJob::gather_files(&paths).unwrap();

    assert!(result.packaged_files.len() == 3);

    let main_pkg = get_package(&result, "main").unwrap();
    assert!(main_pkg.file_paths.contains(&"main.ik".to_string()));
    assert!(main_pkg.file_paths.contains(&"args.ik".to_string()));

    let util_pkg = get_package(&result, "util").unwrap();
    assert!(util_pkg.file_paths.contains(&"util/helpers.ik".to_string()));

    let server_pkg = get_package(&result, "app.server").unwrap();
    assert!(server_pkg.file_paths.contains(&"app/server/health.ik".to_string()));
    assert!(server_pkg.file_paths.contains(&"app/server/index.ik".to_string()));

}
