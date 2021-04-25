use argparse::{ArgumentParser, Store, StoreTrue};

pub struct Args {
    pub path: String,
    pub tokenize_only: bool,
    pub parse_only: bool,
}

impl Args {
    pub fn new() -> Self {
        Args{
            path: String::new(),
            tokenize_only: false,
            parse_only: false,
        }
    }

    // Parse CLI arguments. This may exit.
    pub fn parse() -> Self {
        let mut args = Args::new();

        {
            let mut ap = ArgumentParser::new();
            ap.set_description("ikrs compiler");
            ap.refer(&mut args.tokenize_only).add_option(
                &["--tokenize-only"],
                StoreTrue,
                "Only tokenize");
            ap.refer(&mut args.parse_only).add_option(
                &["--parse-only"],
                StoreTrue,
                "Only parse");
            ap.refer(&mut args.path).add_argument(
                "path",
                Store,
                "path to a directory containing a project or a single .ik file");
            ap.parse_args_or_exit();
        }

        if args.path.is_empty() {
            eprintln!("path argument is required");
            std::process::exit(1);
        }

        args
    }
}
