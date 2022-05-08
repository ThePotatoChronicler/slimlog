mod compiler;

use clap::Parser;
use log::{
    error,
    info,
    warn,
};

#[derive(Parser)]
#[clap(author, version, about)]
struct Args {
    /// The slimlog file to compile
    file: String,
    #[clap(long, default_value_t = 6, parse(try_from_str=verify_varlen))]
    /// Length of hexadecimal unnamed variables
    varlen: usize,
    #[clap(long)]
    /// Order unnamed variables sequentially
    seqvars: bool,
    #[clap(long)]
    /// Enables translation optimizations [experimental]
    transopts: bool
}

fn get_source(args: &Args) -> Result<String, std::io::Error> {
    std::fs::read_to_string(&args.file)
}

fn logger_init() {
    env_logger::builder()
        .init();
}

fn main() {
    logger_init();

    let args = Args::parse();

    let source: String = match get_source(&args) {
        Ok(s) => s,
        Err(e) => {
            error!("Failed to obtain source: {}", e);
            std::process::exit(1)
        }
    };
    info!("Source input:\n{}", source);

    if args.transopts {
        warn!("Translation optimizations are experimental and may break your program, proceed with caution!");
    }

    let mut settings = compiler::Settings::new();
    settings
        .transopts(args.transopts)
        .hex_unnamed_vars(!args.seqvars)
        .hex_var_length(args.varlen.try_into().expect("varlen should fit into u8"));

    let output: Result<String, String> = compiler::compile(&source, &settings);
    match output {
        Ok(out) => {
            println!("{}", out);
        },
        Err(error) => {
            error!("Error: {}", error);
            std::process::exit(1)
        }
    }
}

fn verify_varlen(varlen: &str) -> Result<usize, String> {
    let res: Result<usize, _> = varlen.parse();
    match res {
        Ok(len) => {
            if len > 34 {
                return Err("Variable length too large".into());
            } else if len == 0 {
                return Err("Variable length can't be zero".into());
            }

            if len < 4 {
                warn!("Too short length of variables is not recommended");
            }

            Ok(len)
        },
        Err(err) => Err(err.to_string())
    }
}
