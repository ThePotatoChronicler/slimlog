mod compiler;

use clap::Parser;
use log::{
    error,
    info,
    warn
};

use rand::rngs::StdRng;
use std::sync::Mutex;

#[derive(Parser)]
#[clap(author, version, about)]
struct Args {
    /// The slimlog file to compile
    file: String,
    #[clap(long, default_value_t = 6, parse(try_from_str=verify_varlen))]
    /// Length of unnamed variables
    varlen: usize,
    #[clap(long)]
    /// Order unnamed variables sequentially
    seqvars: bool,
    #[clap(long)]
    /// Enables translation optimizations [experimental]
    transopts: bool
}

lazy_static::lazy_static! {
    static ref ARGS: Args = Args::parse();
    static ref RNG: Mutex<Option<StdRng>> = Mutex::new(None);
}

fn get_source() -> Result<String, std::io::Error> {
    std::fs::read_to_string(&ARGS.file)
}

fn logger_init() {
    env_logger::builder()
        .init();
}

fn main() {
    logger_init();

    let source: String = match get_source() {
        Ok(s) => s,
        Err(e) => {
            error!("Failed to obtain source: {}", e);
            std::process::exit(1)
        }
    };
    info!("Source input:\n{}", source);
    initialize_rng(&source);

    if ARGS.transopts {
        warn!("Translation optimizations are experimental and may break your program, proceed with caution!");
    }

    let output: Result<String, String> = compiler::compile(&source);
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

fn initialize_rng(source: &str) {
    info!("Initializing PRNG");

    use rand::SeedableRng;
    use digest::{ VariableOutput, Update };
    let mut blake = blake2::Blake2bVar::new(32).expect("Failed to create blake2 hasher");
    blake.update(source.as_bytes());
    let mut out: Box<[u8; 32]> = Box::new([0; 32]);
    blake.finalize_variable(out.as_mut()).expect("Failed to finalize hash");
    log::info!("PRNG Seed: {}", hex::encode(*out));

    let mut rng = RNG.lock().unwrap();
    rng.replace(StdRng::from_seed(*out));

    info!("Initialized PRNG");
}
