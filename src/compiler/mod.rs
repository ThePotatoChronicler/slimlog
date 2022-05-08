mod common;
mod compilation;
mod parser;
mod ast;
mod error;
mod instructions;
mod compiler_utils;
mod context;
mod settings;
mod utils;
mod optimizations;

pub use compilation::compile;
pub use settings::Settings;
