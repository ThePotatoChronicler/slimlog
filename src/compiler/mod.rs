mod common;
mod compilation;
mod parser;
mod ast;
mod error;
mod instructions;
mod compiler_utils;

pub use compilation::compile;
