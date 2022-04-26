mod common;
mod compilation;
mod parser;
mod ast;
mod error;
mod instructions;
mod compiler_utils;
mod context;

pub use compilation::compile;
