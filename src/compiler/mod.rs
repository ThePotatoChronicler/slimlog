mod common;
mod compiler;
mod parser;
mod ast;
mod error;
mod instructions;
mod compiler_utils;

pub use compiler::compile;
