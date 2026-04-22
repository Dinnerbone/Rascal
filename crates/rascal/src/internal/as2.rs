extern crate core;

pub mod ast;
pub mod error;
mod global_types;
pub mod hir;
pub mod lexer;
pub mod parser;
pub mod resolver;

pub fn type_path_to_file_path(input: &str) -> String {
    format!("{}.as", input.replace('.', "/"))
}
