extern crate core;

mod ast;
mod error;
mod global_types;
pub mod hir;
mod lexer;
mod parser;
pub mod program;
mod resolver;

pub(crate) fn type_path_to_file_path(input: &str) -> String {
    format!("{}.as", input.replace('.', "/"))
}
