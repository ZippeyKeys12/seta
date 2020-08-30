#[macro_use]
pub mod general;

pub mod docs;
pub mod expression;
pub mod functions;
pub mod pratt;
pub mod types;

use docs::docstring;

extern crate nom;
use nom::{combinator::opt, multi::many0, IResult};

use functions::function_decl;

pub fn compilation_unit(input: &str) -> IResult<&str, ()> {
    let (input, file_doc) = opt(docstring)(input)?;
    let (input, funcs) = many0(function_decl)(input).unwrap();

    Ok((input, ()))
}
