#[macro_use]
pub mod general;

pub mod docs;
pub mod expression;
pub mod functions;
pub mod pratt;
pub mod types;

use docs::docstring;

extern crate nom;
use nom::{branch::alt, combinator::opt, multi::many0, IResult};

use functions::function_decl;
use types::type_decl;

pub fn compilation_unit(input: &str) -> IResult<&str, ()> {
    let (input, _file_doc) = opt(docstring)(input)?;

    let (input, definitions) = many0(alt((type_decl, function_decl)))(input)?;
    assert_eq!(input, "");

    Ok((input, ()))
}
