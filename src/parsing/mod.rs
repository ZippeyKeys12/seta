#[macro_use]
pub mod general;

pub mod docs;
pub mod expression;
pub mod functions;
pub mod pratt;
pub mod types;

pub use expression::Expression;
pub use general::{Definition, FunctionDecl, TypeDecl};
pub use types::{security::SecurityType, shapes::ShapeType, Type};

use docs::docstring;

use std::str::FromStr;

extern crate nom;
use nom::{
    branch::alt,
    combinator::opt,
    combinator::{all_consuming, complete},
    error::ErrorKind,
    multi::many0,
    IResult,
};

use functions::function_decl;
use types::type_decl;

pub struct CompilationUnit {
    pub definitions: Vec<Definition>,
}

impl FromStr for CompilationUnit {
    type Err = nom::Err<ErrorKind>;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match all_consuming(complete(compilation_unit))(input) {
            Ok(ok) => Ok(ok.1),
            Err(nom::Err::Error(err)) => Err(nom::Err::Error(err.1)),
            Err(nom::Err::Failure(err)) => Err(nom::Err::Failure(err.1)),
            _ => Err(nom::Err::Error(ErrorKind::Eof)),
        }
    }
}

pub fn compilation_unit<'a>(input: &'a str) -> IResult<&'a str, CompilationUnit> {
    let (input, _file_doc) = opt(docstring)(input)?;

    let (input, definitions) = many0(alt((type_decl, function_decl)))(input)?;
    let comp_unit = CompilationUnit { definitions };

    Ok((input, comp_unit))
}
