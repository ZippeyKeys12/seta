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

extern crate nom;
use nom::{branch::alt, combinator::opt, multi::many0, IResult};

use functions::function_decl;
use types::type_decl;

pub struct CompilationUnit {
    pub definitions: Vec<Definition>,
}

pub fn compilation_unit(input: &str) -> IResult<&str, CompilationUnit> {
    let (input, _file_doc) = opt(docstring)(input)?;

    let (input, definitions) = many0(alt((type_decl, function_decl)))(input)?;
    let comp_unit = CompilationUnit { definitions };
    assert_eq!(input, "");

    Ok((input, comp_unit))
}
