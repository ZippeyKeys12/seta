use crate::docs::docstring;

extern crate nom;
use nom::{character::complete::alpha1, combinator::opt, IResult};

pub fn compilation_unit(input: &str) -> IResult<&str, ()> {
    let (input, file_doc) = opt(docstring)(input)?;

    Ok((input, ()))
}

pub fn identifier<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    alpha1(input)
}

macro_rules! ws {
    ($x: expr) => {
        nom::sequence::delimited(
            nom::character::complete::space0,
            $x,
            nom::character::complete::space0,
        )
    };
}
