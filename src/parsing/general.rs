use crate::parsing::docs::docstring;

extern crate nom;
use nom::{character::complete::alpha1, combinator::opt, IResult};

pub fn identifier<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    alpha1(input) // Just letters
}

// Adds whitespace in between symbols
macro_rules! ws {
    ($x: expr) => {
        nom::sequence::delimited(
            nom::character::complete::multispace0,
            $x,
            nom::character::complete::multispace0,
        )
    };
}
