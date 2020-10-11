use super::functions::Function;
use super::types::{type_expr, Type};

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, char, newline, one_of},
    combinator::{map, opt},
    multi::separated_list,
    sequence::{delimited, separated_pair, terminated, tuple},
    IResult,
};

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

#[derive(Clone, Debug, PartialEq)]
pub enum Definition {
    FunctionDecl(Function),
    TypeDecl(String, Type),
}

pub fn identifier<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    alpha1(input) // Just letters
}

pub fn line_terminator(input: &str) -> IResult<&str, ()> {
    let ret = alt((ws!(char(';')), char('\n')))(input);

    match ret {
        Ok((input, _)) => Ok((input, ())),
        Err(err) => Err(err),
    }
}
