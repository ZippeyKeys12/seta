use super::expression::Expression;
use super::types::{type_expr, Type};

use std::collections::HashMap;

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, char, line_ending, newline, one_of},
    combinator::{map, opt},
    error::VerboseError,
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
    Function(FunctionDecl),
    Type(TypeDecl),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDecl {
    pub doc: Option<toml::Value>,
    pub name: String,
    pub parameters: HashMap<String, Box<Type>>,
    pub ret: (String, Box<Type>),
    pub body: Box<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeDecl {
    pub name: String,
    pub value: Type,
}

pub fn identifier<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    alpha1(input) // Just letters
}

pub fn line_terminator(input: &str) -> IResult<&str, ()> {
    assert_eq!(
        line_ending::<&str, VerboseError<&str>>("\r\nc"),
        Ok(("c", "\r\n"))
    );

    let ret = alt((ws!(tag(";")), line_ending))(input);

    match ret {
        Ok((input, _)) => Ok((input, ())),
        Err(err) => Err(err),
    }
}
