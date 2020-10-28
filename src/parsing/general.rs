use super::expression::Expression;
use super::types::{type_expr, Type};

use std::collections::HashMap;

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, line_ending},
    error::VerboseError,
    IResult,
};

// Adds whitespace in between symbols
macro_rules! ws {
    ($x: expr) => {
        nom::sequence::preceded(nom::character::complete::multispace0, $x)
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
    if input == "" {
        return Ok(("", ()));
    }

    let ret = alt((ws!(tag(";")), line_ending))(input);

    match ret {
        Ok((input, _)) => Ok((input, ())),
        Err(err) => Err(err),
    }
}
