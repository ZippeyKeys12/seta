use crate::general::identifier;
use crate::pratt::{PrattParser, MAX_PRECEDENCE};

use std::{collections::HashMap, fmt};

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::{is_a, tag, take_until},
    character::complete::one_of,
    error::context,
    sequence::{delimited, tuple},
    IResult,
};

pub enum ShapeType {
    Negation(Box<ShapeType>),
    Intersection(Box<ShapeType>, Box<ShapeType>),
    Union(Box<ShapeType>, Box<ShapeType>),
    Function(Box<ShapeType>, Box<ShapeType>),
    Literal(Box<RecordType>),
}

impl fmt::Display for ShapeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            ShapeType::Negation(op) => write!(f, "(~{})", op),
            ShapeType::Intersection(op1, op2) => write!(f, "({} & {})", op1, op2),
            ShapeType::Union(op1, op2) => write!(f, "({} | {})", op1, op2),
            ShapeType::Function(op1, op2) => write!(f, "({} -> {})", op1, op2),
            ShapeType::Literal(rec) => write!(f, "({})", rec.name),
        }
    }
}

pub struct RecordType {
    name: String,
}

macro_rules! symbol {
    ($x: tt) => {
        |i| tag($x)(i)
    };
}

pub fn test(input: &str) -> IResult<&str, Box<ShapeType>> {
    let parser = PrattParser::<Box<ShapeType>> {
        prefixes: &vec![(symbol!("~"), prefix_expr), (identifier, literal_expr)],
        mixfixes: &vec![
            (100, symbol!("&"), infix_expr),
            (90, symbol!("|"), infix_expr),
            (80, symbol!("->"), infix_expr),
        ],
    };

    parser.type_expr(input, 0)
}

fn literal_expr<'a>(
    _parser: &PrattParser<Box<ShapeType>>,
    input: &'a str,
    token: &'a str,
) -> IResult<&'a str, Box<ShapeType>> {
    Ok((
        input,
        Box::new(ShapeType::Literal(Box::new(RecordType {
            name: token.to_string(),
        }))),
    ))
}

fn prefix_expr<'a>(
    parser: &PrattParser<Box<ShapeType>>,
    input: &'a str,
    token: &'a str,
) -> IResult<&'a str, Box<ShapeType>> {
    match token {
        "~" => {
            let (input, shape) = parser.type_expr(input, MAX_PRECEDENCE)?;
            Ok((input, Box::new(ShapeType::Negation(shape))))
        }

        _ => Result::Err(nom::Err::Error((
            "Unknown type operator",
            nom::error::ErrorKind::Alt,
        ))),
    }
}

fn infix_expr<'a>(
    parser: &PrattParser<Box<ShapeType>>,
    input: &'a str,
    left: Box<ShapeType>,
    token: &'a str,
    precedence: u16,
) -> IResult<&'a str, Box<ShapeType>> {
    match token {
        "->" => {
            let (input, shape) = parser.type_expr(input, precedence)?;
            Ok((input, Box::new(ShapeType::Function(left, shape))))
        }

        "&" => {
            let (input, shape) = parser.type_expr(input, precedence)?;
            Ok((input, Box::new(ShapeType::Intersection(left, shape))))
        }

        "|" => {
            let (input, shape) = parser.type_expr(input, precedence)?;
            Ok((input, Box::new(ShapeType::Union(left, shape))))
        }

        _ => Result::Err(nom::Err::Error((
            "Unknown type operator",
            nom::error::ErrorKind::Alt,
        ))),
    }
}
