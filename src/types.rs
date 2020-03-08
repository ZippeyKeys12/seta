use crate::general::identifier;
use crate::pratt::{PrattParser, MAX_PRECEDENCE};

use std::fmt;

extern crate nom;
use nom::{bytes::complete::tag, character::complete::space0, sequence::delimited, IResult};

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

pub fn test(input: &str) -> IResult<&str, Box<ShapeType>> {
    let parser = PrattParser::<Box<ShapeType>> {
        prefixes: &vec![(symbol!("~"), prefix_expr), (identifier, literal_expr)],
        mixfixes: &vec![
            (100, padded_symbol!("&"), infix_expr),
            (90, padded_symbol!("|"), infix_expr),
            (80, padded_symbol!("->"), infix_expr),
        ],
    };

    parser.expression(input, 0)
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
            let (input, shape) = parser.expression(input, MAX_PRECEDENCE)?;
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
            let (input, shape) = parser.expression(input, precedence)?;
            Ok((input, Box::new(ShapeType::Function(left, shape))))
        }

        "&" => {
            let (input, shape) = parser.expression(input, precedence)?;
            Ok((input, Box::new(ShapeType::Intersection(left, shape))))
        }

        "|" => {
            let (input, shape) = parser.expression(input, precedence)?;
            Ok((input, Box::new(ShapeType::Union(left, shape))))
        }

        _ => Result::Err(nom::Err::Error((
            "Unknown type operator",
            nom::error::ErrorKind::Alt,
        ))),
    }
}
