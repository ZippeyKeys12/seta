use crate::general::identifier;
use crate::pratt::{PrattParser, MAX_PRECEDENCE};
use crate::types::Type;

use std::{collections::HashMap, fmt};

use rayon::prelude::*;

extern crate nom;
use nom::{bytes::complete::tag, character::complete::space0, sequence::delimited, IResult};

pub enum ShapeType {
    Negation(Box<ShapeType>),
    Intersection(Box<ShapeType>, Box<ShapeType>),
    Union(Box<ShapeType>, Box<ShapeType>),
    Function(Box<ShapeType>, Box<ShapeType>),
    // Name(Box<RecordType>),
    Literal(HashMap<String, Type>),
}

impl fmt::Display for ShapeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            ShapeType::Negation(op) => write!(f, "(~{})", op),
            ShapeType::Intersection(op1, op2) => write!(f, "({} & {})", op1, op2),
            ShapeType::Union(op1, op2) => write!(f, "({} | {})", op1, op2),
            ShapeType::Function(op1, op2) => write!(f, "({} -> {})", op1, op2),
            // ShapeType::Name(rec) => write!(f, "({})", rec.name),
            ShapeType::Literal(map) => write!(
                f,
                "{{ {} }}",
                map.par_iter().map(|(k, _)| format!("{}", k)).reduce(
                    || String::new(),
                    |mut a: String, b: String| {
                        a.push_str(&b);
                        a
                    }
                )
            ),
        }
    }
}

pub fn shape_expr(input: &str) -> IResult<&str, Box<ShapeType>> {
    let parser = PrattParser::<Box<ShapeType>> {
        prefixes: &vec![
            (|i| ws!(tag("~"))(i), prefix_expr),
            // (identifier, name_expr)
        ],
        mixfixes: &vec![
            (100, |i| ws!(tag("&"))(i), infix_expr),
            (90, |i| ws!(tag("|"))(i), infix_expr),
            (80, |i| ws!(tag("->"))(i), infix_expr),
        ],
    };

    parser.expression(input, 0)
}

// fn name_expr<'a>(
//     _parser: &PrattParser<Box<ShapeType>>,
//     input: &'a str,
//     token: &'a str,
// ) -> IResult<&'a str, Box<ShapeType>> {
//     Ok((
//         input,
//         Box::new(ShapeType::Literal(Box::new(RecordType {
//             name: token.to_string(),
//         }))),
//     ))
// }

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
