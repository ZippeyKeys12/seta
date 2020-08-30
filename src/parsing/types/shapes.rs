use crate::parsing::{
    general::identifier,
    pratt::{PrattParser, MAX_PRECEDENCE},
};

use std::{collections::HashMap, fmt};

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::peek,
    multi::separated_list,
    sequence::{delimited, separated_pair},
    IResult,
};

pub enum ShapeType {
    Negation(Box<ShapeType>),
    Intersection(Box<ShapeType>, Box<ShapeType>),
    Union(Box<ShapeType>, Box<ShapeType>),
    Function(Box<ShapeType>, Box<ShapeType>),
    RecordLiteral(HashMap<String, Box<ShapeType>>),
    TupleLiteral(Vec<Box<ShapeType>>),
    Reference(String),
}

impl fmt::Display for ShapeType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            ShapeType::Negation(op) => write!(f, "~{}", op),
            ShapeType::Intersection(op1, op2) => write!(f, "({} & {})", op1, op2),
            ShapeType::Union(op1, op2) => write!(f, "({} | {})", op1, op2),
            ShapeType::Function(op1, op2) => write!(f, "({} -> {})", op1, op2),
            ShapeType::RecordLiteral(map) => write!(
                f,
                "({})",
                // TODO: Use join when it's in stable
                map.iter().map(|(k, v)| format!("{}: {}", k, v)).fold(
                    String::new(),
                    |mut a: String, b: String| {
                        if a.is_empty() {
                            a.push_str(&b)
                        } else {
                            a.push_str(&format!(", {}", b))
                        };
                        a
                    }
                )
            ),
            ShapeType::TupleLiteral(map) => write!(
                f,
                "({})",
                // TODO: Use join when it's in stable
                map.iter().map(|v| v.to_string()).fold(
                    String::new(),
                    |mut a: String, b: String| {
                        if a.is_empty() {
                            a.push_str(&b)
                        } else {
                            a.push_str(&format!(", {}", b))
                        };
                        a
                    }
                )
            ),
            ShapeType::Reference(rec) => write!(f, "{}", rec),
        }
    }
}

pub fn shape_expr(input: &str) -> IResult<&str, Box<ShapeType>> {
    let parser = PrattParser::<Box<ShapeType>> {
        prefixes: &vec![
            (|i| ws!(tag("~"))(i), prefix_expr),
            (
                |i| ws!(peek(tag("(")))(i),
                |p, i, t| {
                    alt((
                        |i2| parentheses(p, i2, t),
                        |i2| record_expr(p, i2, t),
                        |i2| tuple_expr(p, i2, t),
                    ))(i)
                },
            ),
            (identifier, reference_expr),
        ],
        mixfixes: &vec![
            (100, |i| ws!(tag("&"))(i), infix_expr),
            (90, |i| ws!(tag("|"))(i), infix_expr),
            (80, |i| ws!(tag("->"))(i), infix_expr),
        ],
    };

    parser.parse(input)
}

fn reference_expr<'a>(
    _parser: &PrattParser<Box<ShapeType>>,
    input: &'a str,
    token: &'a str,
) -> IResult<&'a str, Box<ShapeType>> {
    Ok((input, Box::new(ShapeType::Reference(token.to_string()))))
}

fn parentheses<'a>(
    parser: &PrattParser<Box<ShapeType>>,
    input: &'a str,
    _token: &'a str,
) -> IResult<&'a str, Box<ShapeType>> {
    delimited(ws!(tag("(")), |i| parser.parse(i), ws!(tag(")")))(input)
}

fn tuple_expr<'a>(
    parser: &PrattParser<Box<ShapeType>>,
    input: &'a str,
    _token: &'a str,
) -> IResult<&'a str, Box<ShapeType>> {
    let (input, vals) = delimited(
        ws!(tag("(")),
        separated_list(ws!(tag(",")), |i| parser.parse(i)),
        ws!(tag(")")),
    )(input)?;

    if vals.is_empty() {
        return Err(nom::Err::Error((
            "Empty Tuple (This should never be seen)",
            nom::error::ErrorKind::Alt,
        )));
    }

    Ok((input, Box::new(ShapeType::TupleLiteral(vals))))
}

fn record_expr<'a>(
    parser: &PrattParser<Box<ShapeType>>,
    input: &'a str,
    _token: &'a str,
) -> IResult<&'a str, Box<ShapeType>> {
    let (input, pairs) = delimited(
        ws!(tag("(")),
        separated_list(
            ws!(tag(",")),
            separated_pair(identifier, ws!(tag(":")), |i| parser.parse(i)),
        ),
        ws!(tag(")")),
    )(input)?;

    if pairs.is_empty() {
        return Err(nom::Err::Error((
            "Empty Record (This should never be seen)",
            nom::error::ErrorKind::Alt,
        )));
    }

    let mut map = HashMap::<String, Box<ShapeType>>::with_capacity(pairs.len());

    for (k, v) in pairs {
        map.insert(k.to_string(), v);
    }

    Ok((input, Box::new(ShapeType::RecordLiteral(map))))
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
            let (input, shape) = parser.expression(input, precedence - 1)?;
            Ok((input, Box::new(ShapeType::Function(left, shape))))
        }

        "&" => {
            let (input, shape) = parser.expression(input, precedence - 1)?;
            Ok((input, Box::new(ShapeType::Intersection(left, shape))))
        }

        "|" => {
            let (input, shape) = parser.expression(input, precedence - 1)?;
            Ok((input, Box::new(ShapeType::Union(left, shape))))
        }

        _ => Result::Err(nom::Err::Error((
            "Unknown type operator",
            nom::error::ErrorKind::Alt,
        ))),
    }
}
