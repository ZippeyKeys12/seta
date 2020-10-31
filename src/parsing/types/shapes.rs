use crate::parsing::{
    general::identifier,
    pratt::{PrattParser, MAX_PRECEDENCE},
};

use std::{collections::HashMap, fmt, str::FromStr};

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{all_consuming, complete},
    error::ErrorKind,
    multi::separated_list,
    sequence::{separated_pair, terminated},
    IResult,
};

// TODO: Intersection and Union order shouldn't matter
#[derive(Clone, Debug, PartialEq)]
pub enum ShapeType {
    Negation(Box<ShapeType>),
    Intersection(Box<ShapeType>, Box<ShapeType>),
    Union(Box<ShapeType>, Box<ShapeType>),
    Function(Box<ShapeType>, Box<ShapeType>),
    RecordLiteral(HashMap<String, ShapeType>),
    TupleLiteral(Vec<ShapeType>),
    Reference(String),
}

impl<'a> FromStr for ShapeType {
    type Err = nom::Err<ErrorKind>;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match all_consuming(complete(shape_expr))(input) {
            Ok(ok) => Ok(*ok.1),
            Err(nom::Err::Error(err)) => Err(nom::Err::Error(err.1)),
            Err(nom::Err::Failure(err)) => Err(nom::Err::Failure(err.1)),
            _ => Err(nom::Err::Error(ErrorKind::Eof)),
        }
    }
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
                |i| ws!(tag("("))(i),
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
    terminated(|i| parser.parse(i), ws!(tag(")")))(input)
}

fn tuple_expr<'a>(
    parser: &PrattParser<Box<ShapeType>>,
    input: &'a str,
    _token: &'a str,
) -> IResult<&'a str, Box<ShapeType>> {
    let (input, vals) = terminated(
        separated_list(ws!(tag(",")), |i| parser.parse(i)),
        ws!(tag(")")),
    )(input)?;

    if vals.is_empty() {
        return Err(nom::Err::Error((
            "Empty Tuple (This should never be seen)",
            nom::error::ErrorKind::Alt,
        )));
    };

    let mut res: Vec<ShapeType> = Vec::with_capacity(vals.len());

    for v in vals {
        res.push(*v)
    }

    Ok((input, Box::new(ShapeType::TupleLiteral(res))))
}

fn record_expr<'a>(
    parser: &PrattParser<Box<ShapeType>>,
    input: &'a str,
    _token: &'a str,
) -> IResult<&'a str, Box<ShapeType>> {
    let (input, pairs) = terminated(
        separated_list(
            ws!(tag(",")),
            separated_pair(ws!(identifier), ws!(tag(":")), |i| parser.parse(i)),
        ),
        ws!(tag(")")),
    )(input)?;

    if pairs.is_empty() {
        return Err(nom::Err::Error((
            "Empty Record (This should never be seen)",
            nom::error::ErrorKind::Alt,
        )));
    }

    let mut map = HashMap::<String, ShapeType>::with_capacity(pairs.len());

    for (k, v) in pairs {
        map.insert(k.to_string(), *v);
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

// TODO: Precedence tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal() {
        let (i, t) = shape_expr("A").unwrap();
        assert_eq!(*t, ShapeType::Reference("A".to_string()));
        assert_eq!(i, "")
    }

    #[test]
    fn test_intersection() {
        let test_values = vec![
            ("A&B", "A", "B"),
            ("Bf &Cwer", "Bf", "Cwer"),
            ("Dwr& Gfd", "Dwr", "Gfd"),
            ("fgE & dglW", "fgE", "dglW"),
        ];

        for (test_val, a, b) in test_values {
            let (i, t) = shape_expr(test_val).unwrap();
            assert_eq!(
                *t,
                ShapeType::Intersection(
                    Box::new(ShapeType::Reference(a.to_string())),
                    Box::new(ShapeType::Reference(b.to_string()))
                )
            );
            assert_eq!(i, "")
        }
    }

    #[test]
    fn test_union() {
        let test_values = vec![
            ("A|B", "A", "B"),
            ("Bf |Cwer", "Bf", "Cwer"),
            ("Dwr| Gfd", "Dwr", "Gfd"),
            ("fgE | dglW", "fgE", "dglW"),
        ];

        for (test_val, a, b) in test_values {
            let (i, t) = shape_expr(test_val).unwrap();
            assert_eq!(
                *t,
                ShapeType::Union(
                    Box::new(ShapeType::Reference(a.to_string())),
                    Box::new(ShapeType::Reference(b.to_string()))
                )
            );
            assert_eq!(i, "")
        }
    }

    #[test]
    fn test_function() {
        let test_values = vec![
            ("A->B", "A", "B"),
            ("Bf ->Cwer", "Bf", "Cwer"),
            ("Dwr-> Gfd", "Dwr", "Gfd"),
            ("fgE -> dglW", "fgE", "dglW"),
        ];

        for (test_val, a, b) in test_values {
            let (i, t) = shape_expr(test_val).unwrap();
            assert_eq!(
                *t,
                ShapeType::Function(
                    Box::new(ShapeType::Reference(a.to_string())),
                    Box::new(ShapeType::Reference(b.to_string()))
                )
            );
            assert_eq!(i, "")
        }
    }

    #[test]
    fn test_record_literal() {
        let test_values = vec![
            "(a:A,b:B)",
            "(a:A , b:B)",
            "(a :A,b :B)",
            "(a : A,b : B)",
            "(a : A , b : B)",
        ];

        for test_val in test_values {
            let (i, t) = shape_expr(test_val).unwrap();

            let mut tmp = HashMap::new();
            tmp.insert("a".to_string(), ShapeType::Reference("A".to_string()));
            tmp.insert("b".to_string(), ShapeType::Reference("B".to_string()));

            assert_eq!(*t, ShapeType::RecordLiteral(tmp));
            assert_eq!(i, "")
        }
    }

    #[test]
    fn test_tuple_literal() {
        let test_values = vec!["(A,B)", "(A, B)", "(A ,B)", "(A , B)", "( A , B )"];

        for test_val in test_values {
            let (i, t) = shape_expr(test_val).unwrap();

            let mut tmp = Vec::with_capacity(2);
            tmp.push(ShapeType::Reference("A".to_string()));
            tmp.push(ShapeType::Reference("B".to_string()));

            assert_eq!(*t, ShapeType::TupleLiteral(tmp));
            assert_eq!(i, "")
        }
    }

    #[test]
    fn test_order_of_operations() {
        let test_values = vec![
            (
                "A & B | C",
                ShapeType::Union(
                    Box::new(ShapeType::Intersection(
                        Box::new(ShapeType::Reference("A".to_string())),
                        Box::new(ShapeType::Reference("B".to_string())),
                    )),
                    Box::new(ShapeType::Reference("C".to_string())),
                ),
            ),
            (
                "A -> B | C",
                ShapeType::Function(
                    Box::new(ShapeType::Reference("A".to_string())),
                    Box::new(ShapeType::Union(
                        Box::new(ShapeType::Reference("B".to_string())),
                        Box::new(ShapeType::Reference("C".to_string())),
                    )),
                ),
            ),
            (
                "A -> B -> C",
                ShapeType::Function(
                    Box::new(ShapeType::Reference("A".to_string())),
                    Box::new(ShapeType::Function(
                        Box::new(ShapeType::Reference("B".to_string())),
                        Box::new(ShapeType::Reference("C".to_string())),
                    )),
                ),
            ),
            (
                "A | B -> C",
                ShapeType::Function(
                    Box::new(ShapeType::Union(
                        Box::new(ShapeType::Reference("A".to_string())),
                        Box::new(ShapeType::Reference("B".to_string())),
                    )),
                    Box::new(ShapeType::Reference("C".to_string())),
                ),
            ),
            (
                "A | B & C",
                ShapeType::Union(
                    Box::new(ShapeType::Reference("A".to_string())),
                    Box::new(ShapeType::Intersection(
                        Box::new(ShapeType::Reference("B".to_string())),
                        Box::new(ShapeType::Reference("C".to_string())),
                    )),
                ),
            ),
            (
                "(A | B) & C",
                ShapeType::Intersection(
                    Box::new(ShapeType::Union(
                        Box::new(ShapeType::Reference("A".to_string())),
                        Box::new(ShapeType::Reference("B".to_string())),
                    )),
                    Box::new(ShapeType::Reference("C".to_string())),
                ),
            ),
        ];

        for (test_val, ans) in test_values {
            let (i, t) = shape_expr(test_val).unwrap();
            assert_eq!(*t, ans);
            assert_eq!(i, "")
        }
    }
}
