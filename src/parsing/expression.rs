use crate::parsing::{
    general::identifier,
    pratt::{PrattParser, MAX_PRECEDENCE},
};

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::digit1,
    combinator::{map, opt},
    sequence::{delimited, preceded, tuple},
    IResult,
};

extern crate regex;
use regex::Regex;

#[derive(Debug, PartialEq)]
pub enum Expression {
    IntLiteral(i32),
    Negation(Box<Expression>),
    Inversion(Box<Expression>),
    Not(Box<Expression>),
}

pub fn val_expr(input: &str) -> IResult<&str, Box<Expression>> {
    let parser = PrattParser::<Box<Expression>> {
        prefixes: &vec![
            (int_literal, int_expr),
            (|i| ws!(alt((tag("-"), tag("~"), tag("!"))))(i), prefix_expr),
        ],

        mixfixes: &vec![],
    };

    parser.parse(input)
}

lazy_static! {
    static ref INT_LITERAL_PATTERN: Regex = Regex::new(r"^-?[1-9]\d*(\s|$)").unwrap();
}

fn int_literal<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    match INT_LITERAL_PATTERN.find(input) {
        Some(mat) => match mat.as_str().parse::<i32>() {
            Ok(_) => Ok((&input[mat.end()..], mat.as_str())),
            _ => Err(nom::Err::Error((
                "Integer not i32",
                nom::error::ErrorKind::Digit,
            ))),
        },
        _ => Err(nom::Err::Error((
            "Not an integer",
            nom::error::ErrorKind::Digit,
        ))),
    }
}

fn int_expr<'a>(
    _parser: &PrattParser<Box<Expression>>,
    input: &'a str,
    token: &'a str,
) -> IResult<&'a str, Box<Expression>> {
    Ok((
        input,
        Box::new(Expression::IntLiteral(token.parse::<i32>().unwrap())),
    ))
}

fn prefix_expr<'a>(
    parser: &PrattParser<Box<Expression>>,
    input: &'a str,
    token: &'a str,
) -> IResult<&'a str, Box<Expression>> {
    match token {
        "-" => {
            let (input, shape) = parser.expression(input, MAX_PRECEDENCE)?;
            Ok((input, Box::new(Expression::Negation(shape))))
        }

        "~" => {
            let (input, shape) = parser.expression(input, MAX_PRECEDENCE)?;
            Ok((input, Box::new(Expression::Inversion(shape))))
        }

        "!" => {
            let (input, shape) = parser.expression(input, MAX_PRECEDENCE)?;
            Ok((input, Box::new(Expression::Not(shape))))
        }

        _ => Result::Err(nom::Err::Error((
            "Unknown type operator",
            nom::error::ErrorKind::Alt,
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_literal() {
        let test_values = vec![
            "1234",
            "7864563",
            "-2365464",
            "34636543",
            "2147483647",
            "-2147483648",
        ];

        for test_val in test_values {
            let (i, t) = val_expr(test_val).unwrap();
            assert_eq!(i, "");
            assert_eq!(*t, Expression::IntLiteral(test_val.parse().unwrap()))
        }
    }

    #[test]
    #[should_panic]
    fn test_int_literal_over() {
        let (_, _) = val_expr("2147483648").unwrap();
    }

    #[test]
    #[should_panic]
    fn test_int_literal_under() {
        let (_, _) = val_expr("-2147483649").unwrap();
    }

    #[test]
    fn test_negation() {
        let test_values = vec![
            "--1234",
            "--7864563",
            "--2365464",
            "--34636543",
            "--2147483648",
        ];

        for test_val in test_values {
            let (i, t) = val_expr(test_val).unwrap();
            assert_eq!(i, "");
            assert_eq!(
                *t,
                Expression::Negation(Box::new(Expression::IntLiteral(
                    test_val[1..].parse().unwrap()
                )))
            )
        }
    }

    #[test]
    fn test_inversion() {
        let test_values = vec![
            "~-1234",
            "~-7864563",
            "~-2365464",
            "~-34636543",
            "~-2147483648",
        ];

        for test_val in test_values {
            let (i, t) = val_expr(test_val).unwrap();
            assert_eq!(i, "");
            assert_eq!(
                *t,
                Expression::Inversion(Box::new(Expression::IntLiteral(
                    test_val[1..].parse().unwrap()
                )))
            )
        }
    }

    #[test]
    fn test_not() {
        let test_values = vec![
            "!-1234",
            "!-7864563",
            "!-2365464",
            "!-34636543",
            "!-2147483648",
        ];

        for test_val in test_values {
            let (i, t) = val_expr(test_val).unwrap();
            assert_eq!(i, "");
            assert_eq!(
                *t,
                Expression::Not(Box::new(Expression::IntLiteral(
                    test_val[1..].parse().unwrap()
                )))
            )
        }
    }
}
