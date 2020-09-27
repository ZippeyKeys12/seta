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
}

pub fn val_expr(input: &str) -> IResult<&str, Box<Expression>> {
    let parser = PrattParser::<Box<Expression>> {
        prefixes: &vec![(int_literal, int_expr)],

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
}
