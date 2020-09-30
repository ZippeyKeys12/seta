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
    multi::separated_list,
    sequence::{delimited, preceded, terminated, tuple},
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
    Addition(Box<Expression>, Box<Expression>),
    Subtraction(Box<Expression>, Box<Expression>),
    Multiplication(Box<Expression>, Box<Expression>),
    Division(Box<Expression>, Box<Expression>),
    Reference(String),
    FunctionCall(String, Vec<Expression>),
    Dot(Box<Expression>, Box<Expression>),
}

pub fn val_expr(input: &str) -> IResult<&str, Box<Expression>> {
    let parser = PrattParser::<Box<Expression>> {
        prefixes: &vec![
            (int_literal, int_expr),
            (|i| ws!(alt((tag("-"), tag("~"), tag("!"))))(i), prefix_expr),
            (|i| ws!(tag("("))(i), parentheses),
            (
                |i| ws!(identifier)(i),
                |p, i, t| alt((|i2| function_call(p, i2, t), |i2| reference_expr(p, i2, t)))(i),
            ),
        ],

        mixfixes: &vec![
            (200, |i| ws!(tag("."))(i), dot_expr),
            (100, |i| ws!(alt((tag("*"), tag("/"))))(i), infix_expr),
            (90, |i| ws!(alt((tag("+"), tag("-"))))(i), infix_expr),
        ],
    };

    parser.parse(input)
}

lazy_static! {
    static ref INT_LITERAL_PATTERN: Regex = Regex::new(r"^\s*-?[1-9]\d*").unwrap();
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

fn reference_expr<'a>(
    _parser: &PrattParser<Box<Expression>>,
    input: &'a str,
    token: &'a str,
) -> IResult<&'a str, Box<Expression>> {
    Ok((input, Box::new(Expression::Reference(token.to_string()))))
}

fn parentheses<'a>(
    parser: &PrattParser<Box<Expression>>,
    input: &'a str,
    _token: &'a str,
) -> IResult<&'a str, Box<Expression>> {
    terminated(|i| parser.parse(i), ws!(tag(")")))(input)
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

fn function_call<'a>(
    parser: &PrattParser<Box<Expression>>,
    input: &'a str,
    token: &'a str,
) -> IResult<&'a str, Box<Expression>> {
    let (input, args) = delimited(
        ws!(tag("(")),
        separated_list(ws!(tag(",")), |i| parser.parse(i)),
        ws!(tag(")")),
    )(input)?;

    let mut res = Vec::with_capacity(args.len());

    for a in args {
        res.push(*a)
    }

    Ok((
        input,
        Box::new(Expression::FunctionCall(token.to_string(), res)),
    ))
}

fn infix_expr<'a>(
    parser: &PrattParser<Box<Expression>>,
    input: &'a str,
    left: Box<Expression>,
    token: &'a str,
    precedence: u16,
) -> IResult<&'a str, Box<Expression>> {
    match token {
        "+" => {
            let (input, expr) = parser.expression(input, precedence)?;
            Ok((input, Box::new(Expression::Addition(left, expr))))
        }

        "-" => {
            let (input, expr) = parser.expression(input, precedence)?;
            Ok((input, Box::new(Expression::Subtraction(left, expr))))
        }

        "*" => {
            let (input, expr) = parser.expression(input, precedence)?;
            Ok((input, Box::new(Expression::Multiplication(left, expr))))
        }

        "/" => {
            let (input, expr) = parser.expression(input, precedence)?;
            Ok((input, Box::new(Expression::Division(left, expr))))
        }

        _ => Result::Err(nom::Err::Error((
            "Unknown operator",
            nom::error::ErrorKind::Alt,
        ))),
    }
}

fn dot_expr<'a>(
    parser: &PrattParser<Box<Expression>>,
    input: &'a str,
    left: Box<Expression>,
    _token: &'a str,
    _precedence: u16,
) -> IResult<&'a str, Box<Expression>> {
    let (input, right) = parser.expression(input, _precedence)?;
    Ok((input, Box::new(Expression::Dot(left, right))))
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
            assert_eq!(*t, Expression::IntLiteral(test_val.parse().unwrap()));
            assert_eq!(i, "")
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
            assert_eq!(
                *t,
                Expression::Negation(Box::new(Expression::IntLiteral(
                    test_val[1..].parse().unwrap()
                )))
            );
            assert_eq!(i, "")
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
            assert_eq!(
                *t,
                Expression::Inversion(Box::new(Expression::IntLiteral(
                    test_val[1..].parse().unwrap()
                )))
            );
            assert_eq!(i, "")
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
            assert_eq!(
                *t,
                Expression::Not(Box::new(Expression::IntLiteral(
                    test_val[1..].parse().unwrap()
                )))
            );
            assert_eq!(i, "")
        }
    }

    #[test]
    fn test_addition() {
        let test_values = vec![
            ("1+32", 1, 32),
            ("5342 +32435", 5342, 32435),
            ("2365464+ 3564322", 2365464, 3564322),
            ("1 + 200", 1, 200),
        ];

        for (test_val, a, b) in test_values {
            let (i, t) = val_expr(test_val).unwrap();
            assert_eq!(
                *t,
                Expression::Addition(
                    Box::new(Expression::IntLiteral(a)),
                    Box::new(Expression::IntLiteral(b))
                )
            );
            assert_eq!(i, "")
        }
    }

    #[test]
    fn test_subtraction() {
        let test_values = vec![
            ("1-32", 1, 32),
            ("5342 -32435", 5342, 32435),
            ("2365464- 3564322", 2365464, 3564322),
            ("1 - 200", 1, 200),
        ];

        for (test_val, a, b) in test_values {
            let (i, t) = val_expr(test_val).unwrap();
            assert_eq!(
                *t,
                Expression::Subtraction(
                    Box::new(Expression::IntLiteral(a)),
                    Box::new(Expression::IntLiteral(b))
                )
            );
            assert_eq!(i, "")
        }
    }

    #[test]
    fn test_multiplication() {
        let test_values = vec![
            ("1*32", 1, 32),
            ("5342 *32435", 5342, 32435),
            ("2365464* 3564322", 2365464, 3564322),
            ("1 * 200", 1, 200),
        ];

        for (test_val, a, b) in test_values {
            let (i, t) = val_expr(test_val).unwrap();
            assert_eq!(
                *t,
                Expression::Multiplication(
                    Box::new(Expression::IntLiteral(a)),
                    Box::new(Expression::IntLiteral(b))
                )
            );
            assert_eq!(i, "")
        }
    }

    #[test]
    fn test_division() {
        let test_values = vec![
            ("1/32", 1, 32),
            ("5342 /32435", 5342, 32435),
            ("2365464/ 3564322", 2365464, 3564322),
            ("1 / 200", 1, 200),
        ];

        for (test_val, a, b) in test_values {
            let (i, t) = val_expr(test_val).unwrap();
            assert_eq!(
                *t,
                Expression::Division(
                    Box::new(Expression::IntLiteral(a)),
                    Box::new(Expression::IntLiteral(b))
                )
            );
            assert_eq!(i, "")
        }
    }
    #[test]
    fn test_reference() {
        let test_values = vec![
            "adfsdfd",
            "sghdjfg",
            "uyerfads",
            "hghriehgao",
            "qgargehiow",
            "qafhgilrbhof",
        ];

        for test_val in test_values {
            let (i, t) = val_expr(test_val).unwrap();
            assert_eq!(*t, Expression::Reference(test_val.to_string()));
            assert_eq!(i, "")
        }
    }

    #[test]
    fn test_order_of_operations() {
        let test_values = vec![
            (
                "dsafgsds * 64 + 29",
                Expression::Addition(
                    Box::new(Expression::Multiplication(
                        Box::new(Expression::Reference("dsafgsds".to_string())),
                        Box::new(Expression::IntLiteral(64)),
                    )),
                    Box::new(Expression::IntLiteral(29)),
                ),
            ),
            (
                "23 - sd + 34",
                Expression::Addition(
                    Box::new(Expression::Subtraction(
                        Box::new(Expression::IntLiteral(23)),
                        Box::new(Expression::Reference("sd".to_string())),
                    )),
                    Box::new(Expression::IntLiteral(34)),
                ),
            ),
            (
                "57 / 78 * 31",
                Expression::Multiplication(
                    Box::new(Expression::Division(
                        Box::new(Expression::IntLiteral(57)),
                        Box::new(Expression::IntLiteral(78)),
                    )),
                    Box::new(Expression::IntLiteral(31)),
                ),
            ),
            (
                "43 - 67 * sdfsdfs",
                Expression::Subtraction(
                    Box::new(Expression::IntLiteral(43)),
                    Box::new(Expression::Multiplication(
                        Box::new(Expression::IntLiteral(67)),
                        Box::new(Expression::Reference("sdfsdfs".to_string())),
                    )),
                ),
            ),
            (
                "76 - (21 + 34)",
                Expression::Subtraction(
                    Box::new(Expression::IntLiteral(76)),
                    Box::new(Expression::Addition(
                        Box::new(Expression::IntLiteral(21)),
                        Box::new(Expression::IntLiteral(34)),
                    )),
                ),
            ),
            (
                "45 / (87 * 12)",
                Expression::Division(
                    Box::new(Expression::IntLiteral(45)),
                    Box::new(Expression::Multiplication(
                        Box::new(Expression::IntLiteral(87)),
                        Box::new(Expression::IntLiteral(12)),
                    )),
                ),
            ),
            (
                "76 / (34 * (12 + 42))",
                Expression::Division(
                    Box::new(Expression::IntLiteral(76)),
                    Box::new(Expression::Multiplication(
                        Box::new(Expression::IntLiteral(34)),
                        Box::new(Expression::Addition(
                            Box::new(Expression::IntLiteral(12)),
                            Box::new(Expression::IntLiteral(42)),
                        )),
                    )),
                ),
            ),
            (
                "abc.asd(1+2, 3*4)",
                Expression::Dot(
                    Box::new(Expression::Reference("abc".to_string())),
                    Box::new(Expression::FunctionCall(
                        "asd".to_string(),
                        vec![
                            Expression::Addition(
                                Box::new(Expression::IntLiteral(1)),
                                Box::new(Expression::IntLiteral(2)),
                            ),
                            Expression::Multiplication(
                                Box::new(Expression::IntLiteral(3)),
                                Box::new(Expression::IntLiteral(4)),
                            ),
                        ],
                    )),
                ),
            ),
            (
                "121 + df.htdg()",
                Expression::Addition(
                    Box::new(Expression::IntLiteral(121)),
                    Box::new(Expression::Dot(
                        Box::new(Expression::Reference("df".to_string())),
                        Box::new(Expression::FunctionCall("htdg".to_string(), vec![])),
                    )),
                ),
            ),
        ];

        for (test_val, ans) in test_values {
            let (i, t) = val_expr(test_val).unwrap();
            assert_eq!(*t, ans);
            assert_eq!(i, "")
        }
    }
}
