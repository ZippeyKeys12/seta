extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::digit1,
    combinator::{map, opt},
    sequence::{delimited, preceded, tuple},
    IResult,
};

pub enum Expression {
    IntLiteral(i32),
}

pub fn expression(input: &str) -> IResult<&str, Box<Expression>> {
    let (input, int) = digit1::<_, ()>(input).unwrap();
    let int = int.parse::<i32>().unwrap();

    Ok((input, Box::new(Expression::IntLiteral(int))))
}
