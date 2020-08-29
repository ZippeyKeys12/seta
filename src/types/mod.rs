use crate::general::identifier;

mod security;
mod shapes;

use security::SecurityType;

use std::{collections::HashMap, fmt, iter::FromIterator};

use nom::{
    bytes::complete::tag,
    combinator::{map, opt},
    multi::{many_m_n, separated_list},
    sequence::{separated_pair, tuple},
    IResult,
};

pub struct Type {
    shape: Box<shapes::ShapeType>,
    security: Box<SecurityType>,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}{}", self.shape, self.security)
    }
}

pub fn type_expr(input: &str) -> IResult<&str, Box<Type>> {
    map(
        tuple((ws!(shapes::shape_expr), ws!(opt(security::sec_type_expr)))),
        |(shpe, sec)| {
            Box::new(Type {
                shape: shpe,
                security: match sec {
                    Some(s) => s,
                    None => Box::new(SecurityType::Top),
                },
            })
        },
    )(input)
}

pub fn type_spec(input: &str) -> IResult<&str, (&str, Box<Type>)> {
    separated_pair(identifier, ws!(tag(":")), type_expr)(input)
}

pub fn type_spec_list(input: &str) -> IResult<&str, HashMap<&str, Box<Type>>> {
    let (input, list) = separated_list(tag(","), type_spec)(input)?;

    Ok((input, HashMap::from_iter(list)))
}
