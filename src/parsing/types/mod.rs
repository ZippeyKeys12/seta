use super::general::{identifier, line_terminator, Definition};

pub mod security;
pub mod shapes;

use security::{sec_type_expr, SecurityType};
use shapes::{shape_expr, ShapeType};

use std::{collections::HashMap, fmt, iter::FromIterator};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, newline},
    combinator::{map, opt},
    multi::separated_list,
    sequence::{delimited, separated_pair, terminated, tuple},
    IResult,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub shape: Box<ShapeType>,
    pub security: Box<SecurityType>,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}{}", self.shape, self.security)
    }
}

pub fn type_expr(input: &str) -> IResult<&str, Box<Type>> {
    map(
        tuple((ws!(shape_expr), ws!(opt(sec_type_expr)))),
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

pub fn type_decl(input: &str) -> IResult<&str, Definition> {
    let (input, name) = delimited(ws!(tag("type")), identifier, ws!(tag("=")))(input)?;
    let (input, typ) = terminated(ws!(type_expr), line_terminator)(input)?;

    Ok((input, Definition::TypeDecl(name.to_string(), *typ)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn test_type_expr() {
        let test_values = vec![
            (
                "A & B {_}",
                Type {
                    shape: Box::new(ShapeType::Intersection(
                        Box::new(ShapeType::Reference("A".to_string())),
                        Box::new(ShapeType::Reference("B".to_string())),
                    )),

                    security: Box::new(SecurityType::Top),
                },
            ),
            (
                "A -> D {R, S, T}",
                Type {
                    shape: Box::new(ShapeType::Function(
                        Box::new(ShapeType::Reference("A".to_string())),
                        Box::new(ShapeType::Reference("D".to_string())),
                    )),

                    security: Box::new(SecurityType::Literal({
                        let mut tmp = HashSet::new();
                        tmp.extend(vec!["R".to_string(), "S".to_string(), "T".to_string()]);
                        tmp
                    })),
                },
            ),
        ];

        for (test_val, ans) in test_values {
            let (i, t) = type_expr(test_val).unwrap();
            assert_eq!(*t, ans);
            assert_eq!(i, "");
        }
    }
}
