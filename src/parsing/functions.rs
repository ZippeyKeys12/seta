use super::{
    docs::docstring,
    expression::val_expr,
    general::{identifier, line_terminator, Definition, FunctionDecl},
    types::{
        security::SecurityType, shapes::ShapeType, type_expr, type_spec, type_spec_list, Type,
        TypeSpec,
    },
};

use std::{collections::HashMap, str::FromStr};

extern crate toml;

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{all_consuming, complete},
    combinator::{map, opt},
    error::ErrorKind,
    sequence::{delimited, preceded, tuple},
    IResult,
};

pub fn function_decl(input: &str) -> IResult<&str, Definition> {
    let (input, (doc, FunctionSignature(name, parameters, ret), _, body, _)) = tuple((
        opt(ws!(docstring)),
        ws!(function_sig),
        ws!(tag("=")),
        ws!(val_expr),
        opt(ws!(line_terminator)),
    ))(input)?;

    Ok((
        input,
        Definition::Function(FunctionDecl {
            doc,
            name,
            parameters,
            ret,
            body,
        }),
    ))
}

#[derive(Clone, PartialEq, Debug)]
pub struct FunctionSignature(String, HashMap<String, Type>, TypeSpec);

impl FromStr for FunctionSignature {
    type Err = nom::Err<ErrorKind>;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match all_consuming(complete(function_sig))(input) {
            Ok(ok) => Ok(ok.1),
            Err(nom::Err::Error(err)) => Err(nom::Err::Error(err.1)),
            Err(nom::Err::Failure(err)) => Err(nom::Err::Failure(err.1)),
            _ => Err(nom::Err::Error(ErrorKind::Eof)),
        }
    }
}

pub fn function_sig(input: &str) -> IResult<&str, FunctionSignature> {
    let (input, (a, b, c)) = tuple((
        ws!(preceded(ws!(tag("fn")), ws!(identifier))),
        // Parameters
        ws!(delimited(ws!(tag("(")), type_spec_list, ws!(tag(")")))),
        ws!(alt((
            preceded(
                tag("->"),
                // Gets name for return value, or defaults to 'ret'
                ws!(alt((
                    type_spec,
                    map(type_expr, |i| TypeSpec("ret".to_string(), *i))
                ))),
            ),
            |i| Ok((
                i,
                TypeSpec(
                    "ret".to_string(),
                    Type {
                        shape: Box::new("Unit".parse::<ShapeType>().unwrap()),
                        security: Box::new(SecurityType::Top)
                    }
                )
            ))
        ))),
    ))(input)?;

    Ok((input, FunctionSignature(a.to_string(), b, c)))
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parsing::{
        docs::Doc,
        types::{security::SecurityType, shapes::ShapeType},
        Expression,
    };

    #[test]
    fn test_function_sig() {
        let test_values = vec![
            (
                "fn a() -> A",
                FunctionSignature(
                    "a".to_string(),
                    HashMap::<String, Type>::new(),
                    "ret: A".parse::<TypeSpec>().unwrap(),
                ),
            ),
            (
                "fn b(a: A) -> B",
                FunctionSignature(
                    "b".to_string(),
                    {
                        let mut map = HashMap::<String, Type>::new();
                        map.insert("a".to_string(), "A".parse::<Type>().unwrap());

                        map
                    },
                    "ret: B".parse::<TypeSpec>().unwrap(),
                ),
            ),
            (
                "fn bc(a: A, c: C {}) -> rt: B",
                FunctionSignature(
                    "bc".to_string(),
                    {
                        let mut map = HashMap::<String, Type>::new();
                        map.insert("a".to_string(), "A".parse().unwrap());
                        map.insert("c".to_string(), "C {}".parse().unwrap());

                        map
                    },
                    "rt: B".parse().unwrap(),
                ),
            ),
        ];

        for (test_val, ans) in test_values {
            assert_eq!(test_val.parse::<FunctionSignature>().unwrap(), ans);
        }
    }

    #[test]
    fn test_function_decl() {
        let test_values = vec![
            (
                "fn a() -> B = 1",
                Definition::Function(FunctionDecl {
                    doc: None,
                    name: "a".to_string(),
                    parameters: HashMap::new(),
                    ret: TypeSpec("ret".to_string(), "B".parse::<Type>().unwrap()),
                    body: Box::new(Expression::IntLiteral(1)),
                }),
            ),
            (
                "fn abc(a: Int, b: Int {}) = 1",
                Definition::Function(FunctionDecl {
                    doc: None,
                    name: "abc".to_string(),
                    parameters: {
                        let mut map = HashMap::<String, Type>::new();
                        map.insert("a".to_string(), "Int".parse().unwrap());
                        map.insert("b".to_string(), "Int {}".parse().unwrap());

                        map
                    },
                    ret: "ret: Unit".parse().unwrap(),
                    body: Box::new(Expression::IntLiteral(1)),
                }),
            ),
            (
                "fn abc(a: A {}, b: B) -> Int = 1;",
                Definition::Function(FunctionDecl {
                    doc: None,
                    name: "abc".to_string(),
                    parameters: {
                        let mut map = HashMap::<String, Type>::new();
                        map.insert("a".to_string(), "A {}".parse().unwrap());
                        map.insert("b".to_string(), "B".parse().unwrap());

                        map
                    },
                    ret: TypeSpec("ret".to_string(), "Int".parse::<Type>().unwrap()),
                    body: Box::new(Expression::IntLiteral(1)),
                }),
            ),
            (
                "---\n---\nfn abc(a: Int, b: Int) -> sum: Int = 1 + 2;",
                Definition::Function(FunctionDecl {
                    doc: "\n".parse().ok(),
                    name: "abc".to_string(),
                    parameters: {
                        let mut map = HashMap::<String, Type>::new();
                        map.insert("a".to_string(), "Int".parse().unwrap());
                        map.insert("b".to_string(), "Int".parse().unwrap());

                        map
                    },
                    ret: "sum: Int".parse().unwrap(),
                    body: Box::new("1 + 2".parse().unwrap()),
                }),
            ),
        ];

        for (test_val, ans) in test_values {
            let (i, f) = function_decl(test_val).unwrap();

            assert_eq!(f, ans);
            assert_eq!(i, "");
        }
    }
}
