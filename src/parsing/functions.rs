use super::{
    docs::docstring,
    expression::{val_expr, Expression},
    general::{identifier, Definition},
    types::{type_expr, type_spec, type_spec_list, Type},
};

use std::collections::HashMap;

extern crate toml;

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt},
    sequence::{delimited, preceded, tuple},
    IResult,
};

pub fn function_decl(input: &str) -> IResult<&str, Definition> {
    let (input, (doc, (name, arguments, ret), _, body)) = tuple((
        opt(ws!(docstring)),
        ws!(function_sig),
        ws!(tag("=")),
        ws!(val_expr),
    ))(input)?;

    let mut map = HashMap::<String, Box<Type>>::new();

    for (k, v) in arguments {
        map.insert(k.to_string(), v);
    }

    Ok((
        input,
        Definition::FunctionDecl {
            doc,
            name: name.to_string(),
            parameters: map,
            ret: (ret.0.to_string(), ret.1),
            body,
        },
    ))
}

type FunctionResult<'a> =
    IResult<&'a str, (&'a str, HashMap<&'a str, Box<Type>>, (&'a str, Box<Type>))>;

pub fn function_sig(input: &str) -> FunctionResult {
    tuple((
        ws!(preceded(ws!(tag("fn")), ws!(identifier))),
        // Parameters
        ws!(delimited(ws!(tag("(")), type_spec_list, ws!(tag(")")))),
        ws!(preceded(
            ws!(tag("->")),
            // Gets name for return value, or defaults to 'ret'
            ws!(alt((ws!(type_spec), ws!(map(type_expr, |i| ("ret", i)))))),
        )),
    ))(input)
}
