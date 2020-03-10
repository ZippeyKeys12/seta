use crate::docs::docstring;
use crate::general::identifier;
use crate::types::{type_expr, type_spec, type_spec_list, Type};

use std::collections::HashMap;

extern crate toml;

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::map,
    sequence::{delimited, preceded, tuple},
    IResult,
};

struct Function {
    doc: toml::Value,
    name: String,
    arguments: HashMap<String, Box<Type>>,
    ret: (String, Box<Type>),
}

fn function_decl(input: &str) -> IResult<&str, Function> {
    let (input, (doc, (name, arguments, ret))) = tuple((docstring, function_sig))(input)?;

    let mut map = HashMap::<String, Box<Type>>::new();

    for (k, v) in arguments {
        map.insert(k.to_string(), v);
    }

    Ok((
        input,
        Function {
            doc: doc,
            name: name.to_string(),
            arguments: map,
            ret: (ret.0.to_string(), ret.1),
        },
    ))
}

fn function_sig(input: &str) -> IResult<&str, (&str, HashMap<&str, Box<Type>>, (&str, Box<Type>))> {
    tuple((
        delimited(tag("fn"), ws!(identifier), tag("(")),
        type_spec_list,
        preceded(tag("->"), alt((type_spec, map(type_expr, |i| ("ret", i))))),
    ))(input)
}
