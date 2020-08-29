use crate::general::identifier;

use std::{collections::HashSet, fmt, iter::FromIterator};

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, peek},
    multi::separated_list,
    sequence::delimited,
    IResult,
};

pub enum SecurityType {
    Top,
    Literal(HashSet<String>),
    Bottom,
}

impl fmt::Display for SecurityType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        if let Err(a) = write!(f, "{{") {
            return Err(a);
        };

        match self {
            SecurityType::Top => (),

            SecurityType::Literal(classes) => {
                if let Err(a) = write!(
                    f,
                    "{{ {} }}",
                    classes.iter().map(|c| c.clone()).fold(
                        String::new(),
                        |mut a: String, b: String| {
                            a.push_str(&b);
                            a
                        }
                    )
                ) {
                    return Err(a);
                }
            }

            SecurityType::Bottom => {
                if let Err(a) = write!(f, "_") {
                    {
                        return Err(a);
                    }
                }
            }
        };

        write!(f, "}}")
    }
}

pub fn sec_type_expr(input: &str) -> IResult<&str, Box<SecurityType>> {
    let (input, list) = delimited(
        tag("{"),
        alt((
            map(ws!(peek(tag("}"))), |x| Vec::new()),
            map(ws!(tag("_")), |x| vec![x]),
            separated_list(tag(","), ws!(identifier)),
        )),
        tag("}"),
    )(input)?;

    let tmp = match list[..] {
        [] => SecurityType::Top,
        ["_"] => SecurityType::Bottom,
        _ => SecurityType::Literal(HashSet::from_iter(list.iter().map(|t| t.to_string()))),
    };

    Ok((input, Box::new(tmp)))
}
