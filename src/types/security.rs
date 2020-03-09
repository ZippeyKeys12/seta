use crate::general::identifier;

use std::iter::FromIterator;
use std::{cmp::Eq, collections::HashSet, fmt};

use nom::{bytes::complete::tag, multi::separated_list, sequence::delimited, IResult};

pub struct SecurityType {
    classes: HashSet<String>,
}

impl fmt::Display for SecurityType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        if let Err(a) = write!(f, "{{") {
            return Err(a);
        }

        if let Err(a) = write!(
            f,
            "{{ {} }}",
            self.classes.iter().map(|c| format!("{}", c)).fold(
                String::new(),
                |mut a: String, b: String| {
                    a.push_str(&b);
                    a
                }
            )
        ) {
            return Err(a);
        }

        write!(f, "}}")
    }
}

pub fn sec_type_expr(input: &str) -> IResult<&str, Box<SecurityType>> {
    let (input, list) = delimited(tag("{"), separated_list(tag(","), identifier), tag("}"))(input)?;

    Ok((
        input,
        Box::new(SecurityType {
            classes: HashSet::from_iter(list.iter().map(|t| t.to_string())),
        }),
    ))
}
