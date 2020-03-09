use crate::general::identifier;
use crate::pratt::{PrattParser, MAX_PRECEDENCE};
use crate::types::Type;

use std::fmt;

extern crate toml;

extern crate nom;
use nom::{bytes::complete::tag, character::complete::space0, sequence::delimited, IResult};

struct Function {
    doc: toml::Value,
    arguments: Vec<(String, Type)>,
}

