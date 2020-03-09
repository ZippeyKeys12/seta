extern crate toml;

extern crate nom;
use nom::{
    bytes::complete::{tag, take_until},
    error::context,
    sequence::delimited,
    IResult,
};

pub fn docstring(input: &str) -> IResult<&str, toml::Value> {
    let (input, data) = delimited(
        context("Start of docstring", tag("---")),
        take_until("---"),
        context("End of docstring", tag("---")),
    )(input)?;

    Ok((input, data.parse::<toml::Value>().unwrap()))
}
