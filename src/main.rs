use std::fs::File;
use std::io::prelude::*;

extern crate clap;
use clap::{App, Arg, SubCommand};

extern crate nom;
use nom::{
    bytes::complete::{tag, take_until},
    error::context,
    sequence::delimited,
    IResult,
};

#[macro_use]
mod general;
mod pratt;
mod types;

fn main() {
    let app_m = App::new("Seta")
        .version("0.0.1")
        .author("Zain Aamer <zippeykeys12@gmail.com>")
        .subcommand(
            SubCommand::with_name("check")
                .about("Checks program correctness without compiling")
                .arg(
                    Arg::with_name("FILE")
                        .help("Entry file for program")
                        .required(true)
                        .index(1),
                ),
        )
        .subcommand(
            SubCommand::with_name("build")
                .about("Compiles the program")
                .arg(
                    Arg::with_name("FILE")
                        .help("Entry file for program")
                        .required(true)
                        .index(1),
                ),
        )
        .get_matches();

    match app_m.subcommand() {
        ("check", Some(sub_m)) => check(sub_m.value_of("FILE").unwrap()),
        ("build", Some(sub_m)) => check(sub_m.value_of("FILE").unwrap()),
        _ => {}
    };
}

fn check(filename: &str) {
    let mut contents = String::new();
    let file = {
        let mut file = File::open(filename).unwrap();
        file.read_to_string(&mut contents).unwrap();
        contents.as_str()
    };

    parse(file)
}

fn parse(file: &str) {
    docstring(file);

    // match r {
    //     Ok((a, b)) => {
    //         println!("a: {}", a);
    //         println!("b: {}", b);
    //     }

    //     Err(e) => println!("{}", e),
    // }
}

fn docstring(input: &str) -> IResult<&str, toml::Value> {
    let (input, data) = delimited(
        tag("---"),
        take_until("---"),
        context("End of docstring", tag("---")),
    )(input)?;

    Ok((input, data.parse::<toml::Value>().unwrap()))
}
