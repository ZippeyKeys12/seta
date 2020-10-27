use std::{fs::File, io::prelude::*, path::Path};

extern crate clap;
use clap::{App, Arg, SubCommand};

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::opt,
    multi::many0,
    sequence::{delimited, tuple},
};

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate anyhow;

#[macro_use]
mod parsing;
use parsing::{compilation_unit, CompilationUnit};

mod code_generator;
use code_generator::compile;

mod util;
mod visitor;

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
        ("build", Some(sub_m)) => build(sub_m.value_of("FILE").unwrap()),
        _ => {}
    };
}

fn build(filename: &str) {
    let mut contents = String::new();
    let file = {
        let mut file = File::open(filename).unwrap();
        file.read_to_string(&mut contents).unwrap();
        contents.as_str()
    };

    let program = parse(file);

    let mut path = Path::new(filename).to_path_buf();
    path.set_extension(".out");

    compile(path.as_path(), program);
}

fn check(filename: &str) {
    let mut contents = String::new();
    let file = {
        let mut file = File::open(filename).unwrap();
        file.read_to_string(&mut contents).unwrap();
        contents.as_str()
    };

    // match r {
    //     Ok((a, b)) => {
    //         println!("a: {}", a);
    //         println!("b: {}", b);
    //     }
    parse(file);
}

    //     Err(e) => println!("{}", e),
    // }
fn parse(file: &str) -> CompilationUnit {
    let (s, comp_unit) = compilation_unit(file).unwrap();
    assert_eq!(s, "");
    comp_unit
}
