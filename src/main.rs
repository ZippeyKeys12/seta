#[macro_use]
mod general;
use crate::general::compilation_unit;

mod docs;
mod functions;
mod pratt;
mod types;
mod util;

use std::fs::File;
use std::io::prelude::*;

extern crate clap;
use clap::{App, Arg, SubCommand};

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
    compilation_unit(file);

    // match r {
    //     Ok((a, b)) => {
    //         println!("a: {}", a);
    //         println!("b: {}", b);
    //     }

    //     Err(e) => println!("{}", e),
    // }
}
