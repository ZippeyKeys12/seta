extern crate anyhow;
extern crate clap;
extern crate logos;

mod cst;
mod lexer;
mod util;

use clap::{App, Arg, SubCommand};
use cst::Parser;
use std::{
    fs::File,
    io::{self, prelude::*, Write},
    path::Path,
};

fn main() -> anyhow::Result<()> {
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
        // ("check", Some(sub_m)) => {
        //     let mut contents = String::new();
        //     let file = {
        //         let mut file = File::open(sub_m.value_of("FILE").unwrap()).unwrap();
        //         file.read_to_string(&mut contents).unwrap();
        //         contents.as_str()
        //     };
        //     let program = compilation_unit(file);
        //     assert_eq!(s, "");
        // }
        ("build", Some(sub_m)) => build(sub_m.value_of("FILE").unwrap()),
        ("", _) => repl()?,
        _ => {}
    };

    Ok(())
}

fn build(filename: &str) {
    let mut contents = String::new();
    let file = {
        let mut file = File::open(filename).unwrap();
        file.read_to_string(&mut contents).unwrap();
        contents.as_str()
    };

    // let program = parse(file);

    let mut path = Path::new(filename).to_path_buf();
    path.set_extension("out");

    // compile(path.as_path(), program).unwrap()
}

fn check(filename: &str) {
    let mut contents = String::new();
    let file = {
        let mut file = File::open(filename).unwrap();
        file.read_to_string(&mut contents).unwrap();
        contents.as_str()
    };

    // parse(file);
}

fn repl() -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut input = String::new();

    loop {
        print!("> ");
        stdout.flush()?;

        stdin.read_line(&mut input)?;

        let res = Parser::new(&input);
        println!("{:#?}", res);

        input.clear();
    }
}

// fn parse(file: &str) -> CompilationUnit {
//     file.parse::<CompilationUnit>().unwrap()
// }
