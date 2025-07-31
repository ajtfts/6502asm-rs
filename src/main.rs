use asm6502::{Config, assemble_from_file};
use std::error::Error;
use std::process;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    input_file: String,
    #[arg(short, long = "output")]
    output_file: Option<String>,
    #[arg(short='L', long="list")] // todo
    asm_list_file: Option<String>,
    #[arg(short='l', long="labels")]
    sym_list_file: Option<String>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let config = Config {
        input: args.input_file,
        output: args.output_file.unwrap_or(String::from("a.out")),
        asm_listing: args.asm_list_file,
        sym_listing: args.sym_list_file,
    };

    if let Err(e) = assemble_from_file(config) {
        eprintln!("{e}");
        process::exit(1);
    }

    Ok(())
}
