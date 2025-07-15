use asm6502::{Config, assemble_from_file};
use std::error::Error;
use std::{env, process};

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();

    let config = Config::new(&args).unwrap_or_else(|err| {
        println!("Problem parsing arguments: {}", err);
        process::exit(1);
    });

    if let Err(e) = assemble_from_file(config) {
        println!("Application error: {}", e);

        process::exit(1);
    }
    Ok(())
}
