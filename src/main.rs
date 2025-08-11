use std::process;

use anyhow::Result;
use clap::Parser;
use asm6502::{Args, run};

fn main() -> Result<()> {
    let args = Args::parse();

    if let Err(e) = run(args) {
        eprintln!("{e}");
        process::exit(1);
    }

    Ok(())
}