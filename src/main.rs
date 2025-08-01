use asm6502::assemble;
use std::process;
use std::fs::File;
use std::io::{self, Read, Write};
use clap::Parser;
use anyhow::{Result, bail};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    input_filename: String,
    #[arg(short, long = "output", default_value="a.out")]
    output_filename: String,
    #[arg(short='L', long="list")] // todo
    asm_list_filename: Option<String>,
    #[arg(short='l', long="labels")]
    sym_list_filename: Option<String>,
}

fn assemble_from_file(args: Args) -> Result<()> {
    println!("Assembling file:    {}", args.input_filename);

    let input_file = File::open(args.input_filename)?;
    let mut b = io::BufReader::new(input_file);

    let mut src: String = String::new();

    if let Err(e) = b.read_to_string(&mut src) {
        bail!(e);
    }

    match assemble(&src) {
        Ok(res) => {
            let mut output_file = File::create(args.output_filename.clone())?;
            output_file.write_all(&res.data[..])?;

            println!("Output file:        {}", args.output_filename);

            if let Some(sym_filename) = args.sym_list_filename {
                let mut sym_file = File::create(sym_filename)?;
                for (sym, oper) in res.symbols {
                    if let Err(e) = writeln!(sym_file, "{} = {:?}", sym, oper) {
                        bail!(e)
                    }
                }
            }

            Ok(())
        }
        Err(e) => bail!(e),
    }
}

fn main() -> Result<()> {
    let args = Args::parse();

    if let Err(e) = assemble_from_file(args) {
        eprintln!("{e}");
        process::exit(1);
    }

    Ok(())
}
