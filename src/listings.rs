use std::{collections::HashMap, io::Write};
use std::fs::File;

use anyhow::Result;

use crate::Operand;

pub struct ListingEntry {
    pub line_num: usize,
    pub offset: u16,
    pub hex: Vec<u8>,
    pub monitor: Option<String>,
    pub source: Option<String>,
}

pub fn write_asm_listing(src: &str, data: Vec<u8>, pcs: Vec<usize>, fname: String) -> Result<()> {
    let mut buf = File::create(&fname)?;
    writeln!(buf, "6502asm Assembler V{} listing file", env!("CARGO_PKG_VERSION"))?;
    writeln!(buf, ";******  Processing input file: {}", fname)?;
    writeln!(buf)?;
    writeln!(buf)?;
    writeln!(buf, ";******  End of listing")?;
    Ok(())
}

pub fn write_sym_listing(symbols: HashMap<String, Operand>, fname: String) -> Result<()> {
    let mut buf = File::create(fname)?;
    for (sym, op) in symbols {
        if let Err(e) = writeln!(buf, "{} = {}", sym, op) {
            panic!("{}", e)
        }
    }
    Ok(())
}