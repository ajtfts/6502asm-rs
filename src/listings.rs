use std::{collections::HashMap, io::Write};

use anyhow::Result;

use crate::Operand;

pub struct ListingEntry {
    pub line_num: usize,
    pub offset: u16,
    pub hex: Vec<u8>,
    pub monitor: Option<String>,
    pub source: Option<String>,
}

pub fn write_asm_listing(listing: Vec<ListingEntry>, buf: &mut impl Write) -> Result<()> {
    
    Ok(())
}

pub fn write_sym_listing(symbols: HashMap<String, Operand>, buf: &mut impl Write) {
    for (sym, op) in symbols {
        if let Err(e) = writeln!(buf, "{} = {}", sym, op) {
            panic!("{}", e)
        }
    }
}