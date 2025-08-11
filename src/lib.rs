mod listings;
mod error;
mod opcodes;
mod instruction;

use std::fs::File;
use std::io::{Read, Write};
use std::collections::{HashMap, HashSet, VecDeque};

use anyhow::{Result, bail};
use lazy_static::lazy_static;
use regex::Regex;
use clap::Parser;

use listings::{ListingEntry, write_asm_listing, write_sym_listing};
use error::AsmError;
use opcodes::OPCODES;
use instruction::{Operand, Instruction, parse_inst};

lazy_static! {
    static ref RE_TOKEN: Regex = Regex::new(r"((;[^\n\r]*)|((\$[0-9a-fA-F]+)|(%[01]+)|(\b\d+)|('[ -~]'))|(\w+)|([^\w\s]))").unwrap();
    static ref RE_WORD: Regex = Regex::new(r"^\w+$").unwrap();
    static ref RE_NUM_LITERAL: Regex = Regex::new(r"(\$[0-9a-fA-F]+|%[01]+|\b\d+)").unwrap();
    static ref RE_COMMENT: Regex = Regex::new(r";[^\n\r]*").unwrap();
    static ref RE_HEX_LITERAL: Regex = Regex::new(r"\$[0-9a-fA-F]+").unwrap();
    static ref RE_DEC_LITERAL: Regex = Regex::new(r"\b\d+").unwrap();
    static ref RE_BIN_LITERAL: Regex = Regex::new(r"%[01]+").unwrap();
}

static MAX_PASSES: usize = 10;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
pub struct Args {
    pub input_fname: String,
    #[arg(short, long = "output", default_value = "a.out")]
    pub output_fname: String,
    #[arg(short = 'L', long = "list")]
    pub asm_listing_fname: Option<String>,
    #[arg(short = 'l', long = "labels")]
    pub sym_listing_fname: Option<String>,
}

trait AsmItem {
    fn to_bytes(&self) -> Result<Vec<u8>, AsmError>;
}

impl AsmItem for Instruction {
    fn to_bytes(&self) -> Result<Vec<u8>, AsmError> {
        let mut data = Vec::new();
        // should be able to unwrap here for now as Instruction is
        // constructed after checking that OPCODES contains the op name.
        let opcode = OPCODES.get(&self.name).unwrap()[self.mode as usize]; 
        data.push(opcode);
        match &self.operand {
            Some(Operand::Bytes(bytes)) => {
                data.append(&mut bytes.to_owned().into_iter().rev().collect());
            },
            Some(Operand::Symbol(sym)) => {
                return Err(AsmError::UnresolvedSymbol(sym.to_string()))
            },
            Some(Operand::Pos(_)) => {
                todo!() // I don't think this should happen?
            },
            None => {}
        }
        Ok(data)
    }
}

struct AsmResult {
    data: Vec<u8>,
    asm_items: Vec<Box<dyn AsmItem>>,
    symbols: HashMap<String, Operand>,
    finished: bool,
}

fn resolve_symbols(symbols: &mut HashMap<String, Operand>) -> Result<(), AsmError> {
    let mut to_update: HashMap<String, Operand> = HashMap::new();
    let mut seen: HashSet<String> = HashSet::new();

    for (key_symbol, operand) in symbols.iter() { 
        let mut current: &Operand = operand;

        loop {
            match current {
                Operand::Bytes(val) => {
                    to_update.insert(key_symbol.to_string(), Operand::Bytes(val.clone()));
                    break;
                },
                Operand::Pos(inst_pos) => {
                    to_update.insert(key_symbol.to_string(), Operand::Pos(*inst_pos));
                    break;
                },
                Operand::Symbol(sym) => {
                    if !seen.contains(key_symbol) && let Some(new_op) = symbols.get(sym) {
                        current = new_op;
                        seen.insert(key_symbol.to_string());
                    } else {
                        return Err(AsmError::UndefinedSymbol(sym.to_string())); // type on this might need to change
                    }
                },
            }
        }
    }

    for (symbol, operand) in to_update.into_iter() {
        symbols.insert(symbol, operand);
    }

    Ok(())
}

fn tokenize<'h>(src: &'h str) -> VecDeque<&'h str> {
    RE_TOKEN.find_iter(src).into_iter().map(|m| m.as_str()).collect()
}

fn parse_num_literal(literal: &str) -> Result<Vec<u8>> {
    let val: u16;

    if RE_HEX_LITERAL.is_match(literal) {
        return hex::decode(&literal[1..]).map_err(|e| anyhow::Error::new(e))
    } else if RE_BIN_LITERAL.is_match(literal) {
        val = u16::from_str_radix(&literal[1..], 2)?;
    } else if RE_DEC_LITERAL.is_match(literal) {
        val = literal.parse::<u16>()?;
    } else {
        bail!("") // todo
    }

    if val > 255 {
        Ok(val.to_be_bytes().to_vec())
    } else {
        Ok(vec![val as u8])
    }
}

fn create_data(asm_items: &Vec<Box<dyn AsmItem>>) -> Result<Vec<u8>, AsmError> {
    let mut data: Vec<u8> = vec![];
    
    for item in asm_items {
        let bytes = item.to_bytes()?;
        for byte in bytes {
            data.push(byte);
        }
    }
    
    Ok(data)
}

fn first_pass(src: &str) -> Result<AsmResult, AsmError> {
    let mut asm_items: Vec<Box<dyn AsmItem>> = vec![];
    let mut symbols: HashMap<String, Operand> = HashMap::new();
    for (line_num, line) in src.lines().enumerate() {
        let mut tokens: VecDeque<&str> = tokenize(line);

        // remove comments from end of line
        if !tokens.is_empty() && RE_COMMENT.is_match(&tokens[tokens.len() - 1]) {
            tokens.pop_back();
        }

        // pop labels/assignments
        while tokens.len() > 0 && !opcodes::OPCODES.contains_key(&tokens[0].to_uppercase()) {
            if RE_WORD.is_match(&tokens[0]) {
                // Assigning to a symbol
                if tokens.len() > 2 && tokens[1] == "=" {
                    let operand = tokens[2];
                    if RE_NUM_LITERAL.is_match(operand) {
                        symbols.insert(
                            String::from(tokens.pop_front().unwrap()),
                            Operand::Bytes(parse_num_literal(operand)
                                .map_err(|_| AsmError::InvalidSyntax { line_num, line: line.to_string() })?)
                        );
                    } else if RE_WORD.is_match(operand) {
                        symbols.insert(
                            String::from(tokens.pop_front().unwrap()),
                            Operand::Symbol(operand.to_string())
                        );
                    } else if operand == "*" {
                        todo!("pc set not yet supported");
                    }
                    tokens.pop_front();
                    tokens.pop_front();
                } else { // labels
                    symbols.insert(tokens.pop_front().unwrap().to_string(), Operand::Pos(asm_items.len()));
                }
            } else {
                return Err(AsmError::InvalidSyntax {
                    line_num,
                    line: line.to_string()
                })
            }
        }

        if tokens.is_empty() {
            continue;
        }

        // handle instructions
        if OPCODES.contains_key(&tokens[0].to_uppercase()) {
            let inst = parse_inst(tokens, line_num, line)?;
            asm_items.push(Box::new(inst));
        }
    }

    match create_data(&asm_items) {
        Ok(data) => Ok(AsmResult {
            data,
            asm_items,
            symbols,
            finished: true,
        }),
        Err(AsmError::UnresolvedSymbol(_)) => Ok(AsmResult {
            data: vec![],
            asm_items,
            symbols,
            finished: false,
        }),
        Err(e) => Err(e)
    }
}

fn pass(src: &str, res: &mut AsmResult) -> Result<(), AsmError> {

    resolve_symbols(&mut res.symbols)?;

    res.finished = true;

    Ok(())
}

fn assemble(src: &str) -> Result<AsmResult, AsmError> {

    // There are apparently inputs for which 64tass will never terminate (I haven't been able to
    // figure out what they are yet), which is why we are keeping track of the number of passes here. 
    let mut res = first_pass(&src)?;
    let mut pass_count = 1;

    while !res.finished {
        match pass(&src, &mut res) {
            Ok(()) => {
                if pass_count >= MAX_PASSES {
                    return Err(AsmError::TooManyPasses(MAX_PASSES, pass_count))
                }
                pass_count += 1;
            },
            Err(e) => return Err(e),
        }
    }

    Ok(res)
}

pub fn run(args: Args) -> Result<()> {

    println!("Assembling file:    {}", args.input_fname);
    let mut input_file = File::open(args.input_fname)?;
    let mut src = String::new();
    input_file.read_to_string(&mut src)?;

    match assemble(&src) {
        Ok(res) => {
            let mut output_file = File::create(&args.output_fname)?;
            output_file.write_all(&res.data)?;

            println!("Output file:        {}", args.output_fname);

            // Write to assembly listing file
            if let Some(asm_listing_fname) = args.asm_listing_fname {
                let mut asm_listing_file = File::create(asm_listing_fname)?;
                todo!();//write_asm_listing(res.asm_listing, &mut asm_listing_file);
            }

            // Write to symbol listing file
            if let Some(sym_listing_fname) = args.sym_listing_fname {
                let mut sym_listing_file = File::create(sym_listing_fname)?;
                write_sym_listing(res.symbols, &mut sym_listing_file);
            }
        },
        Err(e) => bail!(e),
    }

    Ok(())
}

#[rustfmt::skip]
#[cfg(test)]
mod tests {
    use super::*;
    // use proptest::prelude::*;

    /*proptest! {
        #[test]
        fn assemble_doesnt_crash(s in "\\PC*") {
            let _ = assemble(&s);
        }

        /*#[test]
        fn assemble_parses_single_abs(s in "(?i)(ADC|AND|ASL|BIT|CMP|CPX|CPY|DEC|EOR|INC|JMP|JSR)[^\\S\\n\\r]*\\$[0-9a-fA-F]{4}") {
            let data = assemble(&s).unwrap();
            
            assert_eq!(data.len(), 3);
        }

        #[test]
        fn assemble_parses_single_imp(s in "((?i)(BR))[kK]") { // Kelvin character...
            let data = assemble(&s).unwrap();

            assert_eq!(data.len(), 1);
        }*/
    }*/

    // Instruction parsing

    #[test]
    fn parse_addr_mode_a() {
        // Should include all instructions that support accumulator addressing mode
        let data: Vec<u8> = assemble("
            ASL A
            LSR A
            ROL A
            ROR A
        ").unwrap().data;
        
        let hex: Vec<u8> = vec![
            0x0A,
            0x4A,
            0x2A,
            0x6A
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn parse_addr_mode_imp() {
        // Should include all implied addressing mode instructions
        let data: Vec<u8> = assemble("
            BRK
            CLC
            CLD
            CLI
            CLV
            DEX
            DEY
            INX
            INY
            NOP
            PHA
            PHP
            PLA
            PLP
            RTI
            RTS
            SEC
            SED
            SEI
            TAX
            TAY
            TSX
            TXA
            TXS
            TYA
        ").unwrap().data;

        let hex: Vec<u8> = vec![
            0x00,
            0x18,
            0xD8,
            0x58,
            0xB8,
            0xCA,
            0x88,
            0xE8,
            0xC8,
            0xEA,
            0x48,
            0x08,
            0x68,
            0x28,
            0x40,
            0x60,
            0x38,
            0xF8,
            0x78,
            0xAA,
            0xA8,
            0xBA,
            0x8A,
            0x9A,
            0x98,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn parse_addr_mode_abs() {
        // Should include all instructions that support absolute addressing mode
        let data = assemble("
            ADC $DF42
            AND $C2B8
            ASL $74A4
            BIT $8A36
            CMP $0D72
            CPX $9D40
            CPY $3F4D
            DEC $49A4
            EOR $3C32
            INC $1E22
            JMP $0B89
            JSR $1109
            LDA $96F6
            LDX $6DEC
            LDY $4B75
            LSR $AF70
            ORA $8558
            ROL $B2A7
            ROR $43A2
            SBC $C909
            STA $47A0
            STX $22A8
            STY $DFB0
            
        ").unwrap().data;

        let hex = vec![
            0x6D, 0x42, 0xDF,
            0x2D, 0xB8, 0xC2,
            0x0E, 0xA4, 0x74,
            0x2C, 0x36, 0x8A,
            0xCD, 0x72, 0x0D,
            0xEC, 0x40, 0x9D,
            0xCC, 0x4D, 0x3F,
            0xCE, 0xA4, 0x49,
            0x4D, 0x32, 0x3C,
            0xEE, 0x22, 0x1E,
            0x4C, 0x89, 0x0B,
            0x20, 0x09, 0x11,
            0xAD, 0xF6, 0x96,
            0xAE, 0xEC, 0x6D,
            0xAC, 0x75, 0x4B,
            0x4E, 0x70, 0xAF,
            0x0D, 0x58, 0x85,
            0x2E, 0xA7, 0xB2,
            0x6E, 0xA2, 0x43,
            0xED, 0x09, 0xC9,
            0x8D, 0xA0, 0x47,
            0x8E, 0xA8, 0x22,
            0x8C, 0xB0, 0xDF,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn parse_addr_mode_abs_x() {
        let data = assemble("
            ADC $A3F1,X
            AND $5C0B,X
            ASL $E29D,X
            CMP $1F70,X
            DEC $8B2C,X
            EOR $F04E,X
            INC $3A65,X
            LDA $7D19,X
            LDY $C482,X
            LSR $2E5F,X
            ORA $9B37,X
            ROL $41AC,X
            ROR $D6EF,X
            SBC $08C3,X
            STA $67BD,X
        ").unwrap().data;

        let hex = vec![
            0x7D, 0xF1, 0xA3,
            0x3D, 0x0B, 0x5C,
            0x1E, 0x9D, 0xE2,
            0xDD, 0x70, 0x1F,
            0xDE, 0x2C, 0x8B,
            0x5D, 0x4E, 0xF0,
            0xFE, 0x65, 0x3A,
            0xBD, 0x19, 0x7D,
            0xBC, 0x82, 0xC4,
            0x5E, 0x5F, 0x2E,
            0x1D, 0x37, 0x9B,
            0x3E, 0xAC, 0x41,
            0x7E, 0xEF, 0xD6,
            0xFD, 0xC3, 0x08,
            0x9D, 0xBD, 0x67,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn parse_addr_mode_abs_y() {
        let data = assemble("
            ADC $22A8,Y 
            AND $DFB0,Y
            CMP $35EE,Y
            EOR $7F14,Y
            LDA $9F7B,Y
            LDX $CDE7,Y
            ORA $7FDD,Y
            SBC $14F1,Y
            STA $A39F,Y
        ").unwrap().data;

        let hex = vec![
            0x79, 0xA8, 0x22,
            0x39, 0xB0, 0xDF,
            0xD9, 0xEE, 0x35,
            0x59, 0x14, 0x7F,
            0xB9, 0x7B, 0x9F,
            0xBE, 0xE7, 0xCD,
            0x19, 0xDD, 0x7F,
            0xF9, 0xF1, 0x14,
            0x99, 0x9F, 0xA3,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn parse_addr_mode_indirect() {
        let data = assemble("
            JMP ($C3B4)
        ").unwrap().data;

        let hex = vec![
            0x6C, 0xB4, 0xC3,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn parse_addr_mode_x_indirect() {
        let data = assemble("
            ADC ($46,X)
            AND ($B0,X)
            CMP ($29,X)
            EOR ($13,X)
            LDA ($36,X)
            ORA ($C0,X)
            SBC ($8D,X)
            STA ($B0,X)
        ").unwrap().data;

        let hex = vec![
            0x61, 0x46,
            0x21, 0xB0,
            0xC1, 0x29,
            0x41, 0x13,
            0xA1, 0x36,
            0x01, 0xC0,
            0xE1, 0x8D,
            0x81, 0xB0,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn parse_addr_mode_indirect_y() {
        let data = assemble("
            ADC ($DF),Y
            AND ($5E),Y
            CMP ($9B),Y
            EOR ($1C),Y
            LDA ($14),Y
            ORA ($7D),Y
            SBC ($35),Y
            STA ($5A),Y
        ").unwrap().data;

        let hex = vec![
            0x71, 0xDF,
            0x31, 0x5E,
            0xD1, 0x9B,
            0x51, 0x1C,
            0xB1, 0x14,
            0x11, 0x7D,
            0xF1, 0x35,
            0x91, 0x5A,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn parse_addr_mode_imm() {
        // Should include all instructions that support immediate addressing mode
        let data: Vec<u8> = assemble("
            ADC #$CA
            AND #$5A
            CMP #$A1
            CPX #$E9
            CPY #$03
            EOR #$FF
            LDA #$9A
            LDX #$AE
            LDY #$DB
            ORA #$1D
            SBC #$E3
        ").unwrap().data;

        let hex: Vec<u8> = vec![
            0x69, 0xCA,
            0x29, 0x5A,
            0xC9, 0xA1,
            0xE0, 0xE9,
            0xC0, 0x03,
            0x49, 0xFF,
            0xA9, 0x9A,
            0xA2, 0xAE,
            0xA0, 0xDB,
            0x09, 0x1D,
            0xE9, 0xE3,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn parse_addr_mode_zpg() {
        let data = assemble("
            ADC $96
            AND $7A
            ASL $FF
            BIT $42
            CMP $CA
            CPX $53
            CPY $0E
            DEC $64
            EOR $6F
            INC $1D
            LDA $F4
            LDX $3D
            LDY $C9
            LSR $ED
            ORA $1C
            ROL $DC
            ROR $18
            SBC $6D
            STA $0C
            STX $6A
            STY $B5
        ").unwrap().data;

        let hex = vec![
            0x65, 0x96,
            0x25, 0x7A,
            0x06, 0xFF,
            0x24, 0x42,
            0xC5, 0xCA,
            0xE4, 0x53,
            0xC4, 0x0E,
            0xC6, 0x64,
            0x45, 0x6F,
            0xE6, 0x1D,
            0xA5, 0xF4,
            0xA6, 0x3D,
            0xA4, 0xC9,
            0x46, 0xED,
            0x05, 0x1C,
            0x26, 0xDC,
            0x66, 0x18,
            0xE5, 0x6D,
            0x85, 0x0C,
            0x86, 0x6A,
            0x84, 0xB5,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn parse_addr_mode_zpg_x() {
        let data = assemble("
            ADC $5F,X
            AND $9A,X
            ASL $2F,X
            CMP $B4,X
            DEC $A5,X
            EOR $E8,X
            INC $99,X
            LDA $C7,X
            LDY $26,X
            LSR $DB,X
            ORA $3D,X
            ROL $9E,X
            ROR $CC,X
            SBC $DE,X
            STA $C0,X
            STY $8E,X
        ").unwrap().data;

        let hex = vec![
            0x75, 0x5F,
            0x35, 0x9A,
            0x16, 0x2F,
            0xD5, 0xB4,
            0xD6, 0xA5,
            0x55, 0xE8,
            0xF6, 0x99,
            0xB5, 0xC7,
            0xB4, 0x26,
            0x56, 0xDB,
            0x15, 0x3D,
            0x36, 0x9E,
            0x76, 0xCC,
            0xF5, 0xDE,
            0x95, 0xC0,
            0x94, 0x8E,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn parse_addr_mode_zpg_y() {
        let data = assemble("
            LDX $B3,Y
            STX $7F,Y
        ").unwrap().data;

        let hex = vec![
            0xB6, 0xB3,
            0x96, 0x7F,
        ];

        assert_eq!(data, hex);
    }

    // for now using hex literals, will test label usage separately
    #[test]
    fn parse_addr_mode_rel() {
        let data = assemble("
            BCC $E3
            BCS $0F
            BEQ $6A
            BMI $C2
            BNE $4B
            BPL $91
            BVC $27
            BVS $D5
        ").unwrap().data;

        let hex = vec![
            0x90, 0xE3, 
            0xB0, 0x0F,
            0xF0, 0x6A,
            0x30, 0xC2,
            0xD0, 0x4B,
            0x10, 0x91,
            0x50, 0x27,
            0x70, 0xD5,
        ];

        assert_eq!(data, hex);
    }

    // Label parsing

    #[test]
    fn dummy_label_1() {
        let data: Vec<u8> = assemble("
            BEGIN ADC #$c4 
            ADC #$1f
        ").unwrap().data;

        let hex = vec![
            0x69, 0xC4,
            0x69, 0x1F,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn dummy_label_2() {

        let data: Vec<u8> = assemble("
            LSR $4283
            what ADC #$1f
        ").unwrap().data;

        let hex = vec![
            0x4E, 0x83, 0x42,
            0x69, 0x1F
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn jmp_label_a_backward() {
        let data: Vec<u8> = assemble("
            NOP ; putting this here so automatically setting the addr to 0x0000 doesn't pass
            BEGIN LDA #$ff
            ADC #$c4
            JMP BEGIN
        ").unwrap().data;

        let hex = vec![
            0xEA,
            0xA9, 0xFF,
            0x69, 0xC4,
            0x4C, 0x01, 0x00,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn jmp_label_a_forward() {
        let data: Vec<u8> = assemble("
            NOP
            JMP END
            ADC #$c4
            END LDA #$FF
        ").unwrap().data;

        let hex = vec![
            0xEA,
            0x4C, 0x06, 0x00,
            0x69, 0xC4, 
            0xA9, 0xFF,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn jmp_label_a_comments() {

        let data: Vec<u8> = assemble("
            NOP
            START LDA #$01 ; This is the start
            JMP START
        ").unwrap().data;

        let hex = vec![
            0xEA,
            0xA9, 0x01,
            0x4C, 0x01, 0x00,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn jmp_label_multiple() {
        let data: Vec<u8> = assemble("
            NOP
            LABEL1  LSR $AC82,X
                    ADC #$11
                    LDA ($05,X)
                    JMP LABEL2
                    LDA ($10),Y
            LABEL2  ADC $1234
                    LDX #$3F
                    JMP LABEL1
        ").unwrap().data;

        let hex: Vec<u8> = vec![
            0xEA,
            0x5E, 0x82, 0xAC, 
            0x69, 0x11, 
            0xA1, 0x05, 
            0x4C, 0x0D, 0x00,
            0xB1, 0x10,
            0x6D, 0x34, 0x12,
            0xA2, 0x3F,
            0x4C, 0x01, 0x00,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn bne_label_relative_forward() {
        let data = assemble("
            ADC $1234
            BNE LABEL
            ADC $1234
            LABEL LDA #$08
        ").unwrap().data;

        let hex = vec![
            0x6D, 0x34, 0x12,
            0xD0, 0x03, // important part here is the 3, relative offset
            0x6D, 0x34, 0x12,
            0xA9, 0x08,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn bne_label_relative_backward() {
        let data = assemble("
                ADC $1234
        LABEL   LDA #$08
                ADC $1234
                BNE LABEL
        ").unwrap().data;

        let hex = vec![
            0x6D, 0x34, 0x12,
            0xA9, 0x08,
            0x6D, 0x34, 0x12,
            0xD0, 0xF9,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn hanging_literal_fail() {
        let res = assemble("
            LSR $10
            $FF
            ADC $1293
        ");

        if let Ok(_) = res {
            panic!("Assemble succeeded when it should not have");
        }
    }

    #[test]
    fn constant_symbol_definition() {
        let data = assemble("
            VALUE = $38 
            LSR $FF
        ").unwrap().data;

        let hex = vec![
            0x46, 0xFF,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn constant_symbol_hex() {
        let data = assemble("
            ONE_BYTE_HEX = $AC
            TWO_BYTE_HEX = $182F

            ADC ONE_BYTE_HEX      ; zpg
            ADC TWO_BYTE_HEX      ; abs
            ADC #ONE_BYTE_HEX     ; imm
            ADC ONE_BYTE_HEX,X    ; zpg,x
            ADC TWO_BYTE_HEX,X    ; abs,x
            ADC TWO_BYTE_HEX,Y    ; abs,y
            ADC (ONE_BYTE_HEX,X)  ; Xind 
            ADC (ONE_BYTE_HEX),Y  ; indY
        ").unwrap().data;

        let hex = vec![
            0x65, 0xAC,
            0x6D, 0x2F, 0x18,
            0x69, 0xAC, 
            0x75, 0xAC,
            0x7D, 0x2F, 0x18,
            0x79, 0x2F, 0x18,
            0x61, 0xAC,
            0x71, 0xAC,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn constant_symbol_dec() {
        let data = assemble("
            ONE_BYTE_DEC = 255
            TWO_BYTE_DEC = 4000

            ADC ONE_BYTE_DEC      ; zpg
            ADC TWO_BYTE_DEC      ; abs
            ADC #ONE_BYTE_DEC     ; imm
            ADC ONE_BYTE_DEC,X    ; zpg,x
            ADC TWO_BYTE_DEC,X    ; abs,x
            ADC TWO_BYTE_DEC,Y    ; abs,y
            ADC (ONE_BYTE_DEC,X)  ; Xind 
            ADC (ONE_BYTE_DEC),Y  ; indY
        ").unwrap().data;

        let hex = vec![
            0x65, 0xFF,
            0x6D, 0xA0, 0x0F,
            0x69, 0xFF, 
            0x75, 0xFF,
            0x7D, 0xA0, 0x0F,
            0x79, 0xA0, 0x0F,
            0x61, 0xFF,
            0x71, 0xFF,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn constant_symbol_bin() {
        let data = assemble("
            ONE_BYTE_BIN = %11111111
            TWO_BYTE_BIN = %111110100000

            ADC ONE_BYTE_BIN      ; zpg
            ADC TWO_BYTE_BIN      ; abs
            ADC #ONE_BYTE_BIN     ; imm
            ADC ONE_BYTE_BIN,X    ; zpg,x
            ADC TWO_BYTE_BIN,X    ; abs,x
            ADC TWO_BYTE_BIN,Y    ; abs,y
            ADC (ONE_BYTE_BIN,X)  ; Xind 
            ADC (ONE_BYTE_BIN),Y  ; indY
        ").unwrap().data;

        let hex = vec![
            0x65, 0xFF,
            0x6D, 0xA0, 0x0F,
            0x69, 0xFF, 
            0x75, 0xFF,
            0x7D, 0xA0, 0x0F,
            0x79, 0xA0, 0x0F,
            0x61, 0xFF,
            0x71, 0xFF,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn symbols_with_jmp() {
        let data = assemble("
                    ONE_BYTE_HEX = $AC
                    TWO_BYTE_HEX = $182F

                    ADC ONE_BYTE_HEX      ; zpg
                    ADC TWO_BYTE_HEX      ; abs
                    ADC #ONE_BYTE_HEX     ; imm
                    JMP LABEL
                    ADC ONE_BYTE_HEX,X    ; zpg,x
                    ADC TWO_BYTE_HEX,X    ; abs,x
            LABEL   ADC TWO_BYTE_HEX,Y    ; abs,y
                    ADC (ONE_BYTE_HEX,X)  ; Xind 
                    ADC (ONE_BYTE_HEX),Y  ; indY
        ").unwrap().data;

        let hex = vec![
            0x65, 0xAC,
            0x6D, 0x2F, 0x18,
            0x69, 0xAC,
            0x4C, 0x0F, 0x00, 
            0x75, 0xAC,
            0x7D, 0x2F, 0x18,
            0x79, 0x2F, 0x18,
            0x61, 0xAC,
            0x71, 0xAC,
        ];

        assert_eq!(data, hex); 
    }

    #[test]
    fn symbols_with_branching() {
        let data = assemble("
                    ONE_BYTE_HEX = $AC
                    TWO_BYTE_HEX = $182F

                    ADC ONE_BYTE_HEX      ; zpg
                    ADC TWO_BYTE_HEX      ; abs
                    ADC #ONE_BYTE_HEX     ; imm
                    BNE LABEL
                    ADC ONE_BYTE_HEX,X    ; zpg,x
                    ADC TWO_BYTE_HEX,X    ; abs,x
            LABEL   ADC TWO_BYTE_HEX,Y    ; abs,y
                    ADC (ONE_BYTE_HEX,X)  ; Xind 
                    ADC (ONE_BYTE_HEX),Y  ; indY
        ").unwrap().data;

        let hex = vec![
            0x65, 0xAC,
            0x6D, 0x2F, 0x18,
            0x69, 0xAC,
            0xD0, 0x05, 
            0x75, 0xAC,
            0x7D, 0x2F, 0x18,
            0x79, 0x2F, 0x18,
            0x61, 0xAC,
            0x71, 0xAC,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn symbol_refers_to_symbol() {
        let data = assemble("
            VAR1 = VAR2
            VAR2 = $FF

            ADC VAR1
        ").unwrap().data;

        let hex = vec![
            0x65, 0xFF
        ];

        assert_eq!(data, hex);
    }

    // rename this when brain is juiced up
    #[test]
    fn test_1() {
        let res = assemble("
            s = \"A=\"
        ");

        if let Ok(_) = res {
            panic!("Assemble succeeded when it should not have");
        }
    }

    // Resolve symbols

    /*#[test]
    fn resolve_constants() {
        let mut symbols: HashMap<String, Operand> = HashMap::new();

        let operand = Operand::Raw(vec![0x3F, 0xD4]);

        symbols.insert("VAR1".to_string(), operand.clone());
        symbols.insert("VAR2".to_string(), Operand::Symbol("VAR1".to_string()));

        resolve_symbols(&mut symbols).unwrap();

        assert_eq!(*symbols.get("VAR1").unwrap(), operand);
        assert_eq!(*symbols.get("VAR2").unwrap(), operand);
    }*/

}
