use std::collections::HashMap;
use std::collections::VecDeque;
use std::fs::File;
use std::io::{self, Read, Write};

use anyhow::Error;
use lazy_static::lazy_static;

use anyhow::Result;
use anyhow::bail;

use regex::Regex;

lazy_static! {
    static ref OPCODES: HashMap<String, Vec<u8>> = {
        let mut map = HashMap::new();

        map.insert(
            "ADC".to_string(),
            vec![
                0x00, 0x6d, 0x7d, 0x79, 0x69, 0x00, 0x00, 0x61, 0x71, 0x00, 0x65, 0x75, 0x00,
            ],
        );

        map.insert(
            "AND".to_string(),
            vec![
                0x00, 0x2d, 0x3d, 0x39, 0x29, 0x00, 0x00, 0x21, 0x31, 0x00, 0x25, 0x35, 0x00,
            ],
        );

        map.insert(
            "ASL".to_string(),
            vec![
                0x0a, 0x0e, 0x1e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x16, 0x00,
            ],
        );

        map.insert(
            "BCC".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x90, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "BCS".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xb0, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "BEQ".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "BIT".to_string(),
            vec![
                0x00, 0x2c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x24, 0x00, 0x00,
            ],
        );

        map.insert(
            "BMI".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "BNE".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xd0, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "BPL".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "BRK".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "BVC".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x50, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "BVS".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x70, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "CLC".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "CLD".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0xd8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "CLI".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x58, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "CLV".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "CMP".to_string(),
            vec![
                0x00, 0xcd, 0xdd, 0xd9, 0xc9, 0x00, 0x00, 0xc1, 0xd1, 0x00, 0xc5, 0xd5, 0x00,
            ],
        );

        map.insert(
            "CPX".to_string(),
            vec![
                0x00, 0xec, 0x00, 0x00, 0xe0, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe4, 0x00, 0x00,
            ],
        );

        map.insert(
            "CPY".to_string(),
            vec![
                0x00, 0xcc, 0x00, 0x00, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc4, 0x00, 0x00,
            ],
        );

        map.insert(
            "DEC".to_string(),
            vec![
                0x00, 0xce, 0xde, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc6, 0xd6, 0x00,
            ],
        );

        map.insert(
            "DEX".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0xca, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "DEY".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x88, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "EOR".to_string(),
            vec![
                0x00, 0x4d, 0x5d, 0x59, 0x49, 0x00, 0x00, 0x41, 0x51, 0x00, 0x45, 0x55, 0x4d,
            ],
        );

        map.insert(
            "INC".to_string(),
            vec![
                0x00, 0xee, 0xfe, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe6, 0xf6, 0x00,
            ],
        );

        map.insert(
            "INX".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0xe8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "INY".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0xc8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "JMP".to_string(),
            vec![
                0x00, 0x4c, 0x00, 0x00, 0x00, 0x00, 0x6c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "JSR".to_string(),
            vec![
                0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "LDA".to_string(),
            vec![
                0x00, 0xad, 0xbd, 0xb9, 0xa9, 0x00, 0x00, 0xa1, 0xb1, 0x00, 0xa5, 0xb5, 0x00,
            ],
        );

        map.insert(
            "LDX".to_string(),
            vec![
                0x00, 0xae, 0x00, 0xbe, 0xa2, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa6, 0x00, 0xb6,
            ],
        );

        map.insert(
            "LDY".to_string(),
            vec![
                0x00, 0xac, 0xbc, 0x00, 0xa0, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa4, 0xb4, 0x00,
            ],
        );

        map.insert(
            "LSR".to_string(),
            vec![
                0x4a, 0x4e, 0x5e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46, 0x56, 0x00,
            ],
        );
        map.insert(
            "NOP".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0xea, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "ORA".to_string(),
            vec![
                0x00, 0x0d, 0x1d, 0x19, 0x09, 0x00, 0x00, 0x01, 0x11, 0x00, 0x05, 0x15, 0x00,
            ],
        );

        map.insert(
            "PHA".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x48, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "PHP".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "PLA".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x68, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "PLP".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x28, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "ROL".to_string(),
            vec![
                0x2a, 0x2e, 0x3e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x26, 0x36, 0x00,
            ],
        );

        map.insert(
            "ROR".to_string(),
            vec![
                0x6a, 0x6e, 0x7e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x66, 0x76, 0x00,
            ],
        );

        map.insert(
            "RTI".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "RTS".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x60, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "SBC".to_string(),
            vec![
                0x00, 0xed, 0xfd, 0xf9, 0xE9, 0x00, 0x00, 0xe1, 0xf1, 0x00, 0xe5, 0xf5, 0x00,
            ],
        );

        map.insert(
            "SEC".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x38, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "SED".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "SEI".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "STA".to_string(),
            vec![
                0x00, 0x8d, 0x9d, 0x99, 0x00, 0x00, 0x00, 0x81, 0x91, 0x00, 0x85, 0x95, 0x00,
            ],
        );

        map.insert(
            "STX".to_string(),
            vec![
                0x00, 0x8e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x86, 0x00, 0x96,
            ],
        );

        map.insert(
            "STY".to_string(),
            vec![
                0x00, 0x8c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x84, 0x94, 0x00,
            ],
        );

        map.insert(
            "TAX".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "TAY".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0xa8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "TSX".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0xba, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "TXA".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x8a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "TXS".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x9a, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map.insert(
            "TYA".to_string(),
            vec![
                0x00, 0x00, 0x00, 0x00, 0x00, 0x98, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ],
        );

        map
    };
    static ref RE_TOKEN: Regex =
        Regex::new(r"(;[^\n\r]*|(\$[0-9a-fA-F]+|%[01]+|\b\d)|(\w+)|([^\w\s]))").unwrap();
    static ref RE_WORD: Regex = Regex::new(r"^\w+$").unwrap();
    static ref RE_NUM_LITERAL: Regex = Regex::new(r"(\$[0-9a-fA-F]+|%[01]+|\b\d)").unwrap();
    static ref RE_COMMENT: Regex = Regex::new(r";[^\n\r]*").unwrap();
}

pub struct Config {
    pub input: String,
    pub output: String,
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &str> {
        if args.len() < 3 {
            return Err("not enough arguments");
        }

        let input = args[1].clone();
        let output = args[2].clone();

        Ok(Config { input, output })
    }
}

#[derive(Copy, Clone, Debug)]
enum AddressMode {
    A,    // accumulator
    Abs,  // absolute
    AbsX, // absolute, X-indexed
    AbsY, // absolute, Y-indexed
    Imm,  // immediate
    Imp,  // implied
    Ind,  // indirect
    XInd, // x-indexed, indirect
    IndY, // indirect, Y-indexed
    Rel,  // relative
    Zpg,  // zeropage
    ZpgX, // zeropage, X-indexed
    ZpgY, // zeropage, Y-indexed
}

struct Instruction {
    name: String,
    mode: AddressMode,
    operand: Vec<u8>,      // zero, one or two item u8 vec
    label: Option<String>, // if the instruction references a label
}

fn tokenize<'h>(src: &'h str) -> impl Iterator<Item = &'h str> + 'h {
    RE_TOKEN.find_iter(src).into_iter().map(|m| m.as_str())
}

fn parse_num_literal(literal: &str) -> Result<Vec<u8>> {
    if literal.len() == 0 {
        bail!("Could not parse literal: {}", literal);
    }
    match literal.chars().next().unwrap() {
        '$' => hex::decode(&literal[1..]).map_err(|e| Error::new(e)),
        _ => bail!("Could not parse literal: {}", literal),
    }
}

pub fn assemble(src: &str) -> Result<Vec<u8>> {
    let mut data: Vec<u8> = vec![];
    let mut instructions: Vec<Instruction> = vec![];

    let mut symbols: HashMap<String, Vec<u8>> = HashMap::new();
    let mut pc = 0x0000; // TODO: allow for this to be set by directive at beginning of src

    for (line_num, line) in src.lines().enumerate() {
        let mut tokens: VecDeque<&str> = tokenize(line).collect();

        // remove comments from end of line
        if !tokens.is_empty() && RE_COMMENT.is_match(&tokens[tokens.len() - 1]) {
            tokens.pop_back();
        }

        // pop labels
        while tokens.len() > 0 && !OPCODES.contains_key(&tokens[0].to_uppercase()) {
            if RE_WORD.is_match(&tokens[0]) {
                if tokens.len() > 2 && tokens[1] == "=" {
                    let operand = tokens[2];
                    if RE_NUM_LITERAL.is_match(operand) {
                        symbols.insert(String::from(tokens.pop_front().unwrap()), parse_num_literal(operand)?);
                    } else if RE_WORD.is_match(operand) {
                        bail!("Unsupported");
                    }
                    tokens.pop_front();
                    tokens.pop_front();
                } else {
                    symbols.insert(String::from(tokens.pop_front().unwrap()), vec![pc / 16, pc % 16]);
                }
            } else {
                bail!("Failed to parse line {}: {}", line_num, line);
            }
        }

        // line could potentially just be labels/comments
        if tokens.is_empty() {
            continue;
        }

        // after removing labels/comments first token should always be the instruction name
        if !OPCODES.contains_key(&tokens[0].to_uppercase()) {
            bail!("Failed to parse line {}: {}", line_num, line);
        }

        let mut inst = Instruction {
            name: String::from(&tokens[0].to_uppercase()),
            mode: AddressMode::Imp,
            operand: vec![],
            label: None,
        };

        match tokens.len() {
            1 => { /* taken care of by default value for inst */ } // AddressMode::Imp
            2 => { // AddressMode::{A, Zpg, Rel, Abs}
                if tokens[1].to_uppercase() == "A" {
                    inst.mode = AddressMode::A;
                } else {
                    if RE_NUM_LITERAL.is_match(&tokens[1]) {
                        inst.operand = parse_num_literal(&tokens[1]).unwrap();
                    } else if RE_WORD.is_match(&tokens[1]) {
                        inst.label = Some(String::from(tokens[1]));
                    }

                    match &tokens[0].to_uppercase()[..] {
                        "BCC" | "BCS" | "BEQ" | "BMI" | "BNE" | "BPL" | "BVC" | "BVS" => {
                            inst.mode = AddressMode::Rel;
                        }
                        _ => match inst.operand.len() {
                            0 if inst.label != None => inst.mode = AddressMode::Abs,
                            1 => inst.mode = AddressMode::Zpg,
                            2 => inst.mode = AddressMode::Abs,
                            _ => bail!(
                                "Could not determine addressing mode (line {}): {}",
                                line_num,
                                line
                            ),
                        },
                    }
                }
            }
            3 if tokens[1] == "#" => { // AddressMode::Imm
                inst.mode = AddressMode::Imm;
                inst.operand = parse_num_literal(&tokens[2]).unwrap();
            }
            4 => { // AddressMode::{ZpgX, ZpgY, AbsX, AbsY, Ind}
                if tokens[1] == "(" && tokens[3] == ")" {
                    inst.mode = AddressMode::Ind;
                    if RE_NUM_LITERAL.is_match(&tokens[2]) {
                        inst.operand = parse_num_literal(&tokens[2]).unwrap();
                    } else if RE_WORD.is_match(&tokens[2]) {
                        inst.label = Some(String::from(tokens[1]));
                    } else {
                        bail!(
                            "Could not determine addressing mode (line {}): {}",
                            line_num,
                            line
                        );
                    }
                } else if tokens[2] == "," {
                    if RE_NUM_LITERAL.is_match(&tokens[1]) {
                        inst.operand = parse_num_literal(&tokens[1]).unwrap();
                        inst.mode = match &tokens[3].to_uppercase()[..] {
                            "X" => match inst.operand.len() {
                                1 => AddressMode::ZpgX,
                                2 => AddressMode::AbsX,
                                _ => bail!(
                                    "Could not determine addressing mode (line {}): {}",
                                    line_num,
                                    line
                                ),
                            },
                            "Y" => match inst.operand.len() {
                                1 => AddressMode::ZpgY,
                                2 => AddressMode::AbsY,
                                _ => bail!(
                                    "Could not determine addressing mode (line {}): {}",
                                    line_num,
                                    line
                                ),
                            },
                            _ => bail!(
                                "Could not determine addressing mode (line {}): {}",
                                line_num,
                                line
                            ),
                        };
                    } else if RE_WORD.is_match(&tokens[1]) {
                        inst.mode = match &tokens[3].to_uppercase()[..] {
                            "X" => AddressMode::AbsX,
                            "Y" => AddressMode::AbsY,
                            _ => bail!("eou"),
                        };
                        inst.label = Some(String::from(tokens[1]));
                    } else {
                        bail!(
                            "Could not determine addressing mode (line {}): {}",
                            line_num,
                            line
                        );
                    }
                } else {
                    bail!(
                        "Could not determine addressing mode (line {}): {}",
                        line_num,
                        line
                    );
                }
            }
            6 => { // AddressMode::{XInd, IndY}
                if tokens[1] == "("
                    && tokens[3] == ","
                    && tokens[4].to_uppercase() == "X"
                    && tokens[5] == ")"
                {
                    inst.mode = AddressMode::XInd;
                    if RE_NUM_LITERAL.is_match(&tokens[2]) {
                        inst.operand = parse_num_literal(&tokens[2]).unwrap();
                    } else if RE_WORD.is_match(&tokens[2]) {
                        inst.label = Some(String::from(tokens[2]));
                    }
                } else if tokens[1] == "("
                    && tokens[3] == ")"
                    && tokens[4] == ","
                    && tokens[5].to_uppercase() == "Y"
                {
                    inst.mode = AddressMode::IndY;
                    if RE_NUM_LITERAL.is_match(&tokens[2]) {
                        inst.operand = parse_num_literal(&tokens[2]).unwrap();
                    } else if RE_WORD.is_match(&tokens[2]) {
                        inst.label = Some(String::from(tokens[2]));
                    }
                } else {
                    bail!(
                        "Could not determine addressing mode (line {}): {}",
                        line_num,
                        line
                    );
                }
            }
            _ => bail!(
                "Could not determine addressing mode (line {}): {}",
                line_num,
                line
            ),
        }

        pc += 1;
        pc += match inst.mode {
            AddressMode::Abs | AddressMode::AbsX | AddressMode::AbsY | AddressMode::Ind => 2,
            AddressMode::Zpg
            | AddressMode::ZpgX
            | AddressMode::ZpgY
            | AddressMode::Imm
            | AddressMode::XInd
            | AddressMode::IndY
            | AddressMode::Rel => 1,
            _ => 0,
        };

        instructions.push(inst);
    }

    for mut inst in instructions {
        let opcode = OPCODES.get(&inst.name).unwrap()[inst.mode as usize];

        if let Some(l) = inst.label {
            if let Some(symbol_value) = symbols.get(&l) {
                match inst.mode {
                    AddressMode::Rel => {
                        let rel_addr: i8 = (((symbol_value[1] + symbol_value[0] * 16) as i16) - (data.len() as i16 + 2))
                            .try_into()
                            .unwrap();
                        inst.operand = vec![rel_addr as u8];
                    }
                    _ => inst.operand = symbol_value.to_vec(),
                }
            } else {
                bail!("Undefined label: {}", l);
            }
        }

        data.push(opcode);
        for byte in inst.operand.into_iter().rev() {
            data.push(byte);
        }
    }

    Ok(data)
}

pub fn assemble_from_file(config: Config) -> Result<Vec<u8>> {
    let input_file = File::open(config.input)?;
    let mut b = io::BufReader::new(input_file);

    let mut src: String = String::new();

    if let Err(e) = b.read_to_string(&mut src) {
        bail!(e);
    }

    match assemble(&src) {
        Ok(data) => {
            let mut output_file = File::create(config.output)?;
            output_file.write_all(&data[..])?;

            Ok(data)
        }
        Err(e) => bail!(e),
    }
}

#[rustfmt::skip]
#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
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
    }

    // Tokenization
    #[test]
    fn tokenize_imp() {
        let tokens: Vec<_> = tokenize("BRK").collect();

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], "BRK");
    }

    #[test]
    fn tokenize_acc() {
        let tokens: Vec<_> = tokenize("LSR A").collect();

        assert_eq!(tokens.len(), 2);

        assert_eq!(tokens[0], "LSR");
        assert_eq!(tokens[1], "A");
    }

    #[test]
    fn tokenize_comment() {
        let src = "LSR A;here's a comment! awfully close there...";
        let tokens: Vec<_> = tokenize(src).collect();


        assert_eq!(tokens, vec![
            "LSR", "A", ";here's a comment! awfully close there..."
        ]);
    }

    #[test]
    fn tokenize_abs() {
        let src = "LDA $3c1d";
        let tokens: Vec<_> = tokenize(src).collect();

        assert_eq!(tokens, vec![
            "LDA", "$3c1d",
        ]);
    }

    #[test]
    fn tokenize_zp_x() {
        let src = "LSR $01,X";
        let tokens: Vec<_> = tokenize(src).collect();

        assert_eq!(tokens, vec![
            "LSR", "$01", ",", "X",
        ]);
    }

    // Instruction parsing

    #[test]
    fn parse_addr_mode_a() {
        // Should include all instructions that support accumulator addressing mode
        let data: Vec<u8> = assemble("
            ASL A
            LSR A
            ROL A
            ROR A
        ").unwrap();
        
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
        ").unwrap();

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
            
        ").unwrap();

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
        ").unwrap();

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
        ").unwrap();

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
        ").unwrap();

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
        ").unwrap();

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
        ").unwrap();

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
        ").unwrap();

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
        ").unwrap();

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
        ").unwrap();

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
        ").unwrap();

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
        ").unwrap();

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
        let src = "
            BEGIN ADC #$c4 
            ADC #$1f
        ";

        let data: Vec<u8> = assemble(src).unwrap();

        assert_eq!(data.len(), 4);

        assert_eq!(data[0], 0x69);
        assert_eq!(data[1], 0xc4);
        assert_eq!(data[2], 0x69);
        assert_eq!(data[3], 0x1f);
    }

    #[test]
    fn dummy_label_2() {
        let src = "
            LSR $4283
            what ADC #$1f
        "; // lowercase labels should also work

        let data: Vec<u8> = assemble(src).unwrap();

        assert_eq!(data.len(), 5);

        assert_eq!(data[0], 0x4E);
        assert_eq!(data[1], 0x83);
        assert_eq!(data[2], 0x42);
        assert_eq!(data[3], 0x69);
        assert_eq!(data[4], 0x1F);
    }

    #[test]
    fn jmp_label_a_backward() {
        let src = "
            BEGIN LDA #$ff
            ADC #$c4
            JMP BEGIN
        ";

        let data: Vec<u8> = assemble(src).unwrap();

        let hex = vec![
            0xA9, 0xFF,
            0x69, 0xC4,
            0x4C, 0x00, 0x00,
        ];

        assert_eq!(data, hex);
    }

    #[test]
    fn jmp_label_a_forward() {
        let src = "
            JMP END
            ADC #$c4
            END LDA #$FF
        ";

        let data: Vec<u8> = assemble(src).unwrap();

        assert_eq!(data.len(), 7);

        assert_eq!(data[0], 0x4c);
        assert_eq!(data[1], 0x05);
        assert_eq!(data[2], 0x00);
        assert_eq!(data[3], 0x69);
        assert_eq!(data[4], 0xc4);
        assert_eq!(data[5], 0xa9);
        assert_eq!(data[6], 0xff);
    }

    #[test]
    fn jmp_label_a_comments() {
        let src = "
            START LDA #$01 ; This is the start
            JMP START
        ";

        let data: Vec<u8> = assemble(src).unwrap();

        assert_eq!(data.len(), 5);

        assert_eq!(data[0], 0xA9);
        assert_eq!(data[1], 0x01);
        assert_eq!(data[2], 0x4C);
        assert_eq!(data[3], 0x00);
        assert_eq!(data[4], 0x00);
    }

    #[test]
    fn jmp_label_multiple() {
        let src = "
            LABEL1  LSR $AC82,X
                    ADC #$11
                    LDA ($05,X)
                    JMP LABEL2
                    LDA ($10),Y
            LABEL2  ADC $1234
                    LDX #$3F
                    JMP LABEL1
        ";

        let data: Vec<u8> = assemble(src).unwrap();

        let hex: Vec<u8> = vec![
            0x5E, 0x82, 0xAC, 
            0x69, 0x11, 
            0xA1, 0x05, 
            0x4C, 0x0C, 0x00,
            0xB1, 0x10,
            0x6D, 0x34, 0x12,
            0xA2, 0x3F,
            0x4C, 0x00, 0x00,
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
        ").unwrap();

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
        ").unwrap();

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
        ").unwrap();

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
        ").unwrap();

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
}
