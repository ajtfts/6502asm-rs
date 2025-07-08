use std::collections::HashMap;
use std::collections::VecDeque;
use std::fs::File;
use std::io::{self, Write, Read};

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
                0x00, 0xed, 0xfd, 0xf9, 0x00, 0x00, 0x00, 0xe1, 0xf1, 0x00, 0xe5, 0xf5, 0x00,
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
    static ref RE_TOKEN: Regex = Regex::new(r"((\w+)|;[^\n\r]*|[^\w\s])").unwrap();
    static ref RE_WORD: Regex = Regex::new(r"^\w+$").unwrap();
    static ref RE_HEX_ONE_BYTE: Regex = Regex::new(r"^[0-9a-fA-F]{2}$").unwrap();
    static ref RE_HEX_TWO_BYTE: Regex = Regex::new(r"^[0-9a-fA-F]{4}$").unwrap();
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

pub fn assemble(src: &str) -> Result<Vec<u8>> {
    let mut data: Vec<u8> = vec![];
    let mut instructions: Vec<Instruction> = vec![];

    let mut labels: HashMap<String, u16> = HashMap::new();
    let mut cur_address = 0x0000; // TODO: allow for this to be set by directive at beginning of src

    for (line_num, line) in src.lines().enumerate() {
        let mut tokens: VecDeque<&str> = tokenize(line).collect();

        // remove comments from end of line
        if !tokens.is_empty() && RE_COMMENT.is_match(&tokens[tokens.len() - 1]) {
            tokens.pop_back();
        }

        // pop labels
        while tokens.len() > 0 && !OPCODES.contains_key(&tokens[0].to_uppercase()) {
            if RE_WORD.is_match(&tokens[0]) {
                labels.insert(String::from(tokens.pop_front().unwrap()), cur_address);
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
            bail!("Unrecognized opcode on line {}: {}", line_num, line);
        }

        let mut inst = Instruction {
            name: String::from(&tokens[0].to_uppercase()),
            mode: AddressMode::Imp,
            operand: vec![],
            label: None,
        };

        // Determine addressing mode and operand
        match tokens.len() {
            0 => continue,
            1 => { /* taken care of by default value for inst */ }
            2 => {
                if tokens[1].to_uppercase() == "A" {
                    inst.mode = AddressMode::A;
                } else if RE_WORD.is_match(&tokens[1]) {
                    // label
                    inst.mode = AddressMode::Abs;
                    inst.label = Some(String::from(tokens[1]));
                } else {
                    bail!(
                        "Failed to determine addressing mode (line {}): {}",
                        line_num,
                        line
                    );
                }
            }
            3 => {
                if tokens[1] == "$" {
                    inst.operand = hex::decode(tokens[2]).unwrap();
                    if RE_HEX_ONE_BYTE.is_match(&tokens[2][..]) {
                        inst.mode = match &tokens[0][..] {
                            "BCC" | "BCS" | "BEQ" | "BMI" | "BNE" | "BPL" | "BVC" | "BVS" => AddressMode::Rel,
                            _ => AddressMode::Zpg,
                        }
                    } else if RE_HEX_TWO_BYTE.is_match(&tokens[2][..]) {
                        inst.mode = AddressMode::Abs;
                    } else {
                        bail!(
                            "Could not determine addressing mode (line {}): {}",
                            line_num,
                            line
                        );
                    }
                } else {
                    bail!(
                        "could not determine addressing mode (line {}): {}",
                        line_num,
                        line
                    );
                }
            }
            4 => {
                if tokens[1] == "#" {
                    if tokens[2] == "$" {
                        if RE_HEX_ONE_BYTE.is_match(&tokens[3]) {
                            inst.operand = hex::decode(tokens[3]).unwrap();
                            inst.mode = AddressMode::Imm;
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
                } else {
                    bail!(
                        "Could not determine addressing mode (line {}): {}",
                        line_num,
                        line
                    );
                }
            },
            5 => {
                if tokens[1] == "$" && tokens[3] == "," {
                    inst.operand = hex::decode(tokens[2]).unwrap();
                    if RE_HEX_ONE_BYTE.is_match(&tokens[2]) {
                        match &tokens[4][..] {
                            "X" => inst.mode = AddressMode::ZpgX,
                            "Y" => inst.mode = AddressMode::ZpgY,
                            _ => bail!(
                                "Could not determine_addressing mode (line {}): {}",
                                line_num,
                                line
                            ),
                        }
                    } else if RE_HEX_TWO_BYTE.is_match(&tokens[2]) {
                        match &tokens[4][..] {
                            "X" => inst.mode = AddressMode::AbsX,
                            "Y" => inst.mode = AddressMode::AbsY,
                            _ => bail!(
                                "Could not determine addressing mode (line {}): {}",
                                line_num,
                                line
                            ),
                        }
                    } else {
                        bail!(
                            "Could not determine addressing mode (line {}): {}",
                            line_num,
                            line
                        );
                    }
                } else if tokens[1] == "("
                    && tokens[2] == "$"
                    && RE_HEX_TWO_BYTE.is_match(&tokens[1])
                    && tokens[4] == ")" 
                {
                    inst.operand = hex::decode(tokens[1]).unwrap();
                    inst.mode = AddressMode::Ind;
                } else {
                    bail!(
                        "Could not determine addressing mode (line {}): {}",
                        line_num,
                        line
                    );
                }
            },
            7 => {
                if tokens[1] == "(" && tokens[2] == "$" && RE_HEX_ONE_BYTE.is_match(&tokens[3]) {
                    inst.operand = hex::decode(tokens[3]).unwrap();
                    if tokens[4] == "," && tokens[5] == "X" && tokens[6] == ")" {
                        inst.mode = AddressMode::XInd;
                    } else if tokens[4] == ")" && tokens[5] == "," && tokens[6] == "Y" {
                        inst.mode = AddressMode::IndY;
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
            },
            _ => bail!("Failed to parse line {}: {}", line_num, line),
        }

        cur_address += 1;
        cur_address += match inst.mode {
            AddressMode::Abs | AddressMode::AbsX | AddressMode::AbsY 
            | AddressMode::Ind => 2,
            AddressMode::Zpg | AddressMode::ZpgX | AddressMode::ZpgY
            | AddressMode::Imm | AddressMode::XInd | AddressMode::IndY => 1,
            _ => 0,
        };

        instructions.push(inst);
    }

    for mut inst in instructions {
        let opcode = OPCODES.get(&inst.name).unwrap()[inst.mode as usize];
        data.push(opcode);

        if let Some(l) = inst.label {
            if let Some(addr) = labels.get(&l) {
                inst.operand = vec![(addr / 16) as u8, (addr % 16) as u8];
            }
            else {
                bail!("Unrecognized label: {}", l);
            }
        }

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
        },
        Err(e) => bail!(e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn tokenize_acc_1() {
        let src = "LSR A;here's a comment! awfully close there...";
        let tokens: Vec<_> = tokenize(src).collect();

        assert_eq!(tokens.len(), 3);

        assert_eq!(tokens[0], "LSR");
        assert_eq!(tokens[1], "A");
    }

    #[test]
    fn tokenize_abs() {
        let src = "LDA $3c1d";
        let tokens: Vec<_> = tokenize(src).collect();

        assert_eq!(tokens.len(), 3);

        assert_eq!(tokens[0], "LDA");
        assert_eq!(tokens[1], "$");
        assert_eq!(tokens[2], "3c1d");
    }

    #[test]
    fn tokenize_comment_0() {
        let src = "BRK ;    here's a comment";
        let tokens: Vec<_> = tokenize(src).collect();

        assert_eq!(tokens.len(), 2);

        assert_eq!(tokens[0], "BRK");
    }

    #[test]
    fn tokenize_zp_x() {
        let src = "LSR $01,X";
        let tokens: Vec<_> = tokenize(src).collect();

        assert_eq!(tokens.len(), 5);

        assert_eq!(tokens[0], "LSR");
        assert_eq!(tokens[1], "$");
        assert_eq!(tokens[2], "01");
        assert_eq!(tokens[3], ",");
        assert_eq!(tokens[4], "X");
    }

    // Individual instruction parsing

    #[test]
    fn lsr_a() {
        let data: Vec<u8> = assemble("LSR A").unwrap();

        assert_eq!(data.len(), 1);

        assert_eq!(data[0], 0x4a);
    }

    #[test]
    fn lsr_abs() {
        let data: Vec<u8> = assemble("LSR $4283").unwrap();

        assert_eq!(data.len(), 3);

        assert_eq!(data[0], 0x4e);
        assert_eq!(data[1], 0x83); // little endian
        assert_eq!(data[2], 0x42);
    }

    #[test]
    fn lsr_abs_x() {
        let data: Vec<u8> = assemble("LSR $AC82,X").unwrap();

        assert_eq!(data.len(), 3);

        assert_eq!(data[0], 0x5e);
        assert_eq!(data[1], 0x82);
        assert_eq!(data[2], 0xac);
    }

    #[test]
    fn lsr_zp() {
        let data: Vec<u8> = assemble("LSR $af").unwrap();

        assert_eq!(data.len(), 2);

        assert_eq!(data[0], 0x46);
        assert_eq!(data[1], 0xaf);
    }

    #[test]
    fn lsr_zp_x() {
        let data: Vec<u8> = assemble("LSR $01,X").unwrap();

        assert_eq!(data.len(), 2);

        assert_eq!(data[0], 0x56);
        assert_eq!(data[1], 0x01);
    }

    #[test]
    fn adc_i() {
        let data: Vec<u8> = assemble("ADC #$c4").unwrap();

        assert_eq!(data.len(), 2);

        assert_eq!(data[0], 0x69);
        assert_eq!(data[1], 0xc4);
    }

    #[test]
    fn lda_indirect_x() {
        let data: Vec<u8> = assemble("LDA ($05,X)").unwrap();

        assert_eq!(data.len(), 2);

        assert_eq!(data[0], 0xa1);
        assert_eq!(data[1], 0x05);
    }

    #[test]
    fn lda_indirect_y() {
        let data: Vec<u8> = assemble("LDA ($10),Y").unwrap();

        assert_eq!(data.len(), 2);

        assert_eq!(data[0], 0xb1);
        assert_eq!(data[1], 0x10);
    }

    #[test]
    fn bne_relative_hex() {
        let data: Vec<u8> = assemble("BNE $34").unwrap();

        assert_eq!(data[0], 0xD0);
        assert_eq!(data[1], 0x34);
    }

    // Multiline parsing

    #[test]
    fn multiline_adc() {
        let data: Vec<u8> = assemble(
            "
            ADC #$C4
            ADC #$1F
        ",
        )
        .unwrap();

        let hex: Vec<u8> = vec![0x69, 0xC4, 0x69, 0x1F];

        assert_eq!(data, hex);
    }

    #[test]
    fn multiline_long() {}

    // Label testing

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

        assert_eq!(data.len(), 7);

        assert_eq!(data[0], 0xa9);
        assert_eq!(data[1], 0xff);
        assert_eq!(data[2], 0x69);
        assert_eq!(data[3], 0xc4);
        assert_eq!(data[4], 0x4c);
        assert_eq!(data[5], 0x00);
        assert_eq!(data[6], 0x00);
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
    fn bne_label_relative() {
        let src = "
            ADC $1234
            BNE LABEL
            ADC $1234
            LABEL LDA #$08
        ";

        let data: Vec<u8> = assemble(src).unwrap();

        assert_eq!(data.len(), 10);

        assert_eq!(data[5], 0x03);
    }
}
