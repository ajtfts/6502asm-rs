use std::collections::VecDeque;
use crate::error::AsmError;
use crate::{RE_NUM_LITERAL, RE_WORD, parse_num_literal};

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum AddressMode {
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

pub enum Operand {
    Bytes(Vec<u8>),
    Symbol(String),
    Pos(usize), // represents a position in the AsmItem vector
}

pub struct Instruction {
    pub name: String,
    pub mode: AddressMode,
    pub operand: Option<Operand>,
}

pub fn parse_operand(op_str: &str, line_num: usize, line: &str) -> Result<Operand, AsmError> {
    if RE_NUM_LITERAL.is_match(&op_str) {
        Ok(Operand::Bytes(parse_num_literal(op_str).map_err(|_| AsmError::InvalidSyntax { line_num, line: line.to_string() })?))
    } else if RE_WORD.is_match(&op_str) {
        Ok(Operand::Symbol(op_str.to_string()))
    } else {
        Err(AsmError::InvalidSyntax { line_num, line: line.to_string() })
    }
}

pub fn parse_inst(tokens: VecDeque<&str>, line_num: usize, line: &str) -> Result<Instruction, AsmError> {
    let mut inst: Instruction = Instruction {
        name: tokens[0].to_uppercase(),
        mode: AddressMode::Imp,
        operand: None,
    };

    match tokens.len() {
        1 => { /* taken care of by default value for inst */ }, // AddressMode::Imp
        2 => { // AddressMode::{A, Zpg, Rel, Abs}
            if tokens[1].to_uppercase() == "A" {
                inst.mode = AddressMode::A;
            } else {
                inst.operand = Some(parse_operand(tokens[1], line_num, line)?);

                match &tokens[0].to_uppercase()[..] {
                    "BCC" | "BCS" | "BEQ" | "BMI" | "BNE" | "BPL" | "BVC" | "BVS" => {
                        inst.mode = AddressMode::Rel;
                    },
                    _ => {
                        match inst.operand {
                            Some(Operand::Bytes(ref bytes)) => {
                                match bytes.len() {
                                    1 => inst.mode = AddressMode::Zpg,
                                    2 => inst.mode = AddressMode::Abs,
                                    _ => return Err(AsmError::OperandTooLarge { line_num, line: line.to_string(), operand: 0 /* todo */ })
                                }
                            },
                            Some(Operand::Symbol(_)) | Some(Operand::Pos(_)) => inst.mode = AddressMode::Abs,
                            _ => return Err(AsmError::InvalidSyntax { line_num, line: line.to_string() })
                        }
                    }
                }
            }
        },
        3 if tokens[1] == "#" => { // AddressMode::Imm
            inst.mode = AddressMode::Imm;
            inst.operand = Some(parse_operand(tokens[2], line_num, line)?);
        },
        4 => { // AddressMode::{ZpgX, ZpgY, AbsX, AbsY, Ind}
            if tokens[1] == "(" && tokens[3] == ")" {
                inst.mode = AddressMode::Ind;
                inst.operand = Some(parse_operand(tokens[2], line_num, line)?);
            } else if tokens[2] == "," {
                inst.operand = Some(parse_operand(tokens[1], line_num, line)?);
                inst.mode = match inst.operand {
                    Some(Operand::Bytes(ref raw)) => match &tokens[3].to_uppercase()[..] {
                        "X" => match raw.len() {
                            1 => AddressMode::ZpgX,
                            2 => AddressMode::AbsX,
                            _ => return Err(AsmError::InvalidSyntax { line_num, line: line.to_string() }), 
                        },
                        "Y" => match raw.len() {
                            1 => AddressMode::ZpgY,
                            2 => AddressMode::AbsY,
                            _ => return Err(AsmError::InvalidSyntax { line_num, line: line.to_string() }),
                        },
                        _ => return Err(AsmError::InvalidSyntax { line_num, line: line.to_string() }),
                    },
                    Some(Operand::Symbol(ref sym)) => match &tokens[3].to_uppercase()[..] {
                        "X" => AddressMode::AbsX,
                        "Y" => AddressMode::AbsY,
                        _ => return Err(AsmError::InvalidSyntax { line_num, line: line.to_string() }),
                    }
                    _ => return Err(AsmError::InvalidSyntax { line_num, line: line.to_string() })
                };
            } else {
                return Err(AsmError::InvalidSyntax { line_num, line: line.to_string() })
            }
        },
        6 => { // AddressMode::{XInd, IndY}
            if tokens[1] == "("
                && tokens[3] == ","
                && tokens[4].to_uppercase() == "X"
                && tokens[5] == ")"
            {
                inst.mode = AddressMode::XInd;
                inst.operand = Some(parse_operand(tokens[2], line_num, line)?);
            } 
            else if tokens[1] == "("
                && tokens[3] == ")"
                && tokens[4] == ","
                && tokens[5].to_uppercase() == "Y"
            {
                inst.mode = AddressMode::IndY;
                inst.operand = Some(parse_operand(tokens[2], line_num, line)?);
            }
        },
        _ => return Err(AsmError::InvalidSyntax { line_num, line: line.to_string() })
    }

    Ok(inst)
}

#[rustfmt::skip]
#[cfg(test)]
mod tests {
    use crate::*;
    #[test]
    fn test_2() {
        println!("{:?}", tokenize("ADC ONE_BYTE_HEX,X    ; zpg,x"));
        parse_inst(tokenize("ADC ONE_BYTE_HEX,X"), 1, "ADC ONE_BYTE_HEX,X    ; zpg,x").unwrap();
    }
}