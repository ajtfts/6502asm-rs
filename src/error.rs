use thiserror::Error;

#[derive(Error, Debug)]
pub enum AsmError {
    #[error("Invalid syntax (line {line_num}): {line}")]
    InvalidSyntax {
        line_num: usize,
        line: String,
    },
    #[error("Operand too large (line {line_num}): {line}")]
    OperandTooLarge {
        line_num: usize,
        line: String,
        operand: usize,
    },
    #[error("Unresolved symbol: {0}")]
    UnresolvedSymbol(String),
    #[error("Undefined symbol: {0}")]
    UndefinedSymbol(String),
    #[error("Exceeded maximum number of passes ({0}): {1}")]
    TooManyPasses(usize, usize),
    #[error("Unknown error")]
    Unknown
}