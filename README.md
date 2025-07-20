# 6502asm-rs
## 6502 assembler written in Rust

This is a small personal project. The goal is to achieve syntax compatibility with [64tass](https://sourceforge.net/projects/tass64/), as well to implement most of its features. Here's a very non-exhaustive list of important features that still need to be implemented:

- [ ] Adjustment of program counter/compile offset via "*="
- [ ] Non-hex data types
- [ ] Storing values using directives like .byte, .char, etc.
- [ ] Support for 6502 illegal opcodes