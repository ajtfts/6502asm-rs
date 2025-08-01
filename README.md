# 6502asm-rs: a 6502 assembler written in Rust

This is a small personal project. The goal is to achieve syntax compatibility with [64tass](https://sourceforge.net/projects/tass64/), as well to implement its more important features. Here's a very non-exhaustive list of features that still need to be implemented:

- [ ] Adjustment of program counter/compile offset via "*="
- [ ] Non hex data-types
	- [x] binary strings (using "%")
	- [x] decimal
	- [ ] characters (delimited by single quotes)
- [ ] Storing values using directives like .byte, .char, etc.
- [ ] Support for 6502 illegal opcodes
- [ ] Output of assembly listing to file using -L option
- [ ] Output of symbols listing to file using -l option