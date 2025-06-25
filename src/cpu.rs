// https://en.wikibooks.org/wiki/6502_Assembly

#[allow(unused_variables)]
pub const OPCODES: [fn(&mut CPU) -> (); 256] = [
    // 00
    |cpu| println!("should be 00"),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // 10
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // 20
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // 30
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // 40
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // 50
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // 60
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // 69: ADC, imm
    |cpu| {
        let n: u8 = cpu.read_pc_byte();
        cpu.reg.a += n;
    },
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // 70
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // 80
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // 8D: LDA, a
    |cpu| {
        let addr: u16 = cpu.read_pc_byte() as u16 + ((cpu.read_pc_byte() as u16) << 8);
        cpu.ram[(addr - cpu.config.ram_offset) as usize] = cpu.reg.a;
    },
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // 90
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // A0
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // A9: LDA imm
    |cpu| {
        let n: u8 = cpu.read_pc_byte();
        cpu.reg.a = n;
    },
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // B0
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // C0
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // D0
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // E0
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    // F0
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
    |cpu| std::todo!(),
];

struct Registers {
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    s: u8,
    p: u8,
}

struct Flag {
    neg: bool,
    overflow: bool,
    brk: bool,
    decimal: bool,
    disable_intrpt: bool,
    zero: bool,
    carry: bool,
}

#[derive(Clone, Copy)]
pub struct Config {
    prg_offset: u16,
    ram_offset: u16,
}

pub struct CPU<'a> {
    prg: &'a [u8],
    reg: Registers,
    flag: Flag,
    ram: &'a mut [u8],
    config: Config,
}

impl<'a> CPU<'a> {
    pub fn new(prg: &'a [u8], ram: &'a mut [u8], config: Config) -> Self {
        CPU {
            prg: prg,
            config: config,
            reg: Registers {
                a: 0,
                x: 0,
                y: 0,
                pc: config.prg_offset,
                s: 0,
                p: 0,
            },
            flag: Flag {
                neg: true,
                overflow: false,
                brk: false,
                decimal: false,
                disable_intrpt: false,
                zero: true,
                carry: false,
            },
            ram: ram,
        }
    }

    fn read_pc_byte(&mut self) -> u8 {
        let opcode = self.prg[(self.reg.pc - self.config.prg_offset) as usize];
        self.reg.pc += 1;
        opcode
    }

    fn step(&mut self) {
        let opcode = self.read_pc_byte();
        OPCODES[opcode as usize](self);
    }

    pub fn run(&mut self) {
        while usize::from(self.reg.pc - self.config.prg_offset) < self.prg.len() {
            self.step();
        }
    }
}

mod tests {
    use super::*;
    use crate::assemble;

    static NES_CONFIG: Config = Config {
        prg_offset: 0x8000,
        ram_offset: 0x0200,
    };

    #[test]
    fn lda_imm_acc() {
        let prg: Vec<u8> = assemble::assemble_from_str(
            "
            LDA #$F1
        ",
        );

        let mut ram: [u8; 2048] = [0; 2048];
        let mut cpu: CPU = CPU::new(&prg, &mut ram, NES_CONFIG);
        cpu.run();

        assert_eq!(cpu.reg.a, 0xF1);
    }

    #[test]
    fn lda_imm_flag_neg() {
        let prg: Vec<u8> = assemble::assemble_from_str(
            "
            LDA #$03
            LDA #$80
            LDA #$7F
            LDA #$FA
        ",
        );

        let mut ram: [u8; 2048] = [0; 2048];
        let mut cpu: CPU = CPU::new(&prg, &mut ram, NES_CONFIG);

        cpu.step();
        assert_eq!(cpu.reg.a, 0x03);
        assert_eq!(cpu.flag.neg, false);
        cpu.step();
        assert_eq!(cpu.reg.a, 0x80);
        assert_eq!(cpu.flag.neg, true);
        cpu.step();
        assert_eq!(cpu.reg.a, 0x7F);
        assert_eq!(cpu.flag.neg, false);
        cpu.step();
        assert_eq!(cpu.reg.a, 0xFA);
        assert_eq!(cpu.flag.neg, true);
    }

    #[test]
    fn adc_imm_acc() {
        let prg: Vec<u8> = assemble::assemble_from_str(
            "
            ADC #$12
            ADC #$14
        ",
        );

        let mut ram: [u8; 2048] = [0; 2048];
        let mut cpu: CPU = CPU::new(&prg, &mut ram, NES_CONFIG);
        cpu.run();

        assert_eq!(cpu.reg.a, 0x26);
    }

    #[test]
    fn adc_imm_flag() {
        let prg: Vec<u8> = assemble::assemble_from_str(
            "
            
        ",
        );
    }

    #[test]
    fn sta_a() {
        let prg: Vec<u8> = assemble::assemble_from_str(
            "
            ADC #$89
            STA $0200
        ",
        );

        let mut ram: [u8; 2048] = [0; 2048];

        let mut cpu: CPU = CPU::new(&prg, &mut ram, NES_CONFIG);
        cpu.run();

        assert_eq!(ram[0], 0x89);
    }
}
