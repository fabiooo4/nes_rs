use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    pub static ref CPU_OPCODES: HashMap<u8, Opcode> = HashMap::from([
        (0x00, Opcode::new(Code::BRK, AddressingMode::Implied),),
        (0xaa, Opcode::new(Code::TAX, AddressingMode::Implied),),
        (0xe8, Opcode::new(Code::INX, AddressingMode::Implied),),
        (0xa9, Opcode::new(Code::LDA, AddressingMode::Immediate)),
    ]);
}

pub struct Opcode {
    pub code: Code,
    pub mode: AddressingMode,
}

impl Opcode {
    const fn new(code: Code, mode: AddressingMode) -> Self {
        Opcode { code, mode }
    }
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    Implied,
}

pub enum Code {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
}
