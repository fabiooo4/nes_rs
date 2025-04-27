use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    pub static ref CPU_OPCODES: HashMap<u8, Opcode> = HashMap::from([
        (0x00, Opcode::new(Code::BRK, AddressingMode::Implied),),
        (0xaa, Opcode::new(Code::TAX, AddressingMode::Implied),),
        (0xe8, Opcode::new(Code::INX, AddressingMode::Implied),),

        // Load ---------------------------------------------------
        (0xa9, Opcode::new(Code::LDA, AddressingMode::Immediate)),
        (0xa5, Opcode::new(Code::LDA, AddressingMode::ZeroPage)),
        (0xb5, Opcode::new(Code::LDA, AddressingMode::ZeroPage_X)),
        (0xad, Opcode::new(Code::LDA, AddressingMode::Absolute)),
        (0xbd, Opcode::new(Code::LDA, AddressingMode::Absolute_X)),
        (0xb9, Opcode::new(Code::LDA, AddressingMode::Absolute_Y)),
        (0xa1, Opcode::new(Code::LDA, AddressingMode::Indirect_X)),
        (0xb1, Opcode::new(Code::LDA, AddressingMode::Indirect_Y)),

        (0xa2, Opcode::new(Code::LDX, AddressingMode::Immediate)),
        (0xa6, Opcode::new(Code::LDX, AddressingMode::ZeroPage)),
        (0xb6, Opcode::new(Code::LDX, AddressingMode::ZeroPage_Y)),
        (0xae, Opcode::new(Code::LDX, AddressingMode::Absolute)),
        (0xbe, Opcode::new(Code::LDX, AddressingMode::Absolute_Y)),

        (0xa0, Opcode::new(Code::LDY, AddressingMode::Immediate)),
        (0xa4, Opcode::new(Code::LDY, AddressingMode::ZeroPage)),
        (0xb4, Opcode::new(Code::LDY, AddressingMode::ZeroPage_X)),
        (0xac, Opcode::new(Code::LDY, AddressingMode::Absolute)),
        (0xbc, Opcode::new(Code::LDY, AddressingMode::Absolute_X)),
        // Load ---------------------------------------------------

        // Add ----------------------------------------------------
        (0x69, Opcode::new(Code::ADC, AddressingMode::Immediate)),
        (0x65, Opcode::new(Code::ADC, AddressingMode::ZeroPage)),
        (0x75, Opcode::new(Code::ADC, AddressingMode::ZeroPage_X)),
        (0x6D, Opcode::new(Code::ADC, AddressingMode::Absolute)),
        (0x7D, Opcode::new(Code::ADC, AddressingMode::Absolute_X)),
        (0x79, Opcode::new(Code::ADC, AddressingMode::Absolute_Y)),
        (0x61, Opcode::new(Code::ADC, AddressingMode::Indirect_X)),
        (0x71, Opcode::new(Code::ADC, AddressingMode::Indirect_Y)),
        // Add ----------------------------------------------------
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
