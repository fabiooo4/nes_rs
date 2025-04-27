use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    pub static ref CPU_OPCODES: HashMap<u8, Opcode> = HashMap::from([
        (0x00, Opcode::new(Code::BRK, AddressingMode::Implied),),
        (0xaa, Opcode::new(Code::TAX, AddressingMode::Implied),),
        (0xe8, Opcode::new(Code::INX, AddressingMode::Implied),),

        // Load ===================================================
        (0xA9, Opcode::new(Code::LDA, AddressingMode::Immediate)),
        (0xA5, Opcode::new(Code::LDA, AddressingMode::ZeroPage)),
        (0xB5, Opcode::new(Code::LDA, AddressingMode::ZeroPage_X)),
        (0xAD, Opcode::new(Code::LDA, AddressingMode::Absolute)),
        (0xBD, Opcode::new(Code::LDA, AddressingMode::Absolute_X)),
        (0xB9, Opcode::new(Code::LDA, AddressingMode::Absolute_Y)),
        (0xA1, Opcode::new(Code::LDA, AddressingMode::Indirect_X)),
        (0xB1, Opcode::new(Code::LDA, AddressingMode::Indirect_Y)),

        (0xA2, Opcode::new(Code::LDX, AddressingMode::Immediate)),
        (0xA6, Opcode::new(Code::LDX, AddressingMode::ZeroPage)),
        (0xB6, Opcode::new(Code::LDX, AddressingMode::ZeroPage_Y)),
        (0xAE, Opcode::new(Code::LDX, AddressingMode::Absolute)),
        (0xBE, Opcode::new(Code::LDX, AddressingMode::Absolute_Y)),

        (0xA0, Opcode::new(Code::LDY, AddressingMode::Immediate)),
        (0xA4, Opcode::new(Code::LDY, AddressingMode::ZeroPage)),
        (0xB4, Opcode::new(Code::LDY, AddressingMode::ZeroPage_X)),
        (0xAC, Opcode::new(Code::LDY, AddressingMode::Absolute)),
        (0xBC, Opcode::new(Code::LDY, AddressingMode::Absolute_X)),
        // Load ===================================================

        // Arithmetic =============================================
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

        // Decrement memory ----------------------------------------
        (0xC6, Opcode::new(Code::DEC, AddressingMode::ZeroPage)),
        (0xD6, Opcode::new(Code::DEC, AddressingMode::ZeroPage_X)),
        (0xCE, Opcode::new(Code::DEC, AddressingMode::Absolute)),
        (0xDE, Opcode::new(Code::DEC, AddressingMode::Absolute_X)),
        // Decrement memory ----------------------------------------

        // Decrement X
        (0xCA, Opcode::new(Code::DEX, AddressingMode::Implied)),
        // Decrement Y
        (0x88, Opcode::new(Code::DEY, AddressingMode::Implied)),
        // Arithmetic =============================================

        // Boolean arithmetic =====================================
        // And ----------------------------------------------------
        (0x29, Opcode::new(Code::AND, AddressingMode::Immediate)),
        (0x25, Opcode::new(Code::AND, AddressingMode::ZeroPage)),
        (0x35, Opcode::new(Code::AND, AddressingMode::ZeroPage_X)),
        (0x2D, Opcode::new(Code::AND, AddressingMode::Absolute)),
        (0x3D, Opcode::new(Code::AND, AddressingMode::Absolute_X)),
        (0x39, Opcode::new(Code::AND, AddressingMode::Absolute_Y)),
        (0x21, Opcode::new(Code::AND, AddressingMode::Indirect_X)),
        (0x31, Opcode::new(Code::AND, AddressingMode::Indirect_Y)),
        // And ----------------------------------------------------

        // Accumulator shift left ---------------------------------
        (0x0A, Opcode::new(Code::ASL, AddressingMode::Implied)),
        (0x06, Opcode::new(Code::ASL, AddressingMode::ZeroPage)),
        (0x16, Opcode::new(Code::ASL, AddressingMode::ZeroPage_X)),
        (0x0E, Opcode::new(Code::ASL, AddressingMode::Absolute)),
        (0x1E, Opcode::new(Code::ASL, AddressingMode::Absolute_X)),
        // Accumulator shift left ---------------------------------

        // Bit test -----------------------------------------------
        (0x24, Opcode::new(Code::BIT, AddressingMode::ZeroPage)),
        (0x2C, Opcode::new(Code::BIT, AddressingMode::Absolute)),
        // Bit test -----------------------------------------------

        // And ----------------------------------------------------
        (0x49, Opcode::new(Code::EOR, AddressingMode::Immediate)),
        (0x45, Opcode::new(Code::EOR, AddressingMode::ZeroPage)),
        (0x55, Opcode::new(Code::EOR, AddressingMode::ZeroPage_X)),
        (0x4D, Opcode::new(Code::EOR, AddressingMode::Absolute)),
        (0x5D, Opcode::new(Code::EOR, AddressingMode::Absolute_X)),
        (0x59, Opcode::new(Code::EOR, AddressingMode::Absolute_Y)),
        (0x41, Opcode::new(Code::EOR, AddressingMode::Indirect_X)),
        (0x51, Opcode::new(Code::EOR, AddressingMode::Indirect_Y)),
        // And ----------------------------------------------------
        // Boolean arithmetic =====================================

        // Branching ==============================================
        // Branch if carry clear
        (0x90, Opcode::new(Code::BCC, AddressingMode::Immediate)),
        // Branch if carry set
        (0xB0, Opcode::new(Code::BCS, AddressingMode::Immediate)),
        // Branch if equal
        (0xF0, Opcode::new(Code::BEQ, AddressingMode::Immediate)),
        // Branch if not equal
        (0xD0, Opcode::new(Code::BNE, AddressingMode::Immediate)),
        // Branch if minus
        (0x30, Opcode::new(Code::BMI, AddressingMode::Immediate)),
        // Branch if positive
        (0x10, Opcode::new(Code::BPL, AddressingMode::Immediate)),
        // Branch if overflow clear
        (0x50, Opcode::new(Code::BVC, AddressingMode::Immediate)),
        // Branch if overflow set
        (0x70, Opcode::new(Code::BVS, AddressingMode::Immediate)),
        // Branching ==============================================

        // Status bits ============================================
        // Clear carry flag
        (0x18, Opcode::new(Code::CLC, AddressingMode::Implied)),
        // Clear decimal mode
        (0xD8, Opcode::new(Code::CLD, AddressingMode::Implied)),
        // Clear interrupt disable
        (0x58, Opcode::new(Code::CLI, AddressingMode::Implied)),
        // Clear overflow flag
        (0xB8, Opcode::new(Code::CLV, AddressingMode::Implied)),

        // Compare A ----------------------------------------------
        (0xC9, Opcode::new(Code::CMP, AddressingMode::Immediate)),
        (0xC5, Opcode::new(Code::CMP, AddressingMode::ZeroPage)),
        (0xD5, Opcode::new(Code::CMP, AddressingMode::ZeroPage_X)),
        (0xCD, Opcode::new(Code::CMP, AddressingMode::Absolute)),
        (0xDD, Opcode::new(Code::CMP, AddressingMode::Absolute_X)),
        (0xD9, Opcode::new(Code::CMP, AddressingMode::Absolute_Y)),
        (0xC1, Opcode::new(Code::CMP, AddressingMode::Indirect_X)),
        (0xD1, Opcode::new(Code::CMP, AddressingMode::Indirect_Y)),
        // Compare A ----------------------------------------------

        // Compare X ----------------------------------------------
        (0xE0, Opcode::new(Code::CPX, AddressingMode::Immediate)),
        (0xE4, Opcode::new(Code::CPX, AddressingMode::ZeroPage)),
        (0xEC, Opcode::new(Code::CPX, AddressingMode::Absolute)),
        // Compare X ----------------------------------------------

        // Compare Y ----------------------------------------------
        (0xC0, Opcode::new(Code::CPY, AddressingMode::Immediate)),
        (0xC4, Opcode::new(Code::CPY, AddressingMode::ZeroPage)),
        (0xCC, Opcode::new(Code::CPY, AddressingMode::Absolute)),
        // Compare Y ----------------------------------------------
        // Status bits ============================================
    ]);
}

#[derive(Debug)]
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

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug)]
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
