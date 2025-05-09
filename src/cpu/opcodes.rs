use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
    pub static ref CPU_OPCODES: HashMap<u8, Opcode> = HashMap::from([
        // Break
        (0x00, Opcode::new(Code::BRK, AddressingMode::Implied, 7, false)),
        // No operation
        (0xEA, Opcode::new(Code::NOP, AddressingMode::Implied, 2, false)),

        // Transfers ==============================================
        // Transfer A to X
        (0xAA, Opcode::new(Code::TAX, AddressingMode::Implied,2, false)),
        // Transfer A to Y
        (0xA8, Opcode::new(Code::TAY, AddressingMode::Implied,2, false)),
        // Transfer Stack to X
        (0xBA, Opcode::new(Code::TSX, AddressingMode::Implied,2, false)),
        // Transfer X to A
        (0x8A, Opcode::new(Code::TXA, AddressingMode::Implied,2, false)),
        // Transfer X to Stack
        (0x9A, Opcode::new(Code::TXS, AddressingMode::Implied,2, false)),
        // Transfer Y to A
        (0x98, Opcode::new(Code::TYA, AddressingMode::Implied,2, false)),
        // Transfers ==============================================

        // Load ===================================================
        (0xA9, Opcode::new(Code::LDA, AddressingMode::Immediate,2, false)),
        (0xA5, Opcode::new(Code::LDA, AddressingMode::ZeroPage,3, false)),
        (0xB5, Opcode::new(Code::LDA, AddressingMode::ZeroPage_X,4, false)),
        (0xAD, Opcode::new(Code::LDA, AddressingMode::Absolute,4, false)),
        (0xBD, Opcode::new(Code::LDA, AddressingMode::Absolute_X,4 /* +1 if page crossed */, false)),
        (0xB9, Opcode::new(Code::LDA, AddressingMode::Absolute_Y,4 /* +1 if page crossed */, false)),
        (0xA1, Opcode::new(Code::LDA, AddressingMode::Indirect_X,6, false)),
        (0xB1, Opcode::new(Code::LDA, AddressingMode::Indirect_Y,5 /* +1 if page crossed */, false)),

        (0xA2, Opcode::new(Code::LDX, AddressingMode::Immediate,2, false)),
        (0xA6, Opcode::new(Code::LDX, AddressingMode::ZeroPage,3, false)),
        (0xB6, Opcode::new(Code::LDX, AddressingMode::ZeroPage_Y,4, false)),
        (0xAE, Opcode::new(Code::LDX, AddressingMode::Absolute,4, false)),
        (0xBE, Opcode::new(Code::LDX, AddressingMode::Absolute_Y,4 /* +1 if page crossed */, false)),

        (0xA0, Opcode::new(Code::LDY, AddressingMode::Immediate,2, false)),
        (0xA4, Opcode::new(Code::LDY, AddressingMode::ZeroPage,3, false)),
        (0xB4, Opcode::new(Code::LDY, AddressingMode::ZeroPage_X,4, false)),
        (0xAC, Opcode::new(Code::LDY, AddressingMode::Absolute,4, false)),
        (0xBC, Opcode::new(Code::LDY, AddressingMode::Absolute_X,4 /* +1 if page crossed */, false)),
        // Load ===================================================

        // Store ==================================================
        (0x85, Opcode::new(Code::STA, AddressingMode::ZeroPage,3, false)),
        (0x95, Opcode::new(Code::STA, AddressingMode::ZeroPage_X,4, false)),
        (0x8D, Opcode::new(Code::STA, AddressingMode::Absolute,4, false)),
        (0x9D, Opcode::new(Code::STA, AddressingMode::Absolute_X,5, false)),
        (0x99, Opcode::new(Code::STA, AddressingMode::Absolute_Y,5, false)),
        (0x81, Opcode::new(Code::STA, AddressingMode::Indirect_X,6, false)),
        (0x91, Opcode::new(Code::STA, AddressingMode::Indirect_Y,6, false)),

        (0x86, Opcode::new(Code::STX, AddressingMode::ZeroPage,3, false)),
        (0x96, Opcode::new(Code::STX, AddressingMode::ZeroPage_Y,4, false)),
        (0x8E, Opcode::new(Code::STX, AddressingMode::Absolute,4, false)),

        (0x84, Opcode::new(Code::STY, AddressingMode::ZeroPage,3, false)),
        (0x94, Opcode::new(Code::STY, AddressingMode::ZeroPage_X,4, false)),
        (0x8C, Opcode::new(Code::STY, AddressingMode::Absolute,4, false)),
        // Store ==================================================

        // Stack ==================================================
        // Push accumulator
        (0x48, Opcode::new(Code::PHA, AddressingMode::Implied,3, false)),
        // Push processor status
        (0x08, Opcode::new(Code::PHP, AddressingMode::Implied,3, false)),
        // Pull accumulator
        (0x68, Opcode::new(Code::PLA, AddressingMode::Implied,4, false)),
        // Pull processor status
        (0x28, Opcode::new(Code::PLP, AddressingMode::Implied,4, false)),
        // Stack ==================================================

        // Arithmetic =============================================
        // Add ----------------------------------------------------
        (0x69, Opcode::new(Code::ADC, AddressingMode::Immediate, 2, false)),
        (0x65, Opcode::new(Code::ADC, AddressingMode::ZeroPage,3, false)),
        (0x75, Opcode::new(Code::ADC, AddressingMode::ZeroPage_X,4, false)),
        (0x6D, Opcode::new(Code::ADC, AddressingMode::Absolute,4, false)),
        (0x7D, Opcode::new(Code::ADC, AddressingMode::Absolute_X,4 /* +1 if page crossed */, false)),
        (0x79, Opcode::new(Code::ADC, AddressingMode::Absolute_Y,4 /* +1 if page crossed */, false)),
        (0x61, Opcode::new(Code::ADC, AddressingMode::Indirect_X,6, false)),
        (0x71, Opcode::new(Code::ADC, AddressingMode::Indirect_Y,5 /* +1 if page crossed */, false)),
        // Add ----------------------------------------------------

        // Subtract -----------------------------------------------
        (0xE9, Opcode::new(Code::SBC, AddressingMode::Immediate,2, false)),
        (0xE5, Opcode::new(Code::SBC, AddressingMode::ZeroPage,3, false)),
        (0xF5, Opcode::new(Code::SBC, AddressingMode::ZeroPage_X,4, false)),
        (0xED, Opcode::new(Code::SBC, AddressingMode::Absolute,4, false)),
        (0xFD, Opcode::new(Code::SBC, AddressingMode::Absolute_X,4 /* +1 if page crossed */, false)),
        (0xF9, Opcode::new(Code::SBC, AddressingMode::Absolute_Y,4 /* +1 if page crossed */, false)),
        (0xE1, Opcode::new(Code::SBC, AddressingMode::Indirect_X,6, false)),
        (0xF1, Opcode::new(Code::SBC, AddressingMode::Indirect_Y,5 /* +1 if page crossed */, false)),
        // Subtract -----------------------------------------------

        // Decrement memory ----------------------------------------
        (0xC6, Opcode::new(Code::DEC, AddressingMode::ZeroPage,5, false)),
        (0xD6, Opcode::new(Code::DEC, AddressingMode::ZeroPage_X,6, false)),
        (0xCE, Opcode::new(Code::DEC, AddressingMode::Absolute,6, false)),
        (0xDE, Opcode::new(Code::DEC, AddressingMode::Absolute_X,7, false)),
        // Decrement memory ----------------------------------------

        // Decrement X
        (0xCA, Opcode::new(Code::DEX, AddressingMode::Implied,2, false)),
        // Decrement Y
        (0x88, Opcode::new(Code::DEY, AddressingMode::Implied,2, false)),

        // Increment memory ----------------------------------------
        (0xE6, Opcode::new(Code::INC, AddressingMode::ZeroPage,5, false)),
        (0xF6, Opcode::new(Code::INC, AddressingMode::ZeroPage_X,6, false)),
        (0xEE, Opcode::new(Code::INC, AddressingMode::Absolute,6, false)),
        (0xFE, Opcode::new(Code::INC, AddressingMode::Absolute_X,7, false)),
        // Increment memory ----------------------------------------

        // Increment X
        (0xE8, Opcode::new(Code::INX, AddressingMode::Implied,2, false)),
        // Increment Y
        (0xC8, Opcode::new(Code::INY, AddressingMode::Implied,2, false)),
        // Arithmetic =============================================

        // Boolean arithmetic =====================================
        // And ----------------------------------------------------
        (0x29, Opcode::new(Code::AND, AddressingMode::Immediate,2, false)),
        (0x25, Opcode::new(Code::AND, AddressingMode::ZeroPage,3, false)),
        (0x35, Opcode::new(Code::AND, AddressingMode::ZeroPage_X,4, false)),
        (0x2D, Opcode::new(Code::AND, AddressingMode::Absolute,4, false)),
        (0x3D, Opcode::new(Code::AND, AddressingMode::Absolute_X,4 /* +1 if page crossed */, false)),
        (0x39, Opcode::new(Code::AND, AddressingMode::Absolute_Y,4 /* +1 if page crossed */, false)),
        (0x21, Opcode::new(Code::AND, AddressingMode::Indirect_X,6, false)),
        (0x31, Opcode::new(Code::AND, AddressingMode::Indirect_Y,5 /* +1 if page crossed */, false)),
        // And ----------------------------------------------------

        // Accumulator shift left ---------------------------------
        (0x0A, Opcode::new(Code::ASL, AddressingMode::Implied,2, false)),
        (0x06, Opcode::new(Code::ASL, AddressingMode::ZeroPage,5, false)),
        (0x16, Opcode::new(Code::ASL, AddressingMode::ZeroPage_X,6, false)),
        (0x0E, Opcode::new(Code::ASL, AddressingMode::Absolute,6, false)),
        (0x1E, Opcode::new(Code::ASL, AddressingMode::Absolute_X,7, false)),
        // Accumulator shift left ---------------------------------

        // Logical shift right ------------------------------------
        (0x4A, Opcode::new(Code::LSR, AddressingMode::Implied,2, false)),
        (0x46, Opcode::new(Code::LSR, AddressingMode::ZeroPage,5, false)),
        (0x56, Opcode::new(Code::LSR, AddressingMode::ZeroPage_X,6, false)),
        (0x4E, Opcode::new(Code::LSR, AddressingMode::Absolute,6, false)),
        (0x5E, Opcode::new(Code::LSR, AddressingMode::Absolute_X,7, false)),
        // Logical shift right ------------------------------------

        // Rotate left --------------------------------------------
        (0x2A, Opcode::new(Code::ROL, AddressingMode::Implied,2, false)),
        (0x26, Opcode::new(Code::ROL, AddressingMode::ZeroPage,5, false)),
        (0x36, Opcode::new(Code::ROL, AddressingMode::ZeroPage_X,6, false)),
        (0x2E, Opcode::new(Code::ROL, AddressingMode::Absolute,6, false)),
        (0x3E, Opcode::new(Code::ROL, AddressingMode::Absolute_X,7, false)),
        // Rotate left --------------------------------------------

        // Rotate right --------------------------------------------
        (0x6A, Opcode::new(Code::ROR, AddressingMode::Implied,2, false)),
        (0x66, Opcode::new(Code::ROR, AddressingMode::ZeroPage,5, false)),
        (0x76, Opcode::new(Code::ROR, AddressingMode::ZeroPage_X,6, false)),
        (0x6E, Opcode::new(Code::ROR, AddressingMode::Absolute,6, false)),
        (0x7E, Opcode::new(Code::ROR, AddressingMode::Absolute_X,7, false)),
        // Rotate right --------------------------------------------

        // Bit test -----------------------------------------------
        (0x24, Opcode::new(Code::BIT, AddressingMode::ZeroPage,3, false)),
        (0x2C, Opcode::new(Code::BIT, AddressingMode::Absolute,4, false)),
        // Bit test -----------------------------------------------

        // Exclusive Or -------------------------------------------
        (0x49, Opcode::new(Code::EOR, AddressingMode::Immediate,2, false)),
        (0x45, Opcode::new(Code::EOR, AddressingMode::ZeroPage,3, false)),
        (0x55, Opcode::new(Code::EOR, AddressingMode::ZeroPage_X,4, false)),
        (0x4D, Opcode::new(Code::EOR, AddressingMode::Absolute,4, false)),
        (0x5D, Opcode::new(Code::EOR, AddressingMode::Absolute_X,4 /* +1 if page crossed */, false)),
        (0x59, Opcode::new(Code::EOR, AddressingMode::Absolute_Y,4 /* +1 if page crossed */, false)),
        (0x41, Opcode::new(Code::EOR, AddressingMode::Indirect_X,6, false)),
        (0x51, Opcode::new(Code::EOR, AddressingMode::Indirect_Y,5 /* +1 if page crossed */, false)),
        // Exclusive Or -------------------------------------------

        // Logical Inclusive Or -----------------------------------
        (0x09, Opcode::new(Code::ORA, AddressingMode::Immediate,2, false)),
        (0x05, Opcode::new(Code::ORA, AddressingMode::ZeroPage,3, false)),
        (0x15, Opcode::new(Code::ORA, AddressingMode::ZeroPage_X,4, false)),
        (0x0D, Opcode::new(Code::ORA, AddressingMode::Absolute,4, false)),
        (0x1D, Opcode::new(Code::ORA, AddressingMode::Absolute_X,4 /* +1 if page crossed */, false)),
        (0x19, Opcode::new(Code::ORA, AddressingMode::Absolute_Y,4 /* +1 if page crossed */, false)),
        (0x01, Opcode::new(Code::ORA, AddressingMode::Indirect_X,6, false)),
        (0x11, Opcode::new(Code::ORA, AddressingMode::Indirect_Y,5 /* +1 if page crossed */, false)),
        // Logical Inclusive Or -----------------------------------
        // Boolean arithmetic =====================================

        // Branching ==============================================
        // Branch if carry clear
        (0x90, Opcode::new(Code::BCC, AddressingMode::Immediate,2 /* +1 if branch taken, +2 if page crossed */, false)),
        // Branch if carry set
        (0xB0, Opcode::new(Code::BCS, AddressingMode::Immediate,2 /* +1 if branch taken, +2 if page crossed */, false)),
        // Branch if equal
        (0xF0, Opcode::new(Code::BEQ, AddressingMode::Immediate,2 /* +1 if branch taken, +2 if page crossed */, false)),
        // Branch if not equal
        (0xD0, Opcode::new(Code::BNE, AddressingMode::Immediate,2 /* +1 if branch taken, +2 if page crossed */, false)),
        // Branch if minus
        (0x30, Opcode::new(Code::BMI, AddressingMode::Immediate,2 /* +1 if branch taken, +2 if page crossed */, false)),
        // Branch if positive
        (0x10, Opcode::new(Code::BPL, AddressingMode::Immediate,2 /* +1 if branch taken, +2 if page crossed */, false)),
        // Branch if overflow clear
        (0x50, Opcode::new(Code::BVC, AddressingMode::Immediate,2 /* +1 if branch taken, +2 if page crossed */, false)),
        // Branch if overflow set
        (0x70, Opcode::new(Code::BVS, AddressingMode::Immediate,2 /* +1 if branch taken, +2 if page crossed */, false)),

        // Jump ---------------------------------------------------
        (0x4C, Opcode::new(Code::JMP, AddressingMode::Absolute,3, false)),
        (0x6C, Opcode::new(Code::JMP, AddressingMode::Indirect,5, false)),
        // Jump ---------------------------------------------------

        // Return from Interrupt
        (0x40, Opcode::new(Code::RTI, AddressingMode::Implied,6, false)),

        // Jump to subroutine
        (0x20, Opcode::new(Code::JSR, AddressingMode::Absolute,6, false)),
        // Return from subroutine
        (0x60, Opcode::new(Code::RTS, AddressingMode::Implied,6, false)),
        // Branching ==============================================

        // Status bits ============================================
        // Clear carry flag
        (0x18, Opcode::new(Code::CLC, AddressingMode::Implied,2, false)),
        // Clear decimal mode
        (0xD8, Opcode::new(Code::CLD, AddressingMode::Implied,2, false)),
        // Clear interrupt disable
        (0x58, Opcode::new(Code::CLI, AddressingMode::Implied,2, false)),
        // Clear overflow flag
        (0xB8, Opcode::new(Code::CLV, AddressingMode::Implied,2, false)),

        // Set carry flag
        (0x38, Opcode::new(Code::SEC, AddressingMode::Implied,2, false)),
        // Set decimal mode
        (0xF8, Opcode::new(Code::SED, AddressingMode::Implied,2, false)),
        // Set interrupt disable
        (0x78, Opcode::new(Code::SEI, AddressingMode::Implied,2, false)),

        // Compare A ----------------------------------------------
        (0xC9, Opcode::new(Code::CMP, AddressingMode::Immediate,2, false)),
        (0xC5, Opcode::new(Code::CMP, AddressingMode::ZeroPage,3, false)),
        (0xD5, Opcode::new(Code::CMP, AddressingMode::ZeroPage_X,4, false)),
        (0xCD, Opcode::new(Code::CMP, AddressingMode::Absolute,4, false)),
        (0xDD, Opcode::new(Code::CMP, AddressingMode::Absolute_X,4 /* +1 if page crossed */, false)),
        (0xD9, Opcode::new(Code::CMP, AddressingMode::Absolute_Y,4 /* +1 if page crossed */, false)),
        (0xC1, Opcode::new(Code::CMP, AddressingMode::Indirect_X,6, false)),
        (0xD1, Opcode::new(Code::CMP, AddressingMode::Indirect_Y,5 /* +1 if page crossed */, false)),
        // Compare A ----------------------------------------------

        // Compare X ----------------------------------------------
        (0xE0, Opcode::new(Code::CPX, AddressingMode::Immediate,2, false)),
        (0xE4, Opcode::new(Code::CPX, AddressingMode::ZeroPage,3, false)),
        (0xEC, Opcode::new(Code::CPX, AddressingMode::Absolute,4, false)),
        // Compare X ----------------------------------------------

        // Compare Y ----------------------------------------------
        (0xC0, Opcode::new(Code::CPY, AddressingMode::Immediate,2, false)),
        (0xC4, Opcode::new(Code::CPY, AddressingMode::ZeroPage,3, false)),
        (0xCC, Opcode::new(Code::CPY, AddressingMode::Absolute,4, false)),
        // Compare Y ----------------------------------------------
        // Status bits ============================================


        // Illegal opcodes ========================================
        // NOP ----------------------------------------------------
        (0x04, Opcode::new(Code::NOP, AddressingMode::ZeroPage,3, true)),
        (0x44, Opcode::new(Code::NOP, AddressingMode::ZeroPage,3, true)),
        (0x64, Opcode::new(Code::NOP, AddressingMode::ZeroPage,3, true)),
        (0x14, Opcode::new(Code::NOP, AddressingMode::ZeroPage_X,4, true)),
        (0x34, Opcode::new(Code::NOP, AddressingMode::ZeroPage_X,4, true)),
        (0x54, Opcode::new(Code::NOP, AddressingMode::ZeroPage_X,4, true)),
        (0x74, Opcode::new(Code::NOP, AddressingMode::ZeroPage_X,4, true)),
        (0xD4, Opcode::new(Code::NOP, AddressingMode::ZeroPage_X,4, true)),
        (0xF4, Opcode::new(Code::NOP, AddressingMode::ZeroPage_X,4, true)),
        (0x80, Opcode::new(Code::NOP, AddressingMode::Immediate,2, true)),
        (0x82, Opcode::new(Code::NOP, AddressingMode::Immediate,2, true)),
        (0x89, Opcode::new(Code::NOP, AddressingMode::Immediate,2, true)),
        (0xC2, Opcode::new(Code::NOP, AddressingMode::Immediate,2, true)),
        (0xE2, Opcode::new(Code::NOP, AddressingMode::Immediate,2, true)),
        (0x0C, Opcode::new(Code::NOP, AddressingMode::Absolute,4, true)),
        (0x1C, Opcode::new(Code::NOP, AddressingMode::Absolute_X,4 /* +1 if page crossed */, true)),
        (0x3C, Opcode::new(Code::NOP, AddressingMode::Absolute_X,4 /* +1 if page crossed */,  true)),
        (0x5C, Opcode::new(Code::NOP, AddressingMode::Absolute_X,4 /* +1 if page crossed */, true)),
        (0x7C, Opcode::new(Code::NOP, AddressingMode::Absolute_X,4 /* +1 if page crossed */, true)),
        (0xDC, Opcode::new(Code::NOP, AddressingMode::Absolute_X,4 /* +1 if page crossed */, true)),
        (0xFC, Opcode::new(Code::NOP, AddressingMode::Absolute_X,4 /* +1 if page crossed */, true)),
        (0x1A, Opcode::new(Code::NOP, AddressingMode::Implied,2, true)),
        (0x3A, Opcode::new(Code::NOP, AddressingMode::Implied,2, true)),
        (0x5A, Opcode::new(Code::NOP, AddressingMode::Implied,2, true)),
        (0x7A, Opcode::new(Code::NOP, AddressingMode::Implied,2, true)),
        (0xDA, Opcode::new(Code::NOP, AddressingMode::Implied,2, true)),
        (0xFA, Opcode::new(Code::NOP, AddressingMode::Implied,2, true)),
        // NOP ----------------------------------------------------

        // Load A and X -------------------------------------------
        (0xA7, Opcode::new(Code::LAX, AddressingMode::ZeroPage,3, true)),
        (0xB7, Opcode::new(Code::LAX, AddressingMode::ZeroPage_Y,4, true)),
        (0xAF, Opcode::new(Code::LAX, AddressingMode::Absolute,4, true)),
        (0xBF, Opcode::new(Code::LAX, AddressingMode::Absolute_Y,4 /* +1 if page crossed */, true)),
        (0xA3, Opcode::new(Code::LAX, AddressingMode::Indirect_X,6, true)),
        (0xB3, Opcode::new(Code::LAX, AddressingMode::Indirect_Y,5 /* +1 if page crossed */, true)),
        // Load A and X -------------------------------------------

        // Store A & X --------------------------------------------
        (0x87, Opcode::new(Code::SAX, AddressingMode::ZeroPage,3, true)),
        (0x97, Opcode::new(Code::SAX, AddressingMode::ZeroPage_Y,4, true)),
        (0x83, Opcode::new(Code::SAX, AddressingMode::Indirect_X,6, true)),
        (0x8F, Opcode::new(Code::SAX, AddressingMode::Absolute,4, true)),
        // Store A & X --------------------------------------------

        // Subtract with carry
        (0xEB, Opcode::new(Code::SBC, AddressingMode::Immediate,2, true)),

        // Subtract 1 from memory (without borrow) ----------------
        (0xC7, Opcode::new(Code::DCP, AddressingMode::ZeroPage,5, true)),
        (0xD7, Opcode::new(Code::DCP, AddressingMode::ZeroPage_X,6, true)),
        (0xCF, Opcode::new(Code::DCP, AddressingMode::Absolute,6, true)),
        (0xDF, Opcode::new(Code::DCP, AddressingMode::Absolute_X,7, true)),
        (0xDB, Opcode::new(Code::DCP, AddressingMode::Absolute_Y,7, true)),
        (0xC3, Opcode::new(Code::DCP, AddressingMode::Indirect_X,8, true)),
        (0xD3, Opcode::new(Code::DCP, AddressingMode::Indirect_Y,8, true)),
        // Subtract 1 from memory (without borrow) ----------------

        // Mem + 1, A - Mem ---------------------------------------
        (0xE7, Opcode::new(Code::ISB, AddressingMode::ZeroPage,5, true)),
        (0xF7, Opcode::new(Code::ISB, AddressingMode::ZeroPage_X,6, true)),
        (0xEF, Opcode::new(Code::ISB, AddressingMode::Absolute,6, true)),
        (0xFF, Opcode::new(Code::ISB, AddressingMode::Absolute_X,7, true)),
        (0xFB, Opcode::new(Code::ISB, AddressingMode::Absolute_Y,7, true)),
        (0xE3, Opcode::new(Code::ISB, AddressingMode::Indirect_X,8, true)),
        (0xF3, Opcode::new(Code::ISB, AddressingMode::Indirect_Y,8, true)),
        // Mem + 1, A - Mem ---------------------------------------

        // Mem << 1, A | Mem ---------------------------------------
        (0x07, Opcode::new(Code::SLO, AddressingMode::ZeroPage,5, true)),
        (0x17, Opcode::new(Code::SLO, AddressingMode::ZeroPage_X,6, true)),
        (0x0F, Opcode::new(Code::SLO, AddressingMode::Absolute,6, true)),
        (0x1F, Opcode::new(Code::SLO, AddressingMode::Absolute_X,7, true)),
        (0x1B, Opcode::new(Code::SLO, AddressingMode::Absolute_Y,7, true)),
        (0x03, Opcode::new(Code::SLO, AddressingMode::Indirect_X,8, true)),
        (0x13, Opcode::new(Code::SLO, AddressingMode::Indirect_Y,8, true)),
        // Mem << 1, A | Mem ---------------------------------------

        // ROL Mem, A & Mem ---------------------------------------
        (0x27, Opcode::new(Code::RLA, AddressingMode::ZeroPage,5, true)),
        (0x37, Opcode::new(Code::RLA, AddressingMode::ZeroPage_X,6, true)),
        (0x2F, Opcode::new(Code::RLA, AddressingMode::Absolute,6, true)),
        (0x3F, Opcode::new(Code::RLA, AddressingMode::Absolute_X,7, true)),
        (0x3B, Opcode::new(Code::RLA, AddressingMode::Absolute_Y,7, true)),
        (0x23, Opcode::new(Code::RLA, AddressingMode::Indirect_X,8, true)),
        (0x33, Opcode::new(Code::RLA, AddressingMode::Indirect_Y,8, true)),
        // ROL Mem, A & Mem ---------------------------------------

        // Mem >> 1, A ^ Mem --------------------------------------
        (0x47, Opcode::new(Code::SRE, AddressingMode::ZeroPage,5, true)),
        (0x57, Opcode::new(Code::SRE, AddressingMode::ZeroPage_X,6, true)),
        (0x4F, Opcode::new(Code::SRE, AddressingMode::Absolute,6, true)),
        (0x5F, Opcode::new(Code::SRE, AddressingMode::Absolute_X,7, true)),
        (0x5B, Opcode::new(Code::SRE, AddressingMode::Absolute_Y,7, true)),
        (0x43, Opcode::new(Code::SRE, AddressingMode::Indirect_X,8, true)),
        (0x53, Opcode::new(Code::SRE, AddressingMode::Indirect_Y,8, true)),
        // Mem >> 1, A ^ Mem --------------------------------------

        // ROR Mem, A + Mem ---------------------------------------
        (0x67, Opcode::new(Code::RRA, AddressingMode::ZeroPage,5, true)),
        (0x77, Opcode::new(Code::RRA, AddressingMode::ZeroPage_X,6, true)),
        (0x6F, Opcode::new(Code::RRA, AddressingMode::Absolute,6, true)),
        (0x7F, Opcode::new(Code::RRA, AddressingMode::Absolute_X,7, true)),
        (0x7B, Opcode::new(Code::RRA, AddressingMode::Absolute_Y,7, true)),
        (0x63, Opcode::new(Code::RRA, AddressingMode::Indirect_X,8, true)),
        (0x73, Opcode::new(Code::RRA, AddressingMode::Indirect_Y,8, true)),
        // ROR Mem, A + Mem ---------------------------------------
        // Illegal opcodes ========================================
    ]);
}

#[derive(Debug)]
pub struct Opcode {
    pub code: Code,
    pub mode: AddressingMode,
    pub cycles: usize,
    pub undocumented: bool,
}

impl Opcode {
    const fn new(code: Code, mode: AddressingMode, cycles: usize, undocumented: bool) -> Self {
        Opcode {
            code,
            mode,
            cycles,
            undocumented,
        }
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
    Indirect,
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

    // Illegal opcodes
    LAX,
    SAX,
    DCP,
    ISB,
    SLO,
    RLA,
    SRE,
    RRA,
}
