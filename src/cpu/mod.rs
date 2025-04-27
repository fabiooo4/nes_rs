mod opcodes;
use core::panic;
use opcodes::{AddressingMode, Code};

pub struct CPU {
    register_a: u8,
    register_x: u8,
    register_y: u8,
    status: Status,
    program_counter: u16,
    memory: [u8; 0xFFFF],
}

impl CPU {
    /// Creates a new CPU instance with default values
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: Status::new(),
            program_counter: 0,
            memory: [0; 0xFFFF],
        }
    }

    /// Loads a program into memory and runs it
    pub fn load_and_run(&mut self, program: &[u8]) {
        self.load(program);
        self.reset();
        self.run();
    }

    /// Reads a byte from memory
    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    /// Reads a 16-bit value from memory (little endian)
    fn mem_read_16(&self, addr: u16) -> u16 {
        // Little endian
        let low = self.mem_read(addr) as u16;
        let high = self.mem_read(addr + 1) as u16;

        high << 8 | low
    }

    /// Writes a byte to memory
    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    /// Writes a 16-bit value to memory (little endian)
    fn mem_write_16(&mut self, addr: u16, data: u16) {
        // Little endian
        let high = (data & 0xFF) as u8;
        let low = (data >> 8) as u8;

        self.mem_write(addr, high);
        self.mem_write(addr + 1, low);
    }

    /// Loads a program into memory at 0x8000
    ///
    /// The starting point of the program is written to 0xFFFC
    fn load(&mut self, program: &[u8]) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(program);

        // Write the program start in 0xFFFC
        self.mem_write_16(0xFFFC, 0x8000);
    }

    /// Resets the CPU registers and status bits, then loads the program start address from 0xFFFC
    fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.status = Status::new();

        // Load program start from 0xFFFC
        self.program_counter = self.mem_read_16(0xFFFC);
    }

    /// Runs the CPU until a BRK instruction is encountered
    ///
    /// # Panics
    /// If an invalid opcode is encountered, the CPU will panic.
    fn run(&mut self) {
        loop {
            let code = self.mem_read(self.program_counter);
            let opcode = opcodes::CPU_OPCODES
                .get(&code)
                .unwrap_or_else(|| panic!("Invalid opcode: {:X}", code));

            self.program_counter += 1;

            match opcode.code {
                Code::ADC => todo!(),
                Code::AND => todo!(),
                Code::ASL => todo!(),
                Code::BCC => todo!(),
                Code::BCS => todo!(),
                Code::BEQ => todo!(),
                Code::BIT => todo!(),
                Code::BMI => todo!(),
                Code::BNE => todo!(),
                Code::BPL => todo!(),
                Code::BRK => return,
                Code::BVC => todo!(),
                Code::BVS => todo!(),
                Code::CLC => todo!(),
                Code::CLD => todo!(),
                Code::CLI => todo!(),
                Code::CLV => todo!(),
                Code::CMP => todo!(),
                Code::CPX => todo!(),
                Code::CPY => todo!(),
                Code::DEC => todo!(),
                Code::DEX => todo!(),
                Code::DEY => todo!(),
                Code::EOR => todo!(),
                Code::INC => todo!(),
                Code::INX => self.inx(),
                Code::INY => todo!(),
                Code::JMP => todo!(),
                Code::JSR => todo!(),
                Code::LDA => self.lda(&opcode.mode),
                Code::LDX => todo!(),
                Code::LDY => todo!(),
                Code::LSR => todo!(),
                Code::NOP => todo!(),
                Code::ORA => todo!(),
                Code::PHA => todo!(),
                Code::PHP => todo!(),
                Code::PLA => todo!(),
                Code::PLP => todo!(),
                Code::ROL => todo!(),
                Code::ROR => todo!(),
                Code::RTI => todo!(),
                Code::RTS => todo!(),
                Code::SBC => todo!(),
                Code::SEC => todo!(),
                Code::SED => todo!(),
                Code::SEI => todo!(),
                Code::STA => todo!(),
                Code::STX => todo!(),
                Code::STY => todo!(),
                Code::TAX => self.tax(),
                Code::TAY => todo!(),
                Code::TSX => todo!(),
                Code::TXA => todo!(),
                Code::TXS => todo!(),
                Code::TYA => todo!(),
            }
        }
    }

    /// Updates the zero and negative flags based on the result of an operation
    fn update_zero_and_negative_flags(&mut self, result: u8) {
        self.status.zero = result == 0;
        self.status.negative = result & 0b10000000 == 0b10000000;
    }

    /// Gets the address of the operand parameters based on the addressing mode.
    /// This function also updates the program counter based on the parameter length.
    ///
    /// # Panics
    /// If an opcode with `NoneAddressing` mode is encountered, the CPU will panic
    /// because no parameters are expected.
    fn get_parameters_address(&mut self, mode: &AddressingMode) -> u16 {
        let addr = match mode {
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,

            AddressingMode::ZeroPage_X => self
                .mem_read(self.program_counter)
                .wrapping_add(self.register_x) as u16,

            AddressingMode::ZeroPage_Y => self
                .mem_read(self.program_counter)
                .wrapping_add(self.register_y) as u16,

            AddressingMode::Absolute => self.mem_read_16(self.program_counter),

            AddressingMode::Absolute_X => self
                .mem_read_16(self.program_counter)
                .wrapping_add(self.register_x as u16),

            AddressingMode::Absolute_Y => self
                .mem_read_16(self.program_counter)
                .wrapping_add(self.register_y as u16),

            AddressingMode::Indirect_X => {
                let base = self
                    .mem_read(self.program_counter)
                    .wrapping_add(self.register_x);
                let low = self.mem_read(base as u16) as u16;
                let high = self.mem_read(base as u16 + 1) as u16;

                self.program_counter += 2;

                high << 8 | low
            }

            AddressingMode::Indirect_Y => {
                let base = self
                    .mem_read(self.program_counter)
                    .wrapping_add(self.register_y);
                let low = self.mem_read(base as u16) as u16;
                let high = self.mem_read(base as u16 + 1) as u16;

                self.program_counter += 2;

                high << 8 | low
            }

            AddressingMode::Implied => panic!("{:?} doesn't expect any parameter", mode),
        };

        // Update program counter based on parameter length
        match mode {
            AddressingMode::Immediate
            | AddressingMode::ZeroPage
            | AddressingMode::ZeroPage_X
            | AddressingMode::ZeroPage_Y => self.program_counter += 1,

            AddressingMode::Absolute
            | AddressingMode::Absolute_X
            | AddressingMode::Absolute_Y
            | AddressingMode::Indirect_X
            | AddressingMode::Indirect_Y => self.program_counter += 2,

            AddressingMode::Implied => {}
        }

        addr
    }
}

/// Opcodes implementation
impl CPU {
    /// Loads a byte of memory into the accumulator setting the zero and
    /// negative flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Immediate        | $A9    | 2     | 2                          |
    /// | Zero Page        | $A5    | 2     | 3                          |
    /// | Zero Page, X     | $B5    | 2     | 4                          |
    /// | Absolute         | $AD    | 3     | 4                          |
    /// | Absolute, X      | $BD    | 3     | 4 (+1 if page crossed)     |
    /// | Absolute, Y      | $B9    | 3     | 4 (+1 if page crossed)     |
    /// | Indirect, X      | $A1    | 2     | 6                          |
    /// | Indirect, Y      | $B1    | 2     | 5 (+1 if page crossed)     |
    fn lda(&mut self, mode: &AddressingMode) {
        let param_addr = self.get_parameters_address(mode);
        let value = self.mem_read(param_addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// Copies the current contents of the accumulator into the X register and
    /// sets the zero and negative flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles |
    /// |------------------|--------|-------|--------|
    /// | Implied          | $AA    | 1     | 2      |
    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// Adds one to the X register setting the zero and negative flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles |
    /// |------------------|--------|-------|--------|
    /// | Implied          | $E8    | 1     | 2      |
    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }
}

impl Default for CPU {
    fn default() -> Self {
        Self::new()
    }
}

/// ```text
/// 7  bit  0
/// ---- ----
/// NV1B DIZC
/// |||| ||||
/// |||| |||+- Carry
/// |||| ||+-- Zero
/// |||| |+--- Interrupt Disable
/// |||| +---- Decimal
/// |||+------ Break (No CPU effect)
/// ||+------- (No CPU effect; always pushed as 1)
/// |+-------- Overflow
/// +--------- Negative
/// ```
struct Status {
    /// The negative flag is set if the result of the last operation had bit 7
    /// set to a one
    negative: bool,

    /// The overflow flag is set during arithmetic operations if the result has
    /// yielded an invalid 2's complement result (e.g. adding to positive
    /// numbers and ending up with a negative result: 64 + 64 => -128). It is
    /// determined by looking at the carry between bits 6 and 7 and between bit
    /// 7 and the carry flag
    overflow: bool,

    /// The break command bit is set when a BRK instruction has been executed
    /// and an interrupt has been generated to process it
    brk: bool,

    /// While the decimal mode flag is set the processor will obey the rules of
    /// Binary Coded Decimal (BCD) arithmetic during addition and subtraction.
    ///
    /// The flag can be explicitly set using `Set Decimal Flag` (SED) and
    /// cleared with `Clear Decimal Flag` (CLD)
    decimal: bool,

    /// The interrupt disable flag is set if the program has executed a
    /// `Set Interrupt Disable` (SEI) instruction. While this flag is set the
    /// processor will not respond to interrupts from devices until it is
    /// cleared by a `Clear Interrupt Disable` (CLI) instruction
    interrupt_disable: bool,

    /// The zero flag is set if the result of the last operation as was zero
    zero: bool,

    /// The carry flag is set if the last operation caused an overflow from bit
    /// 7 of the result or an underflow from bit 0. This condition is set
    /// during arithmetic, comparison and during logical shifts.
    ///
    /// It can be explicitly set using the `Set Carry Flag` (SEC) instruction
    /// and cleared with `Clear Carry Flag` (CLC)
    carry: bool,
}

impl Status {
    fn new() -> Self {
        Status {
            negative: false,
            overflow: false,
            decimal: false,
            interrupt_disable: false,
            zero: false,
            carry: false,
            brk: false,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    // LDA test --------------------------------------
    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&[0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert!(!cpu.status.zero);
        assert!(!cpu.status.negative);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&[0xa9, 0x00, 0x00]);
        assert!(cpu.status.zero);
    }
    // LDA test --------------------------------------

    // TAX test --------------------------------------
    #[test]
    fn test_0xaa_tax() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&[0xa9, 0x11, 0xaa, 0x00]);
        assert_eq!(cpu.register_a, 0x11);
        assert_eq!(cpu.register_x, 0x11);
        assert!(!cpu.status.zero);
        assert!(!cpu.status.negative);
    }

    #[test]
    fn test_0xaa_tax_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&[0xa9, 0x00, 0xaa, 0x00]);
        assert!(cpu.status.zero);
    }
    // TAX test --------------------------------------

    // INX test --------------------------------------
    #[test]
    fn test_0xe8_inx() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&[0xa9, 0x04, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 5)
    }
    #[test]
    fn test_0xe8_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&[0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 1)
    }
    // INX test --------------------------------------

    // Basic program test -------------------------------
    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&[0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1)
    }
    // Basic program test -------------------------------

    // Memory read/write tests ----------------------
    #[test]
    fn test_mem_write() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x1234, 0x69);
        assert_eq!(cpu.memory[0x1234], 0x69)
    }

    #[test]
    fn test_mem_write_16() {
        let mut cpu = CPU::new();
        cpu.mem_write_16(0x1234, 0x420);
        assert_eq!(cpu.memory[0x1234], 0x20);
        assert_eq!(cpu.memory[0x1235], 0x04)
    }

    #[test]
    fn test_mem_read() {
        let mut cpu = CPU::new();
        cpu.memory[0x1234] = 0x69;
        assert_eq!(cpu.mem_read(0x1234), 0x69)
    }

    #[test]
    fn test_mem_read_16() {
        let mut cpu = CPU::new();
        cpu.memory[0x1234] = 0x20;
        cpu.memory[0x1235] = 0x04;
        assert_eq!(cpu.mem_read_16(0x1234), 0x0420);
    }
    // Memory read/write tests ----------------------
}
