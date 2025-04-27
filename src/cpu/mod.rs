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

trait Memory {
    fn mem_read(&self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, data: u8);

    /// Writes a 16-bit value to memory (little endian)
    fn mem_write_16(&mut self, addr: u16, data: u16) {
        // Little endian
        let high = (data & 0xFF) as u8;
        let low = (data >> 8) as u8;

        self.mem_write(addr, high);
        self.mem_write(addr + 1, low);
    }

    /// Reads a 16-bit value from memory (little endian)
    fn mem_read_16(&self, addr: u16) -> u16 {
        // Little endian
        let low = self.mem_read(addr) as u16;
        let high = self.mem_read(addr + 1) as u16;

        high << 8 | low
    }
}

impl Memory for CPU {
    /// Reads a byte from memory
    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    /// Writes a byte to memory
    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }
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
                Code::ADC => self.adc(&opcode.mode),
                Code::AND => self.and(&opcode.mode),
                Code::ASL => self.asl(&opcode.mode),
                Code::BCC => self.branch(!self.status.carry),
                Code::BCS => self.branch(self.status.carry),
                Code::BEQ => self.branch(self.status.zero),
                Code::BIT => self.bit(&opcode.mode),
                Code::BMI => self.branch(self.status.negative),
                Code::BNE => self.branch(!self.status.zero),
                Code::BPL => self.branch(!self.status.negative),
                Code::BRK => return,
                Code::BVC => self.branch(!self.status.overflow),
                Code::BVS => self.branch(self.status.overflow),
                Code::CLC => self.status.carry = false,
                Code::CLD => self.status.decimal = false,
                Code::CLI => self.status.interrupt_disable = false,
                Code::CLV => self.status.overflow = false,
                Code::CMP => self.compare_register(self.register_a, &opcode.mode),
                Code::CPX => self.compare_register(self.register_x, &opcode.mode),
                Code::CPY => self.compare_register(self.register_y, &opcode.mode),
                Code::DEC => self.dec(&opcode.mode),
                Code::DEX => self.dex(),
                Code::DEY => self.dey(),
                Code::EOR => self.eor(&opcode.mode),
                Code::INC => todo!(),
                Code::INX => self.inx(),
                Code::INY => todo!(),
                Code::JMP => todo!(),
                Code::JSR => todo!(),
                Code::LDA => self.lda(&opcode.mode),
                Code::LDX => self.ldx(&opcode.mode),
                Code::LDY => self.ldy(&opcode.mode),
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

    /// Sets register A and updates `zero` and `negative` status flags
    fn set_register_a(&mut self, value: u8) {
        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// Updates the zero and negative flags based on the result of an operation
    fn update_zero_and_negative_flags(&mut self, result: u8) {
        self.status.zero = result == 0;
        self.status.negative = result & 0b10000000 == 0b10000000;
    }

    /// Gets the address of the operand parameters based on the addressing mode.
    /// This function also updates the program counter based on the parameter length.
    ///
    /// # Returns
    /// Returns `None` if the mode is implied
    ///
    /// # Panics
    /// If an opcode with `NoneAddressing` mode is encountered, the CPU will panic
    /// because no parameters are expected.
    fn get_parameters_address(&mut self, mode: &AddressingMode) -> Option<u16> {
        let addr = match mode {
            AddressingMode::Immediate => Some(self.program_counter),
            AddressingMode::ZeroPage => Some(self.mem_read(self.program_counter) as u16),

            AddressingMode::ZeroPage_X => Some(
                self.mem_read(self.program_counter)
                    .wrapping_add(self.register_x) as u16,
            ),

            AddressingMode::ZeroPage_Y => Some(
                self.mem_read(self.program_counter)
                    .wrapping_add(self.register_y) as u16,
            ),

            AddressingMode::Absolute => Some(self.mem_read_16(self.program_counter)),

            AddressingMode::Absolute_X => Some(
                self.mem_read_16(self.program_counter)
                    .wrapping_add(self.register_x as u16),
            ),

            AddressingMode::Absolute_Y => Some(
                self.mem_read_16(self.program_counter)
                    .wrapping_add(self.register_y as u16),
            ),

            AddressingMode::Indirect_X => {
                let base = self
                    .mem_read(self.program_counter)
                    .wrapping_add(self.register_x);
                let low = self.mem_read(base as u16) as u16;
                let high = self.mem_read(base.wrapping_add(1) as u16) as u16;

                self.program_counter += 2;

                Some(high << 8 | low)
            }

            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);
                let low = self.mem_read(base as u16) as u16;
                let high = self.mem_read(base as u16 + 1) as u16;

                let deref_base = high << 8 | low;

                self.program_counter += 2;

                Some(deref_base.wrapping_add(self.register_y as u16))
            }

            AddressingMode::Implied => None,
        };

        // Update program counter based on parameter length
        match mode {
            AddressingMode::Immediate
            | AddressingMode::ZeroPage
            | AddressingMode::ZeroPage_X
            | AddressingMode::ZeroPage_Y => {
                self.program_counter = self.program_counter.wrapping_add(1)
            }

            AddressingMode::Absolute
            | AddressingMode::Absolute_X
            | AddressingMode::Absolute_Y
            | AddressingMode::Indirect_X
            | AddressingMode::Indirect_Y => {
                self.program_counter = self.program_counter.wrapping_add(2)
            }

            AddressingMode::Implied => {}
        }

        addr
    }
}

/// Opcodes implementation
impl CPU {
    /// This instruction adds the contents of a memory location to the
    /// accumulator together with the carry bit. If overflow occurs the
    /// carry bit is set, this enables multiple byte addition to be performed
    ///
    /// The decimal mode is ignored
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Immediate        | 69     | 2     | 2                          |
    /// | Zero Page        | 65     | 2     | 3                          |
    /// | Zero Page, X     | 75     | 2     | 4                          |
    /// | Absolute         | 6D     | 3     | 4                          |
    /// | Absolute, X      | 7D     | 3     | 4 (+1 if page crossed)     |
    /// | Absolute, Y      | 79     | 3     | 4 (+1 if page crossed)     |
    /// | (Indirect, X)    | 61     | 2     | 6                          |
    /// | (Indirect), Y    | 71     | 2     | 5 (+1 if page crossed)     |
    fn adc(&mut self, mode: &AddressingMode) {
        let param_addr = self.get_parameters_address(mode).unwrap(/* safe */);
        let value = self.mem_read(param_addr);

        let sum = self.register_a as u16 + value as u16 + self.status.carry as u16;
        let result = sum as u8;

        self.status.carry = sum > 0xff;
        self.status.overflow = (value ^ result) & (result ^ self.register_a) & 0b10000000 != 0;
        self.set_register_a(result);
    }

    /// A logical AND is performed, bit by bit, on the accumulator contents using
    /// the contents of a byte of memory
    ///
    /// The decimal mode is ignored
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Immediate        | 29     | 2     | 2                          |
    /// | Zero Page        | 25     | 2     | 3                          |
    /// | Zero Page, X     | 35     | 2     | 4                          |
    /// | Absolute         | 2D     | 3     | 4                          |
    /// | Absolute, X      | 3D     | 3     | 4 (+1 if page crossed)     |
    /// | Absolute, Y      | 39     | 3     | 4 (+1 if page crossed)     |
    /// | (Indirect, X)    | 21     | 2     | 6                          |
    /// | (Indirect), Y    | 31     | 2     | 5 (+1 if page crossed)     |
    fn and(&mut self, mode: &AddressingMode) {
        let param_addr = self.get_parameters_address(mode).unwrap(/* safe */);
        let value = self.mem_read(param_addr);

        self.set_register_a(self.register_a & value);
    }

    /// This operation shifts all the bits of the accumulator or memory contents
    /// one bit left. Bit 0 is set to 0 and bit 7 is placed in the carry flag.
    /// The effect of this operation is to multiply the memory contents by 2
    /// (ignoring 2's complement considerations), setting the carry if the result
    /// will not fit in 8 bits
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Implied          | 0A     | 1     | 2                          |
    /// | Zero Page        | 06     | 2     | 5                          |
    /// | Zero Page, X     | 16     | 2     | 6                          |
    /// | Absolute         | 0E     | 3     | 6                          |
    /// | Absolute, X      | 1E     | 3     | 7                          |
    fn asl(&mut self, mode: &AddressingMode) {
        let value = match self.get_parameters_address(mode) {
            Some(param_addr) => self.mem_read(param_addr),
            None => 1, // Implied value
        };

        let res = (self.register_a as u16) << value;

        self.status.overflow = res > 0xff;
        self.set_register_a(res as u8);
    }

    /// If the condition is true then add the relative displacement to the
    /// program counter to cause a branch to a new location
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                    |
    /// |------------------|--------|-------|---------------------------|
    /// | Relative         | 90     | 2     | 2 (+1 branch,+2 new page) |
    /// | Relative         | B0     | 2     | 2 (+1 branch,+2 new page) |
    /// | Relative         | F0     | 2     | 2 (+1 branch,+2 new page) |
    /// | Relative         | 30     | 2     | 2 (+1 branch,+2 new page) |
    /// | Relative         | D0     | 2     | 2 (+1 branch,+2 new page) |
    /// | Relative         | 10     | 2     | 2 (+1 branch,+2 new page) |
    /// | Relative         | 50     | 2     | 2 (+1 branch,+2 new page) |
    /// | Relative         | 70     | 2     | 2 (+1 branch,+2 new page) |
    fn branch(&mut self, condition: bool) {
        let param_addr = self.get_parameters_address(&AddressingMode::Immediate).unwrap(/* safe */);
        let value: i8 = self.mem_read(param_addr) as i8;

        if condition {
            self.program_counter = self.program_counter.wrapping_add(value as u16);
        }
    }

    /// This instruction is used to test if one or more bits are set in a target
    /// memory location. The mask pattern in A is ANDed with the value in memory
    /// to set or clear the zero flag, but the result is not kept. Bits 7 and 6
    /// of the value from memory are copied into the N and V flags
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Zero Page        | 24     | 2     | 3                          |
    /// | Absolute         | 2C     | 3     | 4                          |
    fn bit(&mut self, mode: &AddressingMode) {
        let param_addr = self.get_parameters_address(mode).unwrap(/* safe */);
        let target = self.mem_read(param_addr);

        self.status.zero = self.register_a & target == 0;
        self.status.overflow = target & 0b01000000 == 0b01000000;
        self.status.negative = target & 0b10000000 == 0b10000000;
    }

    /// This instruction compares the contents of a register with another
    /// memory held value and sets the zero and carry flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode AXY  | Bytes | Cycles                     |
    /// |------------------|-------------|-------|----------------------------|
    /// | Immediate        | C9  E0  C0  | 2     | 2                          |
    /// | Zero Page        | C5  E4  C4  | 2     | 3                          |
    /// | Zero Page, X     | D5          | 2     | 4                          |
    /// | Absolute         | CD  EC  CC  | 3     | 4                          |
    /// | Absolute, X      | DD          | 3     | 4 (+1 if page crossed)     |
    /// | Absolute, Y      | D9          | 3     | 4 (+1 if page crossed)     |
    /// | (Indirect, X)    | C1          | 2     | 6                          |
    /// | (Indirect), Y    | D1          | 2     | 5 (+1 if page crossed)     |
    fn compare_register(&mut self, reg: u8, mode: &AddressingMode) {
        let param_addr = self.get_parameters_address(mode).unwrap(/* safe */);
        let target = self.mem_read(param_addr);
        let res: i8 = (reg as i8).wrapping_sub(target as i8);

        self.status.carry = res >= 0;
        self.status.zero = res == 0;
        self.status.negative = res < 0;
    }

    /// Subtracts one from the value held at a specified memory location setting the zero and negative flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Zero Page        | C6     | 2     | 5                          |
    /// | Zero Page, X     | D6     | 2     | 6                          |
    /// | Absolute         | CE     | 3     | 6                          |
    /// | Absolute, X      | DE     | 3     | 7                          |
    fn dec(&mut self, mode: &AddressingMode) {
        let param_addr = self.get_parameters_address(mode).unwrap(/* safe */);
        let value = self.mem_read(param_addr);
        let res = value.wrapping_sub(1);

        self.mem_write(param_addr, res);

        self.update_zero_and_negative_flags(res);
    }

    /// Subtracts one from the X register setting the zero and negative flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Implied          | CA     | 1     | 2                          |
    fn dex(&mut self) {
        let res = self.register_x.wrapping_sub(1);
        self.register_x = res;

        self.update_zero_and_negative_flags(res);
    }

    /// Subtracts one from the Y register setting the zero and negative flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Implied          | 88     | 1     | 2                          |
    fn dey(&mut self) {
        let res = self.register_y.wrapping_sub(1);
        self.register_y = res;

        self.update_zero_and_negative_flags(res);
    }

    /// An exclusive OR is performed, bit by bit, on the accumulator contents
    /// using the contents of a byte of memory
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Immediate        | 49     | 2     | 2                          |
    /// | Zero Page        | 45     | 2     | 3                          |
    /// | Zero Page, X     | 55     | 2     | 4                          |
    /// | Absolute         | 4D     | 3     | 4                          |
    /// | Absolute, X      | 5D     | 3     | 4 (+1 if page crossed)     |
    /// | Absolute, Y      | 59     | 3     | 4 (+1 if page crossed)     |
    /// | (Indirect, X)    | 41     | 2     | 6                          |
    /// | (Indirect), Y    | 51     | 2     | 5 (+1 if page crossed)     |
    fn eor(&mut self, mode: &AddressingMode) {
        let param_addr = self.get_parameters_address(mode).unwrap(/* safe */);
        let target = self.mem_read(param_addr);

        self.set_register_a(self.register_a ^ target);
    }

    /// Adds one to the X register setting the zero and negative flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles |
    /// |------------------|--------|-------|--------|
    /// | Implied          | E8     | 1     | 2      |
    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// Loads a byte of memory into the accumulator setting the zero and
    /// negative flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Immediate        | A9     | 2     | 2                          |
    /// | Zero Page        | A5     | 2     | 3                          |
    /// | Zero Page, X     | B5     | 2     | 4                          |
    /// | Absolute         | AD     | 3     | 4                          |
    /// | Absolute, X      | BD     | 3     | 4 (+1 if page crossed)     |
    /// | Absolute, Y      | B9     | 3     | 4 (+1 if page crossed)     |
    /// | (Indirect, X)    | A1     | 2     | 6                          |
    /// | (Indirect), Y    | B1     | 2     | 5 (+1 if page crossed)     |
    fn lda(&mut self, mode: &AddressingMode) {
        let param_addr = self.get_parameters_address(mode).unwrap(/* safe */);
        let value = self.mem_read(param_addr);

        self.set_register_a(value);
    }

    /// Loads a byte of memory into the X register setting the zero and negative
    /// flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Immediate        | A2     | 2     | 2                          |
    /// | Zero Page        | A6     | 2     | 3                          |
    /// | Zero Page, X     | B6     | 2     | 4                          |
    /// | Absolute         | AE     | 3     | 4                          |
    /// | Absolute, Y      | BE     | 3     | 4 (+1 if page crossed)     |
    fn ldx(&mut self, mode: &AddressingMode) {
        let param_addr = self.get_parameters_address(mode).unwrap(/* safe */);
        let value = self.mem_read(param_addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// Loads a byte of memory into the Y register setting the zero and negative
    /// flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Immediate        | A0     | 2     | 2                          |
    /// | Zero Page        | A4     | 2     | 3                          |
    /// | Zero Page, X     | B4     | 2     | 4                          |
    /// | Absolute         | AC     | 3     | 4                          |
    /// | Absolute, X      | BC     | 3     | 4 (+1 if page crossed)     |
    fn ldy(&mut self, mode: &AddressingMode) {
        let param_addr = self.get_parameters_address(mode).unwrap(/* safe */);
        let value = self.mem_read(param_addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);
    }

    /// Copies the current contents of the accumulator into the X register and
    /// sets the zero and negative flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles |
    /// |------------------|--------|-------|--------|
    /// | Implied          | AA     | 1     | 2      |
    fn tax(&mut self) {
        self.register_x = self.register_a;
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

    // Assembler test --------------------------------------------------------
    #[test]
    fn test_assembler() {
        let program = assemble6502!(
            lda #0x2
            tax
            lda 0x33,x
            brk
        );

        assert_eq!(&program, &[0xa9, 0x02, 0xaa, 0xb5, 0x33, 0x00]);
    }
    // Assembler test --------------------------------------------------------

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

    // LDA test --------------------------------------
    #[test]
    fn test_0xa9_lda_immediate() {
        let program = assemble6502!(
            lda #0x5
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x05);
        assert!(!cpu.status.zero);
        assert!(!cpu.status.negative);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let program = assemble6502!(
            lda #0x00
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);
        assert!(cpu.status.zero);
    }

    #[test]
    fn test_0xa9_lda_negative_flag() {
        let program = assemble6502!(
            lda #0xff
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);
        assert!(cpu.status.negative);
    }

    #[test]
    fn test_0xa5_lda_zero_page() {
        let program = assemble6502!(
            lda 0x33
            brk
        );
        let mut cpu = CPU::new();
        cpu.mem_write(0x33, 0x0a);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x0a);
    }

    #[test]
    fn test_0xb5_lda_zero_page_x() {
        let program = assemble6502!(
            lda #0x2
            tax
            lda 0x33,x
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write(0x35, 0x0a);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x0a);
    }

    #[test]
    fn test_0xad_lda_absolute() {
        let program = assemble6502!(
            lda abs 0x1234
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write(0x1234, 0x08);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x08);
    }

    #[test]
    fn test_0xbd_lda_absolute_x() {
        let program = assemble6502!(
            ldx #0x1
            lda abs 0x1234,x
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write(0x1235, 0x08);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x08);
    }

    #[test]
    fn test_0xbd_lda_absolute_y() {
        let program = assemble6502!(
            ldy #0x1
            lda abs 0x1234,y
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write(0x1235, 0x08);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x08);
    }
    // LDA test --------------------------------------

    // TAX test --------------------------------------
    #[test]
    fn test_0xaa_tax() {
        let program = assemble6502!(
            lda #0x11
            tax
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);
        assert_eq!(cpu.register_a, 0x11);
        assert_eq!(cpu.register_x, 0x11);
        assert!(!cpu.status.zero);
        assert!(!cpu.status.negative);
    }

    #[test]
    fn test_0xaa_tax_zero_flag() {
        let program = assemble6502!(
            lda #0x00
            tax
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);
        assert!(cpu.status.zero);
    }
    // TAX test --------------------------------------

    // INX test --------------------------------------
    #[test]
    fn test_0xe8_inx() {
        let program = assemble6502!(
            lda #0x04
            tax
            inx
            brk
        );
        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_x, 5)
    }
    #[test]
    fn test_0xe8_inx_overflow() {
        let program = assemble6502!(
            lda #0xff
            tax
            inx
            inx
            brk
        );
        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_x, 1)
    }
    // INX test --------------------------------------

    // Basic program test -------------------------------
    #[test]
    fn test_5_ops_working_together() {
        let program = assemble6502!(
            lda #0xc0
            tax
            inx
            brk
        );
        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_x, 0xc1)
    }
    // Basic program test -------------------------------

    // ADC test ----------------------------------------
    #[test]
    fn test_0x69_adc_immediate() {
        let program = assemble6502!(
            adc #0x04
            adc #0x04
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x08);
        assert!(!cpu.status.carry);
        assert!(!cpu.status.zero);
        assert!(!cpu.status.overflow);
        assert!(!cpu.status.negative);
    }

    #[test]
    fn test_0x69_adc_overflow() {
        let program = assemble6502!(
            adc #0xff
            adc #0x04
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x03);
        assert!(cpu.status.carry);
        assert!(!cpu.status.overflow);
        assert!(!cpu.status.zero);
        assert!(!cpu.status.negative);
    }

    #[test]
    fn test_0x69_adc_negative() {
        let program = assemble6502!(
            adc #0xff
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0xff);
        assert!(!cpu.status.carry);
        assert!(!cpu.status.overflow);
        assert!(!cpu.status.zero);
        assert!(cpu.status.negative);
    }

    #[test]
    fn test_0x69_adc_overflow_zero() {
        let program = assemble6502!(
            lda #0xff
            adc #0x01
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x00);
        assert!(cpu.status.carry);
        assert!(cpu.status.zero);
        assert!(!cpu.status.overflow);
        assert!(!cpu.status.negative);
    }

    #[test]
    fn test_0x65_adc_zero_page() {
        let program = assemble6502!(
            lda #0x01
            adc 0xc4
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write(0xc4, 0x09);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x0a);
    }

    #[test]
    fn test_0x75_adc_zero_page_x() {
        let program = assemble6502!(
            lda #0x01
            ldx #0x03
            adc 0xc4,x
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write(0xc7, 0x09);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x0a);
    }

    #[test]
    fn test_0x6d_adc_absolute() {
        let program = assemble6502!(
            lda #0x01
            adc abs 0x1234
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write_16(0x1234, 0x03);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x04);
    }

    #[test]
    fn test_0x7d_adc_absolute_x() {
        let program = assemble6502!(
            ldx #0x03
            lda #0x01
            adc abs 0x1234,x
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write_16(0x1237, 0x03);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x04);
    }

    #[test]
    fn test_0x79_adc_absolute_y() {
        let program = assemble6502!(
            ldy #0x03
            lda #0x01
            adc abs 0x1234,y
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write_16(0x1237, 0x03);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x04);
    }

    #[test]
    fn test_0x61_adc_indirect_x() {
        let program = assemble6502!(
            ldx #0x03
            lda #0x01
            adc (0x12,x)
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write_16(0x15, 0x1234);
        cpu.mem_write_16(0x1234, 0x03);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x04);
    }

    #[test]
    fn test_0x71_adc_indirect_y() {
        let program = assemble6502!(
            ldy #0x03
            lda #0x01
            adc (0x12),y
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write_16(0x12, 0x1234);
        cpu.mem_write_16(0x1237, 0x03);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x04);
    }
    // ADC test ----------------------------------------

    // AND test ----------------------------------------
    #[test]
    fn test_0x29_and_immediate() {
        let program = assemble6502!(
            lda #0b11111110
            and #0b00000011
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x02);
    }

    #[test]
    fn test_0x25_and_zero_page() {
        let program = assemble6502!(
            lda #0xff
            and 0xc4
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write(0xc4, 0b10000000);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0b10000000);
    }

    #[test]
    fn test_0x35_and_zero_page_x() {
        let program = assemble6502!(
            lda #0xff
            ldx #0x03
            and 0xc4,x
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write(0xc7, 0b10101010);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0b10101010);
    }

    #[test]
    fn test_0x2d_and_absolute() {
        let program = assemble6502!(
            lda #0xff
            and abs 0x1234
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write_16(0x1234, 0b10101010);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0b10101010);
    }

    #[test]
    fn test_0x3d_and_absolute_x() {
        let program = assemble6502!(
            ldx #0x03
            lda #0xff
            and abs 0x1234,x
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write_16(0x1237, 0b10101010);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0b10101010);
    }

    #[test]
    fn test_0x39_and_absolute_y() {
        let program = assemble6502!(
            ldy #0x03
            lda #0xff
            and abs 0x1234,y
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write_16(0x1237, 0b10101010);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0b10101010);
    }

    #[test]
    fn test_0x21_and_indirect_x() {
        let program = assemble6502!(
            ldx #0x03
            lda #0xff
            and (0x12,x)
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write_16(0x15, 0x1234);
        cpu.mem_write_16(0x1234, 0b10101010);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0b10101010);
    }

    #[test]
    fn test_0x31_and_indirect_y() {
        let program = assemble6502!(
            ldy #0x03
            lda #0xff
            and (0x12),y
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write_16(0x12, 0x1234);
        cpu.mem_write_16(0x1237, 0b10101010);
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0b10101010);
    }
    // AND test ----------------------------------------

    // ASL test ----------------------------------------
    #[test]
    fn test_0x0a_asl_implied() {
        let program = assemble6502!(
            lda #0x02
            asl a
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x04);

        assert!(!cpu.status.overflow)
    }

    #[test]
    fn test_0x0a_asl_overflow() {
        let program = assemble6502!(
            lda #0b10000000
            asl a
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x0);

        assert!(cpu.status.overflow)
    }
    // ASL test ----------------------------------------

    // BCC test ----------------------------------------
    #[test]
    fn test_0x90_bcc() {
        let program = assemble6502!(
            lda #0xfe
            start:
              adc #0x01
              bcc start
            brk
        );

        assert_eq!(&program, &[0xa9, 0xfe, 0x69, 0x01, 0x90, 0xfc, 0x00]);

        let mut cpu = CPU::new();
        cpu.load(&program);
        cpu.reset();
        let pc = cpu.program_counter;
        assert!(!cpu.status.carry);
        cpu.run();

        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.program_counter, pc + 0x07);

        assert!(cpu.status.carry)
    }
    // BCC test ----------------------------------------

    // CMP test ----------------------------------------
    #[test]
    fn test_0xc9_cmp_immediate_ge() {
        let program = assemble6502!(
            lda #0x01
            cmp #0x00
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert!(cpu.status.carry);
        assert!(!cpu.status.zero);
        assert!(!cpu.status.negative);
    }

    #[test]
    fn test_0xc9_cmp_immediate_e() {
        let program = assemble6502!(
            lda #0x01
            cmp #0x01
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert!(cpu.status.carry);
        assert!(cpu.status.zero);
        assert!(!cpu.status.negative);
    }

    #[test]
    fn test_0xc9_cmp_immediate_l() {
        let program = assemble6502!(
            lda #0x01
            cmp #0x03
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert!(!cpu.status.carry);
        assert!(!cpu.status.zero);
        assert!(cpu.status.negative);
    }
    // CMP test ----------------------------------------

    // DEC test ----------------------------------------
    #[test]
    fn test_0xce_dec_absolute() {
        let program = assemble6502!(
            dec abs 0x1234
            brk
        );

        let mut cpu = CPU::new();
        cpu.mem_write(0x1234, 0x3);
        cpu.load_and_run(&program);

        assert_eq!(cpu.mem_read(0x1234), 0x2);
    }
    // DEC test ----------------------------------------

    // EOR test ----------------------------------------
    #[test]
    fn test_0x49_eor_immediate() {
        let program = assemble6502!(
            lda #0b10101010
            eor #0b01010101
            brk
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0xff);
    }
    // EOR test ----------------------------------------
}
