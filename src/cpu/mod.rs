mod bus;
mod opcodes;
use bus::Bus;
use opcodes::{AddressingMode, Code};
use std::{
    fmt::Debug,
    io::{Write, stdin, stdout},
};

const STACK: u16 = 0x0100;
const STACK_RESET: u8 = 0xff;

pub struct CPU {
    register_a: u8,
    register_x: u8,
    register_y: u8,
    status: Status,
    program_counter: u16,
    stack_pointer: u8,
    bus: Bus,
}

impl Debug for CPU {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let code = self.mem_read(self.program_counter);
        let opcode = opcodes::CPU_OPCODES
            .get(&code)
            .unwrap_or_else(|| panic!("Invalid opcode: {:X}", code));

        let mut s = String::new();
        let _ = stdout().flush();
        stdin()
            .read_line(&mut s)
            .expect("Did not enter a correct string");

        writeln!(
            f,
            "Next instruction: {:02X?}|{:?}",
            opcode.code, opcode.mode
        )?;
        writeln!(
            f,
            "PC:{:04X?} SP:{:02X?} {:08b}",
            self.program_counter,
            self.stack_pointer,
            self.status.as_byte()
        )?;
        writeln!(
            f,
            "A:{:02X?} X:{:02X?} Y:{:02X?}",
            self.register_a, self.register_x, self.register_y
        )?;

        for col in (0x0000_u16..=0x0100_u16).step_by(0x10) {
            write!(f, "{:04X}:  ", col)?;
            for row in 0x0_u8..=0xf_u8 {
                write!(f, "{:02X} ", self.mem_read(col + row as u16))?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

pub trait Memory {
    /// Reads a byte from memory
    fn mem_read(&self, addr: u16) -> u8;

    /// Writes a byte to memory
    fn mem_write(&mut self, addr: u16, data: u8);

    /// Writes a 16-bit value to memory (little endian)
    fn mem_write_16(&mut self, addr: u16, data: u16) {
        // Little endian
        let high = (data & 0xFF) as u8;
        let low = (data >> 8) as u8;

        self.mem_write(addr, high);
        self.mem_write(addr.wrapping_add(1), low);
    }

    /// Reads a 16-bit value from memory (little endian)
    fn mem_read_16(&self, addr: u16) -> u16 {
        // Little endian
        let low = self.mem_read(addr) as u16;
        let high = self.mem_read(addr.wrapping_add(1)) as u16;

        high << 8 | low
    }
}

impl Memory for CPU {
    fn mem_read(&self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.mem_write(addr, data)
    }

    fn mem_read_16(&self, addr: u16) -> u16 {
        self.bus.mem_read_16(addr)
    }

    fn mem_write_16(&mut self, addr: u16, data: u16) {
        self.bus.mem_write_16(addr, data);
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
            stack_pointer: STACK_RESET,
            bus: Bus::new(),
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
    pub fn load(&mut self, program: &[u8]) {
        let program_start = 0x0000;
        // self.memory[program_start..(program_start + program.len())].copy_from_slice(program);
        for i in 0..(program.len() as u16) {
            self.mem_write(program_start + i, program[i as usize]);
        }

        // Write the program start in 0xFFFC
        self.mem_write_16(0xFFFC, program_start);
    }

    /// Resets the CPU registers and status bits, then loads the program start address from 0xFFFC
    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.status = Status::new();

        // Load program start from 0xFFFC
        self.program_counter = self.mem_read_16(0xFFFC);
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    /// Runs the CPU and a callback before each opcode execution
    ///
    /// # Panics
    /// If an invalid opcode is encountered, the CPU will panic.
    pub fn run_with_callback<F: FnMut(&mut CPU)>(&mut self, mut callback: F) {
        #[cfg(debug_assertions)]
        {
            println!("Running in debug mode (to execute normally use 'cargo run --release'):");
            println!("Press enter to step to the next instruction");
        }

        loop {
            callback(self);

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
                Code::INC => self.inc(&opcode.mode),
                Code::INX => self.inx(),
                Code::INY => self.iny(),
                Code::JMP => self.jmp(&opcode.mode),
                Code::JSR => self.jsr(),
                Code::LDA => self.lda(&opcode.mode),
                Code::LDX => self.ldx(&opcode.mode),
                Code::LDY => self.ldy(&opcode.mode),
                Code::LSR => self.lsr(&opcode.mode),
                Code::NOP => continue,
                Code::ORA => self.ora(&opcode.mode),
                Code::PHA => self.stack_push(self.register_a),
                Code::PHP => {
                    self.stack_push(self.status.as_byte());
                    self.status.brk = true
                }
                Code::PLA => self.pla(),
                Code::PLP => self.plp(),
                Code::ROL => self.rol(&opcode.mode),
                Code::ROR => self.ror(&opcode.mode),
                Code::RTI => self.rti(),
                Code::RTS => self.rts(),
                Code::SBC => self.sbc(&opcode.mode),
                Code::SEC => self.status.carry = true,
                Code::SED => self.status.decimal = true,
                Code::SEI => self.status.interrupt_disable = true,
                Code::STA => self.store_reg(&opcode.mode, self.register_a),
                Code::STX => self.store_reg(&opcode.mode, self.register_x),
                Code::STY => self.store_reg(&opcode.mode, self.register_y),
                Code::TAX => self.tax(),
                Code::TAY => self.tay(),
                Code::TSX => self.tsx(),
                Code::TXA => self.set_register_a(self.register_x),
                Code::TXS => self.stack_pointer = self.register_x,
                Code::TYA => self.set_register_a(self.register_y),
            }

            // Step run on user input
            #[cfg(debug_assertions)]
            println!("{:?}", self);
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

    /// Push a byte to the stack
    fn stack_push(&mut self, value: u8) {
        if self.stack_pointer.checked_sub(1).is_none() {
            panic!("Stack overflow")
        }

        self.mem_write(STACK + self.stack_pointer as u16, value);
        self.stack_pointer -= 1;
    }

    // Push a 16 bit value to the stack
    fn stack_push_16(&mut self, value: u16) {
        let high = (value >> 8) as u8;
        let low = (value & 0xff) as u8;

        self.stack_push(high);
        self.stack_push(low);
    }

    /// Pop a byte from the stack
    fn stack_pop(&mut self) -> u8 {
        if self.stack_pointer.checked_add(1).is_none() {
            panic!("Stack overflow")
        }
        let res = self.mem_read(STACK + self.stack_pointer as u16 + 1);
        self.stack_pointer += 1;
        res
    }

    /// Pop a 16 bit value from the stack
    fn stack_pop_16(&mut self) -> u16 {
        let low = self.stack_pop() as u16;
        let high = self.stack_pop() as u16;

        high << 8 | low
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

            AddressingMode::Indirect => {
                let base = self.mem_read(self.program_counter);
                let low = self.mem_read(base as u16) as u16;
                let high = self.mem_read(base as u16 + 1) as u16;

                let deref_base = high << 8 | low;

                Some(deref_base)
            }

            AddressingMode::Indirect_X => {
                let base = self
                    .mem_read(self.program_counter)
                    .wrapping_add(self.register_x);
                let low = self.mem_read(base as u16) as u16;
                let high = self.mem_read(base.wrapping_add(1) as u16) as u16;

                Some(high << 8 | low)
            }

            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);
                let low = self.mem_read(base as u16) as u16;
                let high = self.mem_read(base as u16 + 1) as u16;

                let deref_base = high << 8 | low;

                Some(deref_base.wrapping_add(self.register_y as u16))
            }

            AddressingMode::Implied => None,
        };

        // Update program counter based on parameter length
        match mode {
            AddressingMode::Immediate
            | AddressingMode::ZeroPage
            | AddressingMode::ZeroPage_X
            | AddressingMode::ZeroPage_Y
            | AddressingMode::Indirect_X
            | AddressingMode::Indirect_Y => {
                self.program_counter = self.program_counter.wrapping_add(1)
            }

            AddressingMode::Absolute
            | AddressingMode::Absolute_X
            | AddressingMode::Absolute_Y
            | AddressingMode::Indirect => {
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

    /// Adds one to the value held at a specified memory location setting the
    /// zero and negative flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Zero Page        | E6     | 2     | 5                          |
    /// | Zero Page, X     | F6     | 2     | 6                          |
    /// | Absolute         | EE     | 3     | 6                          |
    /// | Absolute, X      | FE     | 3     | 7                          |
    fn inc(&mut self, mode: &AddressingMode) {
        let param_addr = self.get_parameters_address(mode).unwrap(/* safe */);
        let value = self.mem_read(param_addr);
        let res = value.wrapping_add(1);

        self.mem_write(param_addr, res);

        self.update_zero_and_negative_flags(res);
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

    /// Adds one to the Y register setting the zero and negative flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles |
    /// |------------------|--------|-------|--------|
    /// | Implied          | C8     | 1     | 2      |
    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    /// Sets the program counter to the address specified by the operand
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Absolute         | 4C     | 3     | 3                          |
    /// | Indirect         | 6C     | 3     | 5                          |
    fn jmp(&mut self, mode: &AddressingMode) {
        let target_addr = self.get_parameters_address(mode).unwrap(/* safe */);

        self.program_counter = target_addr;
    }

    /// The JSR instruction pushes the address (minus one) of the return point
    /// on to the stack and then sets the program counter to the target memory address
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Absolute         | 20     | 3     | 6                          |
    fn jsr(&mut self) {
        let target_addr = self.get_parameters_address(&AddressingMode::Absolute).unwrap(/* safe */);
        self.stack_push_16(self.program_counter.wrapping_sub(1));

        self.program_counter = target_addr;
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

    /// Each of the bits in A or M is shifted one place to the right. The bit that
    /// was in bit 0 is shifted into the carry flag. Bit 7 is set to zero
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Implied          | 4A     | 1     | 2                          |
    /// | Zero Page        | 46     | 2     | 5                          |
    /// | Zero Page, X     | 56     | 2     | 6                          |
    /// | Absolute         | 4E     | 3     | 6                          |
    /// | Absolute, X      | 5E     | 3     | 7                          |
    fn lsr(&mut self, mode: &AddressingMode) {
        match self.get_parameters_address(mode) {
            Some(param_addr) => {
                let mem_value = self.mem_read(param_addr);

                self.status.carry = mem_value & 0x01 == 0x01;

                let res = mem_value >> 1;
                self.mem_write(param_addr, res);

                self.update_zero_and_negative_flags(res);
            }
            None => {
                self.status.carry = self.register_a & 0x01 == 0x01;

                self.register_a >>= 1;
                self.update_zero_and_negative_flags(self.register_a);
            }
        }
    }

    /// An inclusive OR is performed, bit by bit, on the accumulator contents
    /// using the contents of a byte of memory
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Immediate        | 09     | 2     | 2                          |
    /// | Zero Page        | 05     | 2     | 3                          |
    /// | Zero Page, X     | 15     | 2     | 4                          |
    /// | Absolute         | 0D     | 3     | 4                          |
    /// | Absolute, X      | 1D     | 3     | 4 (+1 if page crossed)     |
    /// | Absolute, Y      | 19     | 3     | 4 (+1 if page crossed)     |
    /// | (Indirect, X)    | 01     | 2     | 6                          |
    /// | (Indirect), Y    | 11     | 2     | 5 (+1 if page crossed)     |
    fn ora(&mut self, mode: &AddressingMode) {
        let param_addr = self.get_parameters_address(mode).unwrap(/* safe */);
        let target = self.mem_read(param_addr);

        self.set_register_a(self.register_a | target);
    }

    /// Pulls an 8 bit value from the stack and into the accumulator. The zero
    /// and negative flags are set as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles |
    /// |------------------|--------|-------|--------|
    /// | Implied          | 68     | 1     | 4      |
    fn pla(&mut self) {
        let stack_value = self.stack_pop();
        self.set_register_a(stack_value);
    }

    /// Pulls an 8 bit value from the stack and into the processor flags.
    /// The flags will take on new states as determined by the value pulled
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles |
    /// |------------------|--------|-------|--------|
    /// | Implied          | 28     | 1     | 4      |
    fn plp(&mut self) {
        let stack_value = self.stack_pop();
        self.status.set_from_byte(stack_value);
        self.status.brk = false;
        self.update_zero_and_negative_flags(stack_value);
    }

    /// Move each of the bits in either A or M one place to the left. Bit 0 is
    /// filled with the current value of the carry flag whilst the old bit 7
    /// becomes the new carry flag value
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Accumulator      | 2A     | 1     | 2                          |
    /// | Zero Page        | 26     | 2     | 5                          |
    /// | Zero Page, X     | 36     | 2     | 6                          |
    /// | Absolute         | 2E     | 3     | 6                          |
    /// | Absolute, X      | 3E     | 3     | 7                          |
    fn rol(&mut self, mode: &AddressingMode) {
        match self.get_parameters_address(mode) {
            Some(param_addr) => {
                let value = self.mem_read(param_addr);

                self.status.carry = value & 0b10000000 == 0b10000000;
                let res = (value << 1) + self.status.carry as u8;

                self.mem_write(param_addr, res);
                self.update_zero_and_negative_flags(res);
            }
            None => {
                let value = self.register_a;

                self.status.carry = value & 0b10000000 == 0b10000000;
                let res = (value << 1) + self.status.carry as u8;

                self.set_register_a(res);
            }
        };
    }

    /// Move each of the bits in either A or M one place to the right. Bit 7 is
    /// filled with the current value of the carry flag whilst the old bit 0
    /// becomes the new carry flag value
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Accumulator      | 6A     | 1     | 2                          |
    /// | Zero Page        | 66     | 2     | 5                          |
    /// | Zero Page, X     | 76     | 2     | 6                          |
    /// | Absolute         | 6E     | 3     | 6                          |
    /// | Absolute, X      | 7E     | 3     | 7                          |
    fn ror(&mut self, mode: &AddressingMode) {
        match self.get_parameters_address(mode) {
            Some(param_addr) => {
                let value = self.mem_read(param_addr);

                self.status.carry = value & 0x01 == 0x01;
                let res = (value >> 1) | (self.status.carry as u8) << 7;

                self.mem_write(param_addr, res);
                self.update_zero_and_negative_flags(res);
            }
            None => {
                let value = self.register_a;

                self.status.carry = value & 0x01 == 0x01;
                let res = (value >> 1) | (self.status.carry as u8) << 7;

                self.set_register_a(res);
            }
        };
    }

    /// The RTI instruction is used at the end of an interrupt processing routine.
    /// It pulls the processor flags from the stack followed by the program counter
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Implied          | 40     | 1     | 6                          |
    fn rti(&mut self) {
        self.plp(); // Pop cpu status
        let target_addr = self.stack_pop_16();

        self.status.brk = false;
        self.program_counter = target_addr;
    }

    /// The RTS instruction is used at the end of a subroutine to return to the
    /// calling routine. It pulls the program counter (minus one) from the stack
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Implied          | 60     | 1     | 6                          |
    fn rts(&mut self) {
        let target_addr = self.stack_pop_16() + 1;
        self.program_counter = target_addr;
    }

    /// This instruction subtracts the contents of a memory location to the
    /// accumulator together with the not of the carry bit. If overflow occurs
    /// the carry bit is clear, this enables multiple byte subtraction to be performed
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Immediate        | E9     | 2     | 2                          |
    /// | Zero Page        | E5     | 2     | 3                          |
    /// | Zero Page, X     | F5     | 2     | 4                          |
    /// | Absolute         | ED     | 3     | 4                          |
    /// | Absolute, X      | FD     | 3     | 4 (+1 if page crossed)     |
    /// | Absolute, Y      | F9     | 3     | 4 (+1 if page crossed)     |
    /// | (Indirect, X)    | E1     | 2     | 6                          |
    /// | (Indirect), Y    | F1     | 2     | 5 (+1 if page crossed)     |
    fn sbc(&mut self, mode: &AddressingMode) {
        let param_addr = self.get_parameters_address(mode).unwrap(/* safe */);
        let value = -(self.mem_read(param_addr) as i8) as u8;

        let sum = self.register_a as u16 + value as u16 - (1 - self.status.carry as u16);
        let result = sum as u8;

        self.status.carry = sum > 0xff;
        self.status.overflow = (value ^ result) & (result ^ self.register_a) & 0b10000000 != 0;
        self.set_register_a(result);
    }

    /// Stores the contents of a register into memory
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode AXY | Bytes | Cycles                     |
    /// |------------------|------------|-------|----------------------------|
    /// | Zero Page        | 85  86  84 | 2     | 3                          |
    /// | Zero Page, X     | 95      94 | 2     | 4                          |
    /// | Zero Page, Y     |     96     | 2     | 4                          |
    /// | Absolute         | 8D  8E  8C | 3     | 4                          |
    /// | Absolute, X      | 9D         | 3     | 5                          |
    /// | Absolute, Y      | 99         | 3     | 5                          |
    /// | (Indirect, X)    | 81         | 2     | 6                          |
    /// | (Indirect), Y    | 91         | 2     | 6                          |
    fn store_reg(&mut self, mode: &AddressingMode, reg: u8) {
        let param_addr = self.get_parameters_address(mode).unwrap(/* safe */);
        self.mem_write(param_addr, reg);
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

    /// Copies the current contents of the accumulator into the Y register and
    /// sets the zero and negative flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles |
    /// |------------------|--------|-------|--------|
    /// | Implied          | A8     | 1     | 2      |
    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    /// Copies the current contents of the stack register into the X register
    /// and sets the zero and negative flags as appropriate
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles |
    /// |------------------|--------|-------|--------|
    /// | Implied          | BA     | 1     | 2      |
    fn tsx(&mut self) {
        self.register_x = self.stack_pointer;
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
#[derive(Debug)]
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
            brk: false,
            decimal: false,
            interrupt_disable: false,
            zero: false,
            carry: false,
        }
    }

    fn as_byte(&self) -> u8 {
        (self.negative as u8) << 7
            | (self.overflow as u8) << 6
            | 0x1 << 5
            | (self.brk as u8) << 4
            | (self.decimal as u8) << 3
            | (self.interrupt_disable as u8) << 2
            | (self.zero as u8) << 1
            | self.carry as u8
    }

    fn set_from_byte(&mut self, byte: u8) {
        self.negative = byte & 0b10000000 == 0b10000000;
        self.overflow = byte & 0b01000000 == 0b01000000;
        self.brk = byte & 0b00010000 == 0b00010000;
        self.decimal = byte & 0b00001000 == 0b00001000;
        self.interrupt_disable = byte & 0b00000100 == 0b00000100;
        self.zero = byte & 0b00000010 == 0b00000010;
        self.carry = byte & 0b00000001 == 0b00000001;
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
        assert_eq!(cpu.mem_read(0x1234), 0x69)
    }

    #[test]
    fn test_mem_write_16() {
        let mut cpu = CPU::new();
        cpu.mem_write_16(0x1234, 0x420);
        assert_eq!(cpu.mem_read(0x1234), 0x20);
        assert_eq!(cpu.mem_read(0x1235), 0x04)
    }

    #[test]
    fn test_mem_read() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x1234, 0x69);
        assert_eq!(cpu.mem_read(0x1234), 0x69)
    }

    #[test]
    fn test_mem_read_16() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x1234, 0x20);
        cpu.mem_write(0x1235, 0x04);
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

    // JMP test ----------------------------------------
    #[test]
    fn test_0x4c_jmp_absolute() {
        let mut program = assemble6502!(
            jmp start
            lda #0xfe
            adc #0x01
            start:
              lda #0x05
              brk
        );

        let mut cpu = CPU::new();
        cpu.load(&program);
        cpu.reset();

        // Update program start offset
        let program_start = cpu.mem_read_16(0xFFFC);
        let program_jmp = program_start + ((program[2] as u16) << 8 | program[1] as u16);
        program[1] = program_jmp as u8;
        program[2] = (program_jmp >> 8) as u8;

        cpu.load(&program);
        cpu.reset();
        let pc = cpu.program_counter;
        cpu.run();

        assert!(!cpu.status.carry);
        assert_eq!(cpu.register_a, 0x05);
        assert_eq!(cpu.program_counter, pc + 10);
    }
    // JMP test ----------------------------------------

    // Stack test --------------------------------------
    #[test]
    fn test_stack_push() {
        let mut cpu = CPU::new();

        assert_eq!(cpu.stack_pointer, STACK_RESET);
        cpu.stack_push(0x69);
        assert_eq!(cpu.stack_pointer, STACK_RESET - 1);

        assert_eq!(cpu.mem_read(STACK + (cpu.stack_pointer as u16 + 1)), 0x69)
    }

    #[test]
    fn test_stack_pop() {
        let mut cpu = CPU::new();

        cpu.stack_push(0x69);
        assert_eq!(cpu.stack_pointer, STACK_RESET - 1);
        assert_eq!(cpu.stack_pop(), 0x69);
        assert_eq!(cpu.stack_pointer, STACK_RESET);
    }

    #[test]
    #[should_panic]
    fn test_stack_overflow() {
        let mut cpu = CPU::new();

        for i in 0..STACK_RESET {
            cpu.stack_push(i);
        }

        assert_eq!(cpu.stack_pointer, 0x00);

        cpu.stack_push(0xff); // Stack overflow
    }

    #[test]
    #[should_panic]
    fn test_stack_underflow() {
        let mut cpu = CPU::new();
        cpu.stack_pop(); // Stack underflow
    }
    // Stack test --------------------------------------

    // JSR test ----------------------------------------
    #[test]
    fn test_0x20_jsr_absolute() {
        let mut program = assemble6502!(
            jsr end
            lda #0x01
            end:
              brk
        );

        let mut cpu = CPU::new();
        cpu.load(&program);
        cpu.reset();

        // Update program start offset
        let program_start = cpu.mem_read_16(0xFFFC);
        let program_jmp = program_start + ((program[2] as u16) << 8 | program[1] as u16);
        program[1] = program_jmp as u8;
        program[2] = (program_jmp >> 8) as u8;

        cpu.load(&program);
        cpu.reset();
        cpu.program_counter += 1;
        cpu.jsr();
        cpu.run();
        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.program_counter, cpu.mem_read_16(0xFFFC) + 0x06);
        assert_eq!(cpu.stack_pop_16(), cpu.mem_read_16(0xFFFC) + 0x02);
    }
    // JSR test ----------------------------------------

    // LSR test ----------------------------------------
    #[test]
    fn test_0x4a_lsr_implied() {
        let program = assemble6502!(
           lda #0x02
           lsr a
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x01);
        assert!(!cpu.status.carry);
    }

    #[test]
    fn test_0x4e_lsr_absolute() {
        let program = assemble6502!(
           lsr abs 0x1234
        );

        let mut cpu = CPU::new();
        cpu.mem_write(0x1234, 0x03);
        cpu.load_and_run(&program);

        assert_eq!(cpu.mem_read(0x1234), 0x01);
        assert!(cpu.status.carry);
    }
    // LSR test ----------------------------------------

    // PHA test ----------------------------------------
    #[test]
    fn test_0x48_pha() {
        let program = assemble6502!(
            lda #0x69
            pha
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.stack_pop(), 0x69);
    }
    // PHA test ----------------------------------------

    // Status as/from byte test -----------------------------
    #[test]
    fn test_status_as_byte() {
        let mut cpu = CPU::new();
        cpu.status.negative = true;
        cpu.status.carry = true;
        cpu.status.brk = true;

        assert_eq!(cpu.status.as_byte(), 0b10110001);
    }

    #[test]
    fn test_status_set_from_byte() {
        let mut cpu = CPU::new();

        cpu.status.set_from_byte(0b10110001);

        assert!(cpu.status.negative);
        assert!(cpu.status.carry);
        assert!(cpu.status.brk);
        assert!(!cpu.status.interrupt_disable);
        assert!(!cpu.status.overflow);
        assert!(!cpu.status.decimal);
        assert!(!cpu.status.zero);
    }
    // Status as/from byte test -----------------------------

    // PHP test ---------------------------------------------
    #[test]
    fn test_0x08_php() {
        let program = assemble6502!(
            lda #0x02
            cmp #0x02
            php
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.stack_pop(), 0b00100011)
    }
    // PHP test ---------------------------------------------

    // ROL test ---------------------------------------------
    #[test]
    fn test_0x2a_rol_accumulator() {
        let program = assemble6502!(
            lda #0b11000000
            rol a
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0b10000001);
        assert!(cpu.status.carry);
    }

    #[test]
    fn test_0x2a_rol_accumulator_2() {
        let program = assemble6502!(
            lda #0b11000000
            rol a
            rol a
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0b00000011);
        assert!(cpu.status.carry);
    }
    // ROL test ---------------------------------------------

    // ROR test ---------------------------------------------
    #[test]
    fn test_0x6a_ror_accumulator() {
        let program = assemble6502!(
            lda #0b00000011
            ror a
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0b10000001);
        assert!(cpu.status.carry);
    }

    #[test]
    fn test_0x6a_ror_accumulator_2() {
        let program = assemble6502!(
            lda #0b00000011
            ror a
            ror a
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0b11000000);
        assert!(cpu.status.carry);
    }
    // ROR test ---------------------------------------------

    // RTI test ---------------------------------------------
    #[test]
    fn test_0x40_rti() {
        let program = assemble6502!(
            php
            rti
            lda #0x01
            brk
        );

        let mut cpu = CPU::new();
        cpu.load(&program);
        cpu.reset();
        cpu.stack_push_16(cpu.program_counter + 4); // Push pc first
        cpu.run(); // Then rti

        assert_eq!(cpu.register_a, 0x00);
    }
    // RTI test ---------------------------------------------

    // RTS test ---------------------------------------------
    #[test]
    fn test_0x60_rts() {
        let mut program = assemble6502!(
            jsr load
            brk
            load:
              lda #0x01
              rts
        );

        let mut cpu = CPU::new();
        cpu.load(&program);
        cpu.reset();

        // Update program start offset
        let program_start = cpu.mem_read_16(0xFFFC);
        let program_jmp = program_start + ((program[2] as u16) << 8 | program[1] as u16);
        program[1] = program_jmp as u8;
        program[2] = (program_jmp >> 8) as u8;

        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0x01);
        assert_eq!(cpu.program_counter, cpu.mem_read_16(0xFFFC) + 0x04);
    }
    // RTS test ---------------------------------------------

    // SBC test ----------------------------------------
    #[test]
    fn test_0xe9_sbc_immediate() {
        let program = assemble6502!(
            lda #0x05
            sbc #0x04
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
    fn test_0xe9_sbc_overflow() {
        let program = assemble6502!(
            sbc #0x04
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_a, 0xfb);
        assert!(!cpu.status.carry);
        assert!(!cpu.status.overflow);
        assert!(!cpu.status.zero);
        assert!(cpu.status.negative);
    }
    // SBC test ----------------------------------------

    // Store reg test ----------------------------------
    #[test]
    fn test_0x85_sta_zero_page() {
        let program = assemble6502!(
            lda #0x69
            sta 0x11
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.mem_read(0x0011), 0x69)
    }

    #[test]
    fn test_0x85_sta_absolute() {
        let program = assemble6502!(
            lda #0x69
            sta abs 0x1122
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.mem_read(0x1122), 0x69)
    }
    // Store reg test ----------------------------------

    // STA test ----------------------------------------
    #[test]
    fn test_0x91_sta_indirect_y() {
        let program = assemble6502!(
            lda #0x10
            sta 0x00
            lda #0x00
            sta 0x01
            ldy #0x00
            lda #0x69
            sta (0x00),y
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.mem_read(0x0010), 0x69);
    }
    // STA test ----------------------------------------

    // TSX test ----------------------------------------
    #[test]
    fn test_0xba_tsx() {
        let program = assemble6502!(
            lda #0x69
            pha
            tsx
        );

        let mut cpu = CPU::new();
        cpu.load_and_run(&program);

        assert_eq!(cpu.register_x, cpu.stack_pointer)
    }
    // TSX test ----------------------------------------
}
