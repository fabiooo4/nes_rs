mod bus;
pub mod cartridge;
pub mod log;
mod opcodes;

use std::fmt::Debug;

use bus::Bus;
use cartridge::Rom;
use opcodes::{AddressingMode, Code};

const STACK: u16 = 0x0100;
const STACK_RESET: u8 = 0xfd;

pub struct CPU {
    register_a: u8,
    register_x: u8,
    register_y: u8,
    status: Status,
    pub program_counter: u16,
    stack_pointer: u8,
    bus: Bus,

    page_crossed: bool,
}

pub trait Memory {
    /// Reads a byte from memory
    fn mem_read(&mut self, addr: u16) -> u8;

    /// Writes a byte to memory
    fn mem_write(&mut self, addr: u16, data: u8);

    /// Writes a 16-bit value to memory (little endian)
    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        // Little endian
        let high = (data & 0xFF) as u8;
        let low = (data >> 8) as u8;

        self.mem_write(addr, high);
        self.mem_write(addr.wrapping_add(1), low);
    }

    /// Reads a 16-bit value from memory (little endian)
    fn mem_read_u16(&mut self, addr: u16) -> u16 {
        // Little endian
        let low = self.mem_read(addr) as u16;
        let high = self.mem_read(addr.wrapping_add(1)) as u16;

        high << 8 | low
    }
}

impl Memory for CPU {
    fn mem_read(&mut self, addr: u16) -> u8 {
        self.bus.mem_read(addr)
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.bus.mem_write(addr, data)
    }

    fn mem_read_u16(&mut self, addr: u16) -> u16 {
        self.bus.mem_read_u16(addr)
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        self.bus.mem_write_u16(addr, data);
    }
}

impl CPU {
    /// Creates a new CPU instance with default values
    pub fn new(rom: Rom) -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: Status::new(),
            program_counter: 0x8000,
            stack_pointer: STACK_RESET,
            bus: Bus::new(rom),
            page_crossed: false,
        }
    }

    /// Loads a program into memory and runs it
    pub fn load_and_run(&mut self, program: &[u8]) {
        self.load(program);
        self.reset();
        self.run();
    }

    /// Loads a program into memory
    ///
    /// The starting point of the program is written to 0xFFFC
    pub fn load(&mut self, program: &[u8]) {
        let program_start = 0x0000;
        for i in 0..(program.len() as u16) {
            self.mem_write(program_start + i, program[i as usize]);
        }

        // Write the program start in 0xFFFC
        self.mem_write_u16(0xFFFC, program_start);
    }

    /// Resets the CPU registers and status bits, then loads the program start address from 0xFFFC
    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = Status::new();
        self.stack_pointer = STACK_RESET;

        // Load program start from 0xFFFC
        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    /// Runs the CPU and a callback before each opcode execution
    ///
    /// # Panics
    /// If an invalid opcode is encountered, the CPU will panic.
    pub fn run_with_callback<F: FnMut(&mut CPU)>(&mut self, mut callback: F) {
        #[cfg(all(not(test), feature = "debug"))]
        {
            println!("Running in debug mode (to execute normally use 'cargo run --release'):");
            println!("Press enter to step to the next instruction, or 'q' to quit\n\n");
        }

        loop {
            if self.bus.poll_nmi_status().is_some() {
                self.interrupt_nmi();
            }

            callback(self);

            // Step by step debug
            #[cfg(all(not(test), feature = "debug"))]
            self.debug();

            let code = self.mem_read(self.program_counter);
            let opcode = opcodes::CPU_OPCODES
                .get(&code)
                .unwrap_or_else(|| panic!("Invalid opcode: {:02X}", code));
            let mut opcode_cycles = opcode.cycles;
            self.page_crossed = false;

            self.program_counter += 1;
            let pc_state = self.program_counter;

            match opcode.undocumented {
                false => match opcode.code {
                    Code::ADC => {
                        self.adc(&opcode.mode);

                        if self.page_crossed {
                            opcode_cycles += 1;
                        }
                    }
                    Code::AND => {
                        self.and(&opcode.mode);

                        if self.page_crossed {
                            opcode_cycles += 1;
                        }
                    }
                    Code::ASL => {
                        self.asl(&opcode.mode);
                    }
                    Code::BCC => {
                        self.branch(!self.status.carry);

                        if !self.status.carry {
                            opcode_cycles += 1;

                            if self.page_crossed {
                                opcode_cycles += 2;
                            }
                        }
                    }
                    Code::BCS => {
                        self.branch(self.status.carry);

                        if self.status.carry {
                            opcode_cycles += 1;

                            if self.page_crossed {
                                opcode_cycles += 2;
                            }
                        }
                    }
                    Code::BEQ => {
                        self.branch(self.status.zero);

                        if self.status.zero {
                            opcode_cycles += 1;

                            if self.page_crossed {
                                opcode_cycles += 2;
                            }
                        }
                    }
                    Code::BIT => self.bit(&opcode.mode),
                    Code::BMI => {
                        self.branch(self.status.negative);

                        if self.status.negative {
                            opcode_cycles += 1;

                            if self.page_crossed {
                                opcode_cycles += 2;
                            }
                        }
                    }
                    Code::BNE => {
                        self.branch(!self.status.zero);

                        if !self.status.zero {
                            opcode_cycles += 1;

                            if self.page_crossed {
                                opcode_cycles += 2;
                            }
                        }
                    }
                    Code::BPL => {
                        self.branch(!self.status.negative);

                        if !self.status.negative {
                            opcode_cycles += 1;

                            if self.page_crossed {
                                opcode_cycles += 2;
                            }
                        }
                    }
                    Code::BRK => return,
                    Code::BVC => {
                        self.branch(!self.status.overflow);

                        if !self.status.overflow {
                            opcode_cycles += 1;

                            if self.page_crossed {
                                opcode_cycles += 2;
                            }
                        }
                    }
                    Code::BVS => {
                        self.branch(self.status.overflow);

                        if self.status.overflow {
                            opcode_cycles += 1;

                            if self.page_crossed {
                                opcode_cycles += 2;
                            }
                        }
                    }
                    Code::CLC => self.status.carry = false,
                    Code::CLD => self.status.decimal = false,
                    Code::CLI => self.status.interrupt_disable = false,
                    Code::CLV => self.status.overflow = false,
                    Code::CMP => {
                        self.compare_register(self.register_a, &opcode.mode);

                        if self.page_crossed {
                            opcode_cycles += 1;
                        }
                    }
                    Code::CPX => self.compare_register(self.register_x, &opcode.mode),
                    Code::CPY => self.compare_register(self.register_y, &opcode.mode),
                    Code::DEC => self.dec(&opcode.mode),
                    Code::DEX => self.dex(),
                    Code::DEY => self.dey(),
                    Code::EOR => {
                        self.eor(&opcode.mode);

                        if self.page_crossed {
                            opcode_cycles += 1;
                        }
                    }
                    Code::INC => self.inc(&opcode.mode),
                    Code::INX => self.inx(),
                    Code::INY => self.iny(),
                    Code::JMP => self.jmp(&opcode.mode),
                    Code::JSR => self.jsr(),
                    Code::LDA => {
                        self.lda(&opcode.mode);

                        if self.page_crossed {
                            opcode_cycles += 1;
                        }
                    }
                    Code::LDX => {
                        self.ldx(&opcode.mode);

                        if self.page_crossed {
                            opcode_cycles += 1;
                        }
                    }
                    Code::LDY => {
                        self.ldy(&opcode.mode);

                        if self.page_crossed {
                            opcode_cycles += 1;
                        }
                    }
                    Code::LSR => {
                        self.lsr(&opcode.mode);
                    }
                    Code::NOP => continue,
                    Code::ORA => {
                        self.ora(&opcode.mode);

                        if self.page_crossed {
                            opcode_cycles += 1;
                        }
                    }
                    Code::PHA => self.stack_push(self.register_a),
                    Code::PHP => self.php(),
                    Code::PLA => self.pla(),
                    Code::PLP => self.plp(),
                    Code::ROL => {
                        self.rol(&opcode.mode);
                    }
                    Code::ROR => {
                        self.ror(&opcode.mode);
                    }
                    Code::RTI => self.rti(),
                    Code::RTS => self.rts(),
                    Code::SBC => {
                        self.sbc(&opcode.mode);

                        if self.page_crossed {
                            opcode_cycles += 1;
                        }
                    }
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
                    _ => unreachable!(),
                },
                true => match opcode.code {
                    Code::NOP => {
                        self.dop(&opcode.mode);

                        if self.page_crossed {
                            opcode_cycles += 1;
                        }
                    }
                    Code::LAX => {
                        self.lax(&opcode.mode);

                        if self.page_crossed {
                            opcode_cycles += 1;
                        }
                    }
                    Code::SAX => self.store_reg(&opcode.mode, self.register_x & self.register_a),
                    Code::SBC => self.sbc(&opcode.mode),
                    Code::DCP => self.dcp(&opcode.mode),
                    Code::ISB => self.isb(&opcode.mode),
                    Code::SLO => self.slo(&opcode.mode),
                    Code::RLA => self.rla(&opcode.mode),
                    Code::SRE => self.sre(&opcode.mode),
                    Code::RRA => self.rra(&opcode.mode),
                    _ => unreachable!(),
                },
            }

            self.bus.tick(opcode_cycles);

            // Update program counter based on parameter length
            if self.program_counter == pc_state {
                self.program_counter = self
                    .program_counter
                    .wrapping_add(self.params_num(&opcode.mode) as u16);
            }
        }
    }

    #[cfg(all(debug_assertions, not(test)))]
    fn debug(&mut self) {
        use log::monitor;
        use std::io::{Write, stdin, stdout};
        println!("{}", monitor(self));

        let mut s = String::new();
        let _ = stdout().flush();
        stdin()
            .read_line(&mut s)
            .expect("Did not enter a correct string");

        if s.trim() == "q" {
            std::process::exit(0);
        }
    }

    /// Sets register A and updates `zero` and `negative` status flags
    fn set_register_a(&mut self, value: u8) {
        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// Adds to register A while keeping track of wrapping and overflow
    fn add_to_register_a(&mut self, data: u8) {
        let sum = self.register_a as u16 + data as u16 + (self.status.carry as u16);

        self.status.carry = sum > 0xff;

        let result = sum as u8;

        self.status.overflow = (data ^ result) & (result ^ self.register_a) & 0x80 != 0;

        self.set_register_a(result);
    }

    /// Subtracts from register A while keeping track of wrapping and overflow
    fn sub_from_register_a(&mut self, data: u8) {
        self.add_to_register_a(((data as i8).wrapping_neg().wrapping_sub(1)) as u8);
    }

    /// Updates the zero and negative flags based on the result of an operation
    fn update_zero_and_negative_flags(&mut self, result: u8) {
        self.status.zero = result == 0;
        self.status.negative = result & 0b10000000 == 0b10000000;
    }

    /// Push a byte to the stack
    fn stack_push(&mut self, value: u8) {
        if self.stack_pointer.checked_sub(1).is_none() || self.stack_pointer > STACK_RESET {
            panic!("Stack overflow")
        }

        self.mem_write(STACK + self.stack_pointer as u16, value);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    // Push a 16 bit value to the stack
    fn stack_push_u16(&mut self, value: u16) {
        let high = (value >> 8) as u8;
        let low = (value & 0xff) as u8;

        self.stack_push(high);
        self.stack_push(low);
    }

    /// Pop a byte from the stack
    fn stack_pop(&mut self) -> u8 {
        if self.stack_pointer + 1 > STACK_RESET {
            panic!("Stack overflow")
        }
        let res = self.mem_read(STACK + self.stack_pointer as u16 + 1);
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        res
    }

    /// Pop a 16 bit value from the stack
    fn stack_pop_u16(&mut self) -> u16 {
        let low = self.stack_pop() as u16;
        let high = self.stack_pop() as u16;

        high << 8 | low
    }

    /// Gets the address of the operand parameters based on the addressing mode.
    ///
    /// # Returns
    /// Returns `None` if the mode is implied
    fn get_parameters_address(&mut self, mode: &AddressingMode, start_addr: u16) -> Option<u16> {
        match mode {
            AddressingMode::Immediate => Some(start_addr),
            AddressingMode::ZeroPage => Some(self.mem_read(start_addr) as u16),

            AddressingMode::ZeroPage_X => {
                Some(self.mem_read(start_addr).wrapping_add(self.register_x) as u16)
            }

            AddressingMode::ZeroPage_Y => {
                Some(self.mem_read(start_addr).wrapping_add(self.register_y) as u16)
            }

            AddressingMode::Absolute => Some(self.mem_read_u16(start_addr)),

            AddressingMode::Absolute_X => {
                let addr = self.mem_read_u16(start_addr);
                let res = addr.wrapping_add(self.register_x as u16);

                if ((res & 0xff) as u8) < ((addr & 0xff) as u8) {
                    self.page_crossed = true;
                }

                Some(res)
            }

            AddressingMode::Absolute_Y => {
                let addr = self.mem_read_u16(start_addr);
                let res = addr.wrapping_add(self.register_y as u16);

                if ((res & 0xff) as u8) < ((addr & 0xff) as u8) {
                    self.page_crossed = true;
                }

                Some(res)
            }

            AddressingMode::Indirect => {
                let base = self.mem_read_u16(start_addr);

                let deref_base = if base & 0x00FF == 0x00FF {
                    let low = self.mem_read(base) as u16;
                    let high = self.mem_read(base & 0xFF00) as u16;

                    high << 8 | low
                } else {
                    self.mem_read_u16(base)
                };

                Some(deref_base)
            }

            AddressingMode::Indirect_X => {
                let base = self.mem_read(start_addr).wrapping_add(self.register_x);
                let low = self.mem_read(base as u16) as u16;
                let high = self.mem_read(base.wrapping_add(1) as u16) as u16;

                Some(high << 8 | low)
            }

            AddressingMode::Indirect_Y => {
                let base = self.mem_read(start_addr);
                let low = self.mem_read(base as u16) as u16;
                let high = self.mem_read(base.wrapping_add(1) as u16) as u16;

                let deref_base = (high << 8) | low;
                let res = deref_base.wrapping_add(self.register_y as u16);

                if ((res & 0xff) as u8) < ((deref_base & 0xff) as u8) {
                    self.page_crossed = true;
                }

                Some(res)
            }

            AddressingMode::Implied => None,
        }
    }

    /// Returns the number of parameters based on the addressing mode
    fn params_num(&self, mode: &AddressingMode) -> usize {
        match mode {
            AddressingMode::Immediate
            | AddressingMode::ZeroPage
            | AddressingMode::ZeroPage_X
            | AddressingMode::ZeroPage_Y
            | AddressingMode::Indirect_X
            | AddressingMode::Indirect_Y => 1,

            AddressingMode::Absolute
            | AddressingMode::Absolute_X
            | AddressingMode::Absolute_Y
            | AddressingMode::Indirect => 2,

            AddressingMode::Implied => 0,
        }
    }

    /// Executes the NMI interrupt
    fn interrupt_nmi(&mut self) {
        self.stack_push_u16(self.program_counter);
        let mut status = self.status.clone();
        status.brk = false;
        self.stack_push(status.as_byte());

        self.status.interrupt_disable = true;

        // Load address of interrupt handler
        self.bus.tick(2);
        self.program_counter = self.mem_read_u16(0xFFFA);
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
        let param_addr = self.get_parameters_address(mode, self.program_counter).unwrap(/* safe */);
        let value = self.mem_read(param_addr);

        self.add_to_register_a(value);
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
        let param_addr = self.get_parameters_address(mode, self.program_counter).unwrap(/* safe */);
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
    fn asl(&mut self, mode: &AddressingMode) -> u8 {
        let value = match self.get_parameters_address(mode, self.program_counter) {
            Some(param_addr) => self.mem_read(param_addr),
            None => self.register_a,
        };

        let res = (value as u16) << 1;

        self.status.carry = (value >> 7) & 1 == 1;

        match self.get_parameters_address(mode, self.program_counter) {
            Some(param_addr) => {
                self.mem_write(param_addr, res as u8);
                self.update_zero_and_negative_flags(res as u8);
            }
            None => self.set_register_a(res as u8),
        };

        res as u8
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
        let param_addr = self.get_parameters_address(&AddressingMode::Immediate, self.program_counter).unwrap(/* safe */);
        let value: i8 = self.mem_read(param_addr) as i8;

        let res = self
            .program_counter
            .wrapping_add(1)
            .wrapping_add(value as u16);

        if ((res & 0xff) as u8) < ((self.program_counter & 0xff) as u8) {
            self.page_crossed = true;
        }

        if condition {
            self.program_counter = res;
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
        let param_addr = self.get_parameters_address(mode, self.program_counter).unwrap(/* safe */);
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
        let param_addr = self.get_parameters_address(mode, self.program_counter).unwrap(/* safe */);
        let target = self.mem_read(param_addr);
        let res: i8 = (reg as i8).wrapping_sub(target as i8);

        self.status.carry = target <= reg;
        self.update_zero_and_negative_flags(res as u8);
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
        let param_addr = self.get_parameters_address(mode, self.program_counter).unwrap(/* safe */);
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
        let param_addr = self.get_parameters_address(mode, self.program_counter).unwrap(/* safe */);
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
        let param_addr = self.get_parameters_address(mode, self.program_counter).unwrap(/* safe */);
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
        let target_addr =
            self.get_parameters_address(mode, self.program_counter).unwrap(/* safe */);

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
        let target_addr = self.get_parameters_address(&AddressingMode::Absolute, self.program_counter).unwrap(/* safe */);
        self.stack_push_u16(self.program_counter.wrapping_add(1));

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
        let param_addr = self.get_parameters_address(mode, self.program_counter).unwrap(/* safe */);
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
        let param_addr = self.get_parameters_address(mode, self.program_counter).unwrap(/* safe */);
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
        let param_addr = self.get_parameters_address(mode, self.program_counter).unwrap(/* safe */);
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
    fn lsr(&mut self, mode: &AddressingMode) -> u8 {
        let value = match self.get_parameters_address(mode, self.program_counter) {
            Some(param_addr) => self.mem_read(param_addr),
            None => self.register_a,
        };

        let res = (value as u16) >> 1;

        self.status.carry = value & 1 == 1;

        match self.get_parameters_address(mode, self.program_counter) {
            Some(param_addr) => {
                self.mem_write(param_addr, res as u8);
                self.update_zero_and_negative_flags(res as u8);
            }
            None => self.set_register_a(res as u8),
        };

        res as u8
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
        let param_addr = self.get_parameters_address(mode, self.program_counter).unwrap(/* safe */);
        let target = self.mem_read(param_addr);

        self.set_register_a(self.register_a | target);
    }

    /// Pushes a copy of the status flags on to the stack
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles |
    /// |------------------|--------|-------|--------|
    /// | Implied          | 08     | 1     | 3      |
    fn php(&mut self) {
        self.status.brk = true;
        self.stack_push(self.status.as_byte());
        self.status.brk = false;
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
    fn rol(&mut self, mode: &AddressingMode) -> u8 {
        let value = match self.get_parameters_address(mode, self.program_counter) {
            Some(param_addr) => self.mem_read(param_addr),
            None => self.register_a,
        };

        let old_carry = self.status.carry;

        self.status.carry = value >> 7 == 1;
        let res = (value << 1) | if old_carry { 0b00000001 } else { 0 };

        match self.get_parameters_address(mode, self.program_counter) {
            Some(param_addr) => {
                self.mem_write(param_addr, res);
                self.update_zero_and_negative_flags(res);
            }
            None => self.set_register_a(res),
        };

        res
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
    fn ror(&mut self, mode: &AddressingMode) -> u8 {
        let value = match self.get_parameters_address(mode, self.program_counter) {
            Some(param_addr) => self.mem_read(param_addr),
            None => self.register_a,
        };

        let old_carry = self.status.carry;

        self.status.carry = value & 0x01 == 0x01;
        let res = (value >> 1) | if old_carry { 0b10000000 } else { 0 };

        match self.get_parameters_address(mode, self.program_counter) {
            Some(param_addr) => {
                self.mem_write(param_addr, res);
                self.update_zero_and_negative_flags(res);
            }
            None => self.set_register_a(res),
        };

        res
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
        let target_addr = self.stack_pop_u16();

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
        let target_addr = self.stack_pop_u16() + 1;
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
        let param_addr = self.get_parameters_address(mode, self.program_counter).unwrap(/* safe */);
        let value = self.mem_read(param_addr);

        self.sub_from_register_a(value);
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
        let param_addr = self.get_parameters_address(mode, self.program_counter).unwrap(/* safe */);
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

    // Undocumented opcodes ----------------------------------------------------

    /// No operation (double NOP). The argument has no signifi-cance
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Immediate        | 80     | 2     | 2                          |
    /// | Immediate        | 82     | 2     | 2                          |
    /// | Immediate        | 89     | 2     | 2                          |
    /// | Immediate        | C2     | 2     | 2                          |
    /// | Immediate        | E2     | 2     | 2                          |
    /// | Zero Page        | 04     | 2     | 3                          |
    /// | Zero Page        | 44     | 2     | 3                          |
    /// | Zero Page        | 64     | 2     | 3                          |
    /// | Zero Page, X     | 14     | 2     | 4                          |
    /// | Zero Page, X     | 34     | 2     | 4                          |
    /// | Zero Page, X     | 54     | 2     | 4                          |
    /// | Zero Page, X     | 74     | 2     | 4                          |
    /// | Zero Page, X     | D4     | 2     | 4                          |
    /// | Zero Page, X     | F4     | 2     | 4                          |
    /// | Absolute         | 0C     | 3     | 4                          |
    /// | Absolute, X      | 1C     | 3     | 4 (+1 if page crossed)     |
    /// | Absolute, X      | 3C     | 3     | 4 (+1 if page crossed)     |
    /// | Absolute, X      | 5C     | 3     | 4 (+1 if page crossed)     |
    /// | Absolute, X      | 7C     | 3     | 4 (+1 if page crossed)     |
    /// | Absolute, X      | DC     | 3     | 4 (+1 if page crossed)     |
    /// | Absolute, X      | FC     | 3     | 4 (+1 if page crossed)     |
    /// | Implied          | 1A     | 1     | 2                          |
    /// | Implied          | 3A     | 1     | 2                          |
    /// | Implied          | 5A     | 1     | 2                          |
    /// | Implied          | 7A     | 1     | 2                          |
    /// | Implied          | DA     | 1     | 2                          |
    /// | Implied          | FA     | 1     | 2                          |
    fn dop(&mut self, mode: &AddressingMode) {
        if let Some(param_addr) = self.get_parameters_address(mode, self.program_counter) {
            self.mem_read(param_addr);
        }
    }

    /// Load accumulator and X register with memory and sets the zero and negative
    /// flags appropriately
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Zero Page        | 64     | 2     | 3                          |
    /// | Zero Page, Y     | 14     | 2     | 4                          |
    /// | Absolute         | 0C     | 3     | 4                          |
    /// | Absolute, Y      | FC     | 3     | 4 (+1 if page crossed)     |
    /// | Indirect, X      | C2     | 2     | 6                          |
    /// | Indirect, Y      | E2     | 2     | 5 (+1 if page crossed)     |
    fn lax(&mut self, mode: &AddressingMode) {
        let param_addr = self
            .get_parameters_address(mode, self.program_counter)
            .unwrap();
        let data = self.mem_read(param_addr);

        self.register_a = data;
        self.register_x = data;
        self.update_zero_and_negative_flags(data);
    }

    /// Subtract 1 from memory (without borrow)
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Zero Page        | C7     | 2     | 5                          |
    /// | Zero Page, X     | D7     | 2     | 6                          |
    /// | Absolute         | CF     | 3     | 6                          |
    /// | Absolute, X      | DF     | 3     | 7                          |
    /// | Absolute, Y      | DB     | 3     | 7                          |
    /// | Indirect, X      | C3     | 2     | 8                          |
    /// | Indirect, Y      | D3     | 2     | 8                          |
    fn dcp(&mut self, mode: &AddressingMode) {
        let param_addr = self
            .get_parameters_address(mode, self.program_counter)
            .unwrap();
        let mut data = self.mem_read(param_addr);
        data = data.wrapping_sub(1);
        self.mem_write(param_addr, data);
        self.status.carry = data <= self.register_a;

        self.update_zero_and_negative_flags(self.register_a.wrapping_sub(data));
    }

    /// Increase memory by one, then subtract memory from accu-mulator (with borrow)
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Zero Page        | E7     | 2     | 5                          |
    /// | Zero Page, X     | F7     | 2     | 6                          |
    /// | Absolute         | EF     | 3     | 6                          |
    /// | Absolute, X      | FF     | 3     | 7                          |
    /// | Absolute, Y      | FB     | 3     | 7                          |
    /// | Indirect, X      | E3     | 2     | 8                          |
    /// | Indirect, Y      | F3     | 2     | 8                          |
    fn isb(&mut self, mode: &AddressingMode) {
        let param_addr = self
            .get_parameters_address(mode, self.program_counter)
            .unwrap();
        let mut data = self.mem_read(param_addr);

        data = data.wrapping_add(1);
        self.mem_write(param_addr, data);

        self.sub_from_register_a(data);
    }

    /// Shift left one bit in memory, then OR accumulator with memory
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Zero Page        | 07     | 2     | 5                          |
    /// | Zero Page, X     | 17     | 2     | 6                          |
    /// | Absolute         | 0F     | 3     | 6                          |
    /// | Absolute, X      | 1F     | 3     | 7                          |
    /// | Absolute, Y      | 1B     | 3     | 7                          |
    /// | Indirect, X      | 03     | 2     | 8                          |
    /// | Indirect, Y      | 13     | 2     | 8                          |
    fn slo(&mut self, mode: &AddressingMode) {
        let data = self.asl(mode);
        self.set_register_a(data | self.register_a);
    }

    /// Rotate one bit left in memory, then AND accumulator with memory
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Zero Page        | 27     | 2     | 5                          |
    /// | Zero Page, X     | 37     | 2     | 6                          |
    /// | Absolute         | 2F     | 3     | 6                          |
    /// | Absolute, X      | 3F     | 3     | 7                          |
    /// | Absolute, Y      | 3B     | 3     | 7                          |
    /// | Indirect, X      | 23     | 2     | 8                          |
    /// | Indirect, Y      | 33     | 2     | 8                          |
    fn rla(&mut self, mode: &AddressingMode) {
        let data = self.rol(mode);
        self.set_register_a(data & self.register_a);
    }

    /// Shift right one bit in memory, then EOR accumulator with memory
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Zero Page        | 47     | 2     | 5                          |
    /// | Zero Page, X     | 57     | 2     | 6                          |
    /// | Absolute         | 4F     | 3     | 6                          |
    /// | Absolute, X      | 5F     | 3     | 7                          |
    /// | Absolute, Y      | 5B     | 3     | 7                          |
    /// | Indirect, X      | 43     | 2     | 8                          |
    /// | Indirect, Y      | 53     | 2     | 8                          |
    fn sre(&mut self, mode: &AddressingMode) {
        let data = self.lsr(mode);
        self.set_register_a(data ^ self.register_a);
    }

    /// Rotate one bit right in memory, then ADC accumulator with memory
    ///
    /// ## Addressing modes
    /// | Addressing Mode  | Opcode | Bytes | Cycles                     |
    /// |------------------|--------|-------|----------------------------|
    /// | Zero Page        | 67     | 2     | 5                          |
    /// | Zero Page, X     | 77     | 2     | 6                          |
    /// | Absolute         | 6F     | 3     | 6                          |
    /// | Absolute, X      | 7F     | 3     | 7                          |
    /// | Absolute, Y      | 7B     | 3     | 7                          |
    /// | Indirect, X      | 63     | 2     | 8                          |
    /// | Indirect, Y      | 73     | 2     | 8                          |
    fn rra(&mut self, mode: &AddressingMode) {
        let data = self.ror(mode);
        self.add_to_register_a(data);
    }
    // Undocumented opcodes ----------------------------------------------------
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
#[derive(Debug, Clone)]
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
            interrupt_disable: true,
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
    use cartridge::test::test_rom;

    // Assembler test --------------------------------------------------------
    #[test]
    fn test_assembler() {
        let program = assemble6502!(
            lda #0x2
            tax
            lda 0x33,x
        );

        assert_eq!(&program, &[0xa9, 0x02, 0xaa, 0xb5, 0x33]);
    }
    // Assembler test --------------------------------------------------------

    // Memory read/write tests ----------------------
    #[test]
    fn test_mem_write() {
        let mut cpu = CPU::new(test_rom(vec![0x00]));
        cpu.mem_write(0x1234, 0x69);
        assert_eq!(cpu.mem_read(0x1234), 0x69)
    }

    #[test]
    fn test_mem_write_u16() {
        let mut cpu = CPU::new(test_rom(vec![0x00]));
        cpu.mem_write_u16(0x1234, 0x420);
        assert_eq!(cpu.mem_read(0x1234), 0x20);
        assert_eq!(cpu.mem_read(0x1235), 0x04)
    }

    #[test]
    fn test_mem_read() {
        let mut cpu = CPU::new(test_rom(vec![0x00]));
        cpu.mem_write(0x1234, 0x69);
        assert_eq!(cpu.mem_read(0x1234), 0x69)
    }

    #[test]
    fn test_mem_read_u16() {
        let mut cpu = CPU::new(test_rom(vec![0x00]));
        cpu.mem_write(0x1234, 0x20);
        cpu.mem_write(0x1235, 0x04);
        assert_eq!(cpu.mem_read_u16(0x1234), 0x0420);
    }
    // Memory read/write tests ----------------------

    // LDA test --------------------------------------
    #[test]
    fn test_0xa9_lda_immediate() {
        let program = assemble6502!(
            lda #0x5
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.register_a, 0x05);
        assert!(!cpu.status.zero);
        assert!(!cpu.status.negative);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let program = assemble6502!(
            lda #0x00
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();
        assert!(cpu.status.zero);
    }

    #[test]
    fn test_0xa9_lda_negative_flag() {
        let program = assemble6502!(
            lda #0xff
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();
        assert!(cpu.status.negative);
    }

    #[test]
    fn test_0xa5_lda_zero_page() {
        let program = assemble6502!(
            lda 0x33
        );
        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write(0x33, 0x0a);
        cpu.run();

        assert_eq!(cpu.register_a, 0x0a);
    }

    #[test]
    fn test_0xb5_lda_zero_page_x() {
        let program = assemble6502!(
            lda #0x2
            tax
            lda 0x33,x
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write(0x35, 0x0a);
        cpu.run();

        assert_eq!(cpu.register_a, 0x0a);
    }

    #[test]
    fn test_0xad_lda_absolute() {
        let program = assemble6502!(
            lda abs 0x1234
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write(0x1234, 0x08);
        cpu.run();

        assert_eq!(cpu.register_a, 0x08);
    }

    #[test]
    fn test_0xbd_lda_absolute_x() {
        let program = assemble6502!(
            ldx #0x1
            lda abs 0x1234,x
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write(0x1235, 0x08);
        cpu.run();

        assert_eq!(cpu.register_a, 0x08);
    }

    #[test]
    fn test_0xbd_lda_absolute_y() {
        let program = assemble6502!(
            ldy #0x1
            lda abs 0x1234,y
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write(0x1235, 0x08);
        cpu.run();

        assert_eq!(cpu.register_a, 0x08);
    }

    #[test]
    fn test_0xb1_lda_indirect_y() {
        let program = assemble6502!(
            ldy #0x01
            lda #0x03
            sta 0x01
            lda #0x07
            sta 0x02
            ldx #0x0a
            stx abs 0x0704
            lda (0x01),y
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();
        assert_eq!(cpu.register_a, 0x0a);
    }
    // LDA test --------------------------------------

    // TAX test --------------------------------------
    #[test]
    fn test_0xaa_tax() {
        let program = assemble6502!(
            lda #0x11
            tax
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();
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
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();
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
        );
        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.register_x, 5)
    }
    #[test]
    fn test_0xe8_inx_overflow() {
        let program = assemble6502!(
            lda #0xff
            tax
            inx
            inx
        );
        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

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
        );
        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.register_x, 0xc1)
    }
    // Basic program test -------------------------------

    // ADC test ----------------------------------------
    #[test]
    fn test_0x69_adc_immediate() {
        let program = assemble6502!(
            adc #0x04
            adc #0x04
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

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
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

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
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

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
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

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
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write(0xc4, 0x09);
        cpu.run();

        assert_eq!(cpu.register_a, 0x0a);
    }

    #[test]
    fn test_0x75_adc_zero_page_x() {
        let program = assemble6502!(
            lda #0x01
            ldx #0x03
            adc 0xc4,x
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write(0xc7, 0x09);
        cpu.run();

        assert_eq!(cpu.register_a, 0x0a);
    }

    #[test]
    fn test_0x6d_adc_absolute() {
        let program = assemble6502!(
            lda #0x01
            adc abs 0x1234
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write_u16(0x1234, 0x03);
        cpu.run();

        assert_eq!(cpu.register_a, 0x04);
    }

    #[test]
    fn test_0x7d_adc_absolute_x() {
        let program = assemble6502!(
            ldx #0x03
            lda #0x01
            adc abs 0x1234,x
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write_u16(0x1237, 0x03);
        cpu.run();

        assert_eq!(cpu.register_a, 0x04);
    }

    #[test]
    fn test_0x79_adc_absolute_y() {
        let program = assemble6502!(
            ldy #0x03
            lda #0x01
            adc abs 0x1234,y
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write_u16(0x1237, 0x03);
        cpu.run();

        assert_eq!(cpu.register_a, 0x04);
    }

    #[test]
    fn test_0x61_adc_indirect_x() {
        let program = assemble6502!(
            ldx #0x03
            lda #0x01
            adc (0x12,x)
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write_u16(0x15, 0x1234);
        cpu.mem_write_u16(0x1234, 0x03);
        cpu.run();

        assert_eq!(cpu.register_a, 0x04);
    }

    #[test]
    fn test_0x71_adc_indirect_y() {
        let program = assemble6502!(
            ldy #0x03
            lda #0x01
            adc (0x12),y
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write_u16(0x12, 0x1234);
        cpu.mem_write_u16(0x1237, 0x03);
        cpu.run();

        assert_eq!(cpu.register_a, 0x04);
    }
    // ADC test ----------------------------------------

    // AND test ----------------------------------------
    #[test]
    fn test_0x29_and_immediate() {
        let program = assemble6502!(
            lda #0b11111110
            and #0b00000011
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.register_a, 0x02);
    }

    #[test]
    fn test_0x25_and_zero_page() {
        let program = assemble6502!(
            lda #0xff
            and 0xc4
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write(0xc4, 0b10000000);
        cpu.run();

        assert_eq!(cpu.register_a, 0b10000000);
    }

    #[test]
    fn test_0x35_and_zero_page_x() {
        let program = assemble6502!(
            lda #0xff
            ldx #0x03
            and 0xc4,x
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write(0xc7, 0b10101010);
        cpu.run();

        assert_eq!(cpu.register_a, 0b10101010);
    }

    #[test]
    fn test_0x2d_and_absolute() {
        let program = assemble6502!(
            lda #0xff
            and abs 0x1234
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write_u16(0x1234, 0b10101010);
        cpu.run();

        assert_eq!(cpu.register_a, 0b10101010);
    }

    #[test]
    fn test_0x3d_and_absolute_x() {
        let program = assemble6502!(
            ldx #0x03
            lda #0xff
            and abs 0x1234,x
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write_u16(0x1237, 0b10101010);
        cpu.run();

        assert_eq!(cpu.register_a, 0b10101010);
    }

    #[test]
    fn test_0x39_and_absolute_y() {
        let program = assemble6502!(
            ldy #0x03
            lda #0xff
            and abs 0x1234,y
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write_u16(0x1237, 0b10101010);
        cpu.run();

        assert_eq!(cpu.register_a, 0b10101010);
    }

    #[test]
    fn test_0x21_and_indirect_x() {
        let program = assemble6502!(
            ldx #0x03
            lda #0xff
            and (0x12,x)
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write_u16(0x15, 0x1234);
        cpu.mem_write_u16(0x1234, 0b10101010);
        cpu.run();

        assert_eq!(cpu.register_a, 0b10101010);
    }

    #[test]
    fn test_0x31_and_indirect_y() {
        let program = assemble6502!(
            ldy #0x03
            lda #0xff
            and (0x12),y
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write_u16(0x12, 0x1234);
        cpu.mem_write_u16(0x1237, 0b10101010);
        cpu.run();

        assert_eq!(cpu.register_a, 0b10101010);
    }
    // AND test ----------------------------------------

    // ASL test ----------------------------------------
    #[test]
    fn test_0x0a_asl() {
        let program = assemble6502!(
            lda #0x02
            asl a
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.register_a, 0x04);

        assert!(!cpu.status.overflow)
    }

    #[test]
    fn test_0x0a_asl_overflow() {
        let program = assemble6502!(
            lda #0b10000000
            asl a
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.register_a, 0x0);
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

        let mut cpu = CPU::new(test_rom(program.to_vec()));
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
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert!(cpu.status.carry);
        assert!(!cpu.status.zero);
        assert!(!cpu.status.negative);
    }

    #[test]
    fn test_0xc9_cmp_immediate_e() {
        let program = assemble6502!(
            lda #0x01
            cmp #0x01
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert!(cpu.status.carry);
        assert!(cpu.status.zero);
        assert!(!cpu.status.negative);
    }

    #[test]
    fn test_0xc9_cmp_immediate_l() {
        let program = assemble6502!(
            lda #0x01
            cmp #0x03
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

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
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write(0x1234, 0x3);
        cpu.run();

        assert_eq!(cpu.mem_read(0x1234), 0x2);
    }
    // DEC test ----------------------------------------

    // EOR test ----------------------------------------
    #[test]
    fn test_0x49_eor_immediate() {
        let program = assemble6502!(
            lda #0b10101010
            eor #0b01010101
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

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

        let mut cpu = CPU::new(test_rom(program.to_vec()));

        // Update program start offset
        let program_start = cpu.program_counter;
        let program_jmp = program_start + ((program[2] as u16) << 8 | program[1] as u16);
        program[1] = program_jmp as u8;
        program[2] = (program_jmp >> 8) as u8;

        cpu = CPU::new(test_rom(program.to_vec()));
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
        let mut cpu = CPU::new(test_rom(vec![0x00]));

        assert_eq!(cpu.stack_pointer, STACK_RESET);
        cpu.stack_push(0x69);
        assert_eq!(cpu.stack_pointer, STACK_RESET - 1);

        assert_eq!(cpu.mem_read(STACK + (cpu.stack_pointer as u16 + 1)), 0x69)
    }

    #[test]
    fn test_stack_pop() {
        let mut cpu = CPU::new(test_rom(vec![0x00]));

        cpu.stack_push(0x69);
        assert_eq!(cpu.stack_pointer, STACK_RESET - 1);
        assert_eq!(cpu.stack_pop(), 0x69);
        assert_eq!(cpu.stack_pointer, STACK_RESET);
    }

    #[test]
    #[should_panic]
    fn test_stack_overflow() {
        let mut cpu = CPU::new(test_rom(vec![0x00]));

        for i in 0..STACK_RESET {
            cpu.stack_push(i);
        }

        assert_eq!(cpu.stack_pointer, 0x00);

        cpu.stack_push(0xff); // Stack overflow
    }

    #[test]
    #[should_panic]
    fn test_stack_underflow() {
        let mut cpu = CPU::new(test_rom(vec![0x00]));
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

        let mut cpu = CPU::new(test_rom(program.to_vec()));

        // Update program start offset
        let program_start = cpu.program_counter;
        let program_jmp = program_start + ((program[2] as u16) << 8 | program[1] as u16);
        program[1] = program_jmp as u8;
        program[2] = (program_jmp >> 8) as u8;

        cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();
        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.program_counter, program_start + 0x06);
        assert_eq!(cpu.stack_pop_u16(), program_start + 0x02);
    }
    // JSR test ----------------------------------------

    // LSR test ----------------------------------------
    #[test]
    fn test_0x4a_lsr_implied() {
        let program = assemble6502!(
           lda #0x02
           lsr a
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.register_a, 0x01);
        assert!(!cpu.status.carry);
    }

    #[test]
    fn test_0x4e_lsr_absolute() {
        let program = assemble6502!(
           lsr abs 0x1234
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write(0x1234, 0x03);
        cpu.run();

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

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.stack_pop(), 0x69);
    }
    // PHA test ----------------------------------------

    // Status as/from byte test -----------------------------
    #[test]
    fn test_status_as_byte() {
        let mut cpu = CPU::new(test_rom(vec![0x00]));
        cpu.status.negative = true;
        cpu.status.carry = true;
        cpu.status.brk = true;

        assert_eq!(cpu.status.as_byte(), 0b10110101);
    }

    #[test]
    fn test_status_set_from_byte() {
        let mut cpu = CPU::new(test_rom(vec![0x00]));

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

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.stack_pop(), 0b00110111)
    }
    // PHP test ---------------------------------------------

    // ROL test ---------------------------------------------
    #[test]
    fn test_0x2a_rol_accumulator() {
        let program = assemble6502!(
            lda #0b11000000
            rol a
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.register_a, 0b10000000);
        assert!(cpu.status.carry);
    }

    #[test]
    fn test_0x2a_rol_accumulator_2() {
        let program = assemble6502!(
            lda #0b11000000
            rol a
            rol a
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.register_a, 0b00000001);
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

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.register_a, 0b00000001);
        assert!(cpu.status.carry);
    }

    #[test]
    fn test_0x6a_ror_accumulator_2() {
        let program = assemble6502!(
            lda #0b00000011
            ror a
            ror a
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.register_a, 0b10000000);
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
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.stack_push_u16(cpu.program_counter + 4); // Push pc first
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

        let mut cpu = CPU::new(test_rom(program.to_vec()));

        // Update program start offset
        let program_start = cpu.program_counter;
        let program_jmp = program_start + ((program[2] as u16) << 8 | program[1] as u16);
        program[1] = program_jmp as u8;
        program[2] = (program_jmp >> 8) as u8;

        cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.register_a, 0x01);
        assert_eq!(cpu.program_counter, program_start + 0x04);
    }
    // RTS test ---------------------------------------------

    // SBC test ----------------------------------------
    #[test]
    fn test_0xe9_sbc_immediate() {
        let program = assemble6502!(
            lda #0x05
            sbc #0x04
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

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

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

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

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.mem_read(0x0011), 0x69)
    }

    #[test]
    fn test_0x85_sta_absolute() {
        let program = assemble6502!(
            lda #0x69
            sta abs 0x1122
        );

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

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

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

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

        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.run();

        assert_eq!(cpu.register_x, cpu.stack_pointer)
    }
    // TSX test ----------------------------------------

    // Page crossed test -------------------------------
    #[test]
    fn test_0xbd_lda_page_crossed_absolute_x() {
        let program = assemble6502!(
            ldx #0x01
            lda abs 0x00ff,x
        );

        let mut page_crossed = false;
        let mut cpu = CPU::new(test_rom(program.to_vec()));

        cpu.run_with_callback(|cpu| {
            println!("PageCrossed: {}", cpu.page_crossed);
            if cpu.page_crossed {
                page_crossed = true;
            }
        });

        assert!(page_crossed);
    }

    #[test]
    fn test_0xb9_lda_page_crossed_absolute_y() {
        let program = assemble6502!(
            ldy #0x01
            lda abs 0x00ff,y
        );

        let mut page_crossed = false;
        let mut cpu = CPU::new(test_rom(program.to_vec()));

        cpu.run_with_callback(|cpu| {
            println!("PageCrossed: {}", cpu.page_crossed);
            if cpu.page_crossed {
                page_crossed = true;
            }
        });

        assert!(page_crossed);
    }

    #[test]
    fn test_0xb1_lda_page_crossed_indirect_y() {
        let program = assemble6502!(
            ldy #0x01
            lda (0x00),y
        );

        let mut page_crossed = false;
        let mut cpu = CPU::new(test_rom(program.to_vec()));
        cpu.mem_write(0x00, 0xff);
        cpu.mem_write(0x01, 0x00);
        cpu.mem_write(0x0100, 0x69);

        cpu.run_with_callback(|cpu| {
            println!("PageCrossed: {}", cpu.page_crossed);
            if cpu.page_crossed {
                page_crossed = true;
            }
        });

        assert_eq!(cpu.register_a, 0x69);
        assert!(page_crossed);
    }
    // Page crossed test -------------------------------
}
