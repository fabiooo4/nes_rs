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

    pub fn load_and_run(&mut self, program: &[u8]) {
        self.load(program);
        self.reset();
        self.run();
    }

    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_read_16(&self, addr: u16) -> u16 {
        // Little endian
        let low = self.mem_read(addr) as u16;
        let high = self.mem_read(addr + 1) as u16;

        high << 8 | low
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn mem_write_16(&mut self, addr: u16, data: u16) {
        // Little endian
        let high = (data & 0xFF) as u8;
        let low = (data >> 8) as u8;

        self.mem_write(addr, high);
        self.mem_write(addr + 1, low);
    }

    pub fn load(&mut self, program: &[u8]) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(program);

        // Write the program start in 0xFFFC
        self.mem_write_16(0xFFFC, 0x8000);
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.status = Status::new();

        // Load program start from 0xFFFC
        self.program_counter = self.mem_read_16(0xFFFC);
    }

    pub fn run(&mut self) {
        loop {
            let code = self.mem_read(self.program_counter);
            let opcode = opcodes::CPU_OPCODES
                .get(&code)
                .unwrap_or_else(|| panic!("Invalid opcode: {:X}", code));

            self.program_counter += 1;

            match opcode.code {
                Code::LDA => self.lda(&opcode.mode),
                Code::TAX => self.tax(),
                Code::INX => self.inx(),
                Code::BRK => return,
                _ => {}
            }
        }
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        self.status.zero = result == 0;
        self.status.negative = result & 0b10000000 == 0b10000000;
    }

    fn get_operand_address(&mut self, mode: &AddressingMode) -> u16 {
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

            AddressingMode::NoneAddressing => panic!("Mode {:?} is not supported", mode),
        };

        // Update program counter based on parameter length
        match mode {
            AddressingMode::Immediate => self.program_counter += 1,
            AddressingMode::ZeroPage => self.program_counter += 1,
            AddressingMode::ZeroPage_X => self.program_counter += 1,
            AddressingMode::ZeroPage_Y => self.program_counter += 1,
            AddressingMode::Absolute => self.program_counter += 2,
            AddressingMode::Absolute_X => self.program_counter += 2,
            AddressingMode::Absolute_Y => self.program_counter += 2,
            AddressingMode::Indirect_X => self.program_counter += 2,
            AddressingMode::Indirect_Y => self.program_counter += 2,
            AddressingMode::NoneAddressing => {}
        }

        addr
    }
}

/// Opcodes implementation
impl CPU {
    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

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
/// |||+------ (No CPU effect; cause of status change)
/// ||+------- (No CPU effect; always pushed as 1)
/// |+-------- Overflow
/// +--------- Negative
/// ```
struct Status {
    negative: bool,
    overflow: bool,
    b: bool,
    decimal: bool,
    interrupt_disable: bool,
    zero: bool,
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
            b: false,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

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

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&[0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(&[0xa9, 0xff, 0xaa, 0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 1)
    }

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
}
