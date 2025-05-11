use crate::cpu::CPU;

use super::{
    Memory,
    opcodes::{self, AddressingMode, Code},
};

/// Logs the current cpu instruction to stdout
pub fn log(cpu: &mut CPU) -> String {
    let code = cpu.mem_read(cpu.program_counter);
    let opcode = opcodes::CPU_OPCODES
        .get(&code)
        .unwrap_or_else(|| panic!("Invalid opcode: {:02X}", code));

    let begin = cpu.program_counter;

    // Get the parameters of the opcode
    let mut params: [u8; 2] = [0, 0];

    (0..cpu.params_num(&opcode.mode)).for_each(|i| {
        let param = cpu.mem_read(begin.wrapping_add(i as u16 + 1));
        params[i] = param
    });

    let hex_dump = match cpu.params_num(&opcode.mode) {
        0 => String::default(),
        1 => format!("{:02X}", params[0]),
        2 => format!("{:02X} {:02X}", params[0], params[1]),
        _ => {
            unreachable!()
        }
    };

    let (mem_addr, stored_value) = match opcode.mode {
        AddressingMode::Implied => (0, 0),
        AddressingMode::Immediate => match opcode.code {
            Code::BCC
            | Code::BCS
            | Code::BEQ
            | Code::BNE
            | Code::BMI
            | Code::BPL
            | Code::BVC
            | Code::BVS => {
                let addr = cpu.get_parameters_address(&opcode.mode, begin + 1).unwrap();
                (addr, cpu.mem_read(addr))
            }
            _ => (0, 0),
        },
        _ => {
            let addr = cpu.get_parameters_address(&opcode.mode, begin + 1).unwrap();
            (addr, cpu.mem_read(addr))
        }
    };

    let asm = format!(
        "{:?}{}",
        opcode.code,
        match opcode.mode {
            AddressingMode::Immediate => {
                match opcode.code {
                    Code::BCC
                    | Code::BCS
                    | Code::BEQ
                    | Code::BNE
                    | Code::BMI
                    | Code::BPL
                    | Code::BVC
                    | Code::BVS => {
                        format!(
                            " ${:04X}",
                            mem_addr
                                .wrapping_add((stored_value as i8) as u16)
                                .wrapping_add(1)
                        )
                    }
                    _ => format!(" #${:02X}", params[0]),
                }
            }
            AddressingMode::ZeroPage => format!(" ${:02X}", params[0]),
            AddressingMode::ZeroPage_X => format!(" ${:02X},X", params[0]),
            AddressingMode::ZeroPage_Y => format!(" ${:02X},Y", params[0]),
            AddressingMode::Absolute => format!(" ${:02X}{:02X}", params[1], params[0]),
            AddressingMode::Absolute_X => format!(" ${:02X}{:02X},X", params[1], params[0]),
            AddressingMode::Absolute_Y => format!(" ${:02X}{:02X},Y", params[1], params[0]),
            AddressingMode::Indirect => format!(" (${:02X}{:02X})", params[1], params[0]),
            AddressingMode::Indirect_X => format!(" (${:02X},X)", params[0]),
            AddressingMode::Indirect_Y => format!(" (${:02X}),Y", params[0]),
            AddressingMode::Implied => String::default(),
        }
    );

    let addr_values = match opcode.mode {
        AddressingMode::Implied => match opcode.code {
            Code::ASL | Code::LSR | Code::ROL | Code::ROR => String::from("A"),
            _ => String::default(),
        },
        AddressingMode::Immediate => String::default(),
        AddressingMode::ZeroPage => {
            format!("= {:02X}", cpu.mem_read(params[0] as u16))
        }
        AddressingMode::ZeroPage_X | AddressingMode::ZeroPage_Y => {
            format!("@ {:02X} = {:02X}", mem_addr, stored_value)
        }
        AddressingMode::Indirect => format!("= {:04X}", mem_addr),

        AddressingMode::Indirect_X => {
            format!(
                "@ {:02X} = {:04X} = {:02X}",
                params[0].wrapping_add(cpu.register_x),
                mem_addr,
                stored_value
            )
        }
        AddressingMode::Indirect_Y => {
            format!(
                "= {:04X} @ {:04X} = {:02X}",
                mem_addr.wrapping_sub(cpu.register_y as u16),
                mem_addr,
                stored_value,
            )
        }
        AddressingMode::Absolute_X | AddressingMode::Absolute_Y => {
            format!("@ {:04X} = {:02X}", mem_addr, stored_value)
        }
        AddressingMode::Absolute => match opcode.code {
            Code::JMP | Code::JSR => String::default(),
            _ => format!("= {:02X}", stored_value),
        },
    };
    let asm = format!("{} {}", asm, addr_values);

    let cpu_state = format!(
        "A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
        cpu.register_a,
        cpu.register_x,
        cpu.register_y,
        cpu.status.as_byte(),
        cpu.stack_pointer
    );

    let log = format!(
        "{:04X}  {:02X} {:5} {}{:30}  {}",
        begin,
        code,
        hex_dump,
        if opcode.undocumented { '*' } else { ' ' },
        asm,
        cpu_state
    );

    log
}

/// This mode prints on stdout one bank of memory and all of
/// the stack along with cpu registers status
pub fn monitor(cpu: &mut CPU) -> String {
    use crate::cpu::STACK;

    let code = cpu.mem_read(cpu.program_counter);
    let opcode = opcodes::CPU_OPCODES
        .get(&code)
        .unwrap_or_else(|| panic!("Invalid opcode: {:02X}", code));

    // Get the parameters of the opcode
    let mut params: [String; 2] = [String::from("  "), String::from("  ")];
    (0..cpu.params_num(&opcode.mode)).for_each(|i| {
        params[i] = format!(
            "{:02X}",
            cpu.mem_read(cpu.program_counter.wrapping_add(i as u16 + 1))
        )
    });

    let next_ist = format!(
        "Next instruction: {:02X?} {} {} | {:?}",
        opcode.code, params[0], params[1], opcode.mode
    );

    let cpu_state = format!(
        "PC:{:04X?} SP:{:02X?} P:{:08b}|{:02X}",
        cpu.program_counter,
        cpu.stack_pointer,
        cpu.status.as_byte(),
        cpu.status.as_byte()
    );
    let cpu_regs = format!(
        "A:{:02X?} X:{:02X?} Y:{:02X?}",
        cpu.register_a, cpu.register_x, cpu.register_y
    );

    let mut memory_banks = String::default();
    for col in (0x00_u16..=0xff_u16).step_by(0x10) {
        memory_banks.push_str(&format!("{:04X}: ", col));
        for row in 0x0_u8..=0xf_u8 {
            memory_banks.push_str(&format!("{:02X} ", cpu.mem_read(col + row as u16)));
        }
        memory_banks.push('\t');
        memory_banks.push_str(&format!("{:02X}: ", col));
        for row in 0x0_u8..=0xf_u8 {
            memory_banks.push_str(&format!("{:02X} ", cpu.mem_read(STACK + col + row as u16)));
        }
        memory_banks.push('\n');
    }

    format!("{next_ist}\n{cpu_state}\n{cpu_regs}\nMemory:\t\t\t\t\t\t\tStack:\n{memory_banks}")
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::cpu::{Memory, bus::Bus, cartridge::test::test_rom};

    #[test]
    fn test_format_log() {
        let bus = Bus::new(test_rom(vec![]), |_,_,_| {});
        let mut cpu = CPU::new(bus);
        cpu.mem_write(100, 0xa2);
        cpu.mem_write(101, 0x01);
        cpu.mem_write(102, 0xca);
        cpu.mem_write(103, 0x88);
        cpu.mem_write(104, 0x00);

        cpu.program_counter = 0x64;
        cpu.register_a = 1;
        cpu.register_x = 2;
        cpu.register_y = 3;
        let mut result: Vec<String> = vec![];
        cpu.run_with_callback(|cpu| {
            result.push(log(cpu));
        });
        assert_eq!(
            "0064  A2 01     LDX #$01                        A:01 X:02 Y:03 P:24 SP:FD",
            result[0]
        );
        assert_eq!(
            "0066  CA        DEX                             A:01 X:01 Y:03 P:24 SP:FD",
            result[1]
        );
        assert_eq!(
            "0067  88        DEY                             A:01 X:00 Y:03 P:26 SP:FD",
            result[2]
        );
    }

    #[test]
    fn test_format_mem_access() {
        let bus = Bus::new(test_rom(vec![]), |_,_,_| {});
        let mut cpu = CPU::new(bus);
        // ORA ($33), Y
        cpu.mem_write(100, 0x11);
        cpu.mem_write(101, 0x33);

        //data
        cpu.mem_write(0x33, 0x00);
        cpu.mem_write(0x34, 0x04);

        //target cell
        cpu.mem_write(0x400, 0xAA);

        cpu.program_counter = 0x64;
        cpu.register_y = 0;
        let mut result: Vec<String> = vec![];
        cpu.run_with_callback(|cpu| {
            result.push(log(cpu));
        });
        assert_eq!(
            "0064  11 33     ORA ($33),Y = 0400 @ 0400 = AA  A:00 X:00 Y:00 P:24 SP:FD",
            result[0]
        );
    }
}
