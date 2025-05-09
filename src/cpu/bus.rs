use crate::ppu::{PPU, PPUOperations};

use super::{Memory, cartridge::Rom};

pub struct Bus {
    cpu_wram: [u8; 2048], // 11 Bits
    prg_rom: Vec<u8>,
    ppu: PPU,

    cycles: usize,
}

impl Bus {
    pub fn new(rom: Rom) -> Self {
        Bus {
            cpu_wram: [0; 2048],
            prg_rom: rom.prg_rom,
            ppu: PPU::new(rom.chr_rom, rom.screen_mirroring),
            cycles: 0,
        }
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000; // Remove mapping

        // Mirror if PRG has only 1 bank of 16kB
        if self.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            addr %= 0x4000;
        }

        self.prg_rom[addr as usize]
    }

    /// Tick the clock of other components based on the CPU cycles
    pub fn tick(&mut self, cycles: usize) {
        self.cycles += cycles;
        self.ppu.tick(cycles * 3);
    }

    /// Gets the nmi interrupt status from the PPU
    pub fn poll_nmi_status(&mut self) -> Option<u8> {
        self.ppu.nmi_interrupt.take()
    }
}

// Reserved spaces
// RAM --------------------
const RAM: u16 = 0x0000;
const RAM_MIRROR_END: u16 = 0x1FFF;

// PPU --------------------
const PPU_REG_MIRROR_END: u16 = 0x3FFF;

// PRG_ROM ----------------
const PRG_ROM: u16 = 0x8000;
const PRG_ROM_END: u16 = 0xffff;

impl Memory for Bus {
    fn mem_read(&mut self, addr: u16) -> u8 {
        match addr {
            RAM..=RAM_MIRROR_END => {
                // [0x0000..0x01FFF] -> [0x0000..0x0800]
                let mirrored_addr = addr & 0x7ff;
                self.cpu_wram[mirrored_addr as usize]
            }
            PRG_ROM..=PRG_ROM_END => {
                // [0x8000..0x10000] -> PRG_ROM
                self.read_prg_rom(addr)
            }
            0x2002 => self.ppu.read_status(),
            0x2004 => self.ppu.read_oam_data(),
            0x2007 => self.ppu.read_data(),
            0x2008..=PPU_REG_MIRROR_END => {
                let mirror_down_addr = addr & 0b00100000_00000111;
                self.mem_read(mirror_down_addr)
            }

            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                panic!("Attempt to read from write-only PPU address {:x}", addr);
            }
            _ => {
                println!("Memory access at {:X} ignored", addr);
                0
            }
        }
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            RAM..=RAM_MIRROR_END => {
                // [0x0000..0x0800]
                let mirrored_addr = addr & 0x7ff;
                self.cpu_wram[mirrored_addr as usize] = data
            }
            0x2000 => self.ppu.write_to_ctrl(data),
            0x2001 => self.ppu.write_to_mask(data),
            0x2003 => self.ppu.write_to_oam_addr(data),
            0x2004 => self.ppu.write_to_oam_data(data),
            0x2005 => self.ppu.write_to_scroll(data),
            0x2006 => self.ppu.write_to_addr(data),
            0x2007 => self.ppu.write_to_data(data),

            0x2002 => {
                panic!("Attempt to write to read-only PPU address {:x}", addr);
            }

            0x2008..=PPU_REG_MIRROR_END => {
                let mirror_down_addr = addr & 0b00100000_00000111;
                self.mem_write(mirror_down_addr, data);
            }

            PRG_ROM..=PRG_ROM_END => {
                panic!("Attempted to write in cartridge Read Only Memory");
            }
            _ => {
                println!("Memory write at {:X} ignored", addr);
            }
        }
    }
}
