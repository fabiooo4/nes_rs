use super::{Memory, cartridge::Rom};

pub struct Bus {
    cpu_wram: [u8; 2048], // 11 Bits
    rom: Rom,
}

impl Bus {
    pub fn new(rom: Rom) -> Self {
        Bus {
            cpu_wram: [0; 2048],
            rom,
        }
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000; // Remove mapping

        // Mirror if PRG has only 1 bank of 16kB
        if self.rom.prg_rom.len() == 0x4000 && addr >= 0x4000 {
            addr %= 0x4000;
        }

        self.rom.prg_rom[addr as usize]
    }
}

// Reserved spaces
// RAM --------------------
const RAM: u16 = 0x0000;
const RAM_MIRROR_END: u16 = 0x1FFF;
// RAM --------------------
// PPU --------------------
const PPU_REG: u16 = 0x2000;
const PPU_REG_MIRROR_END: u16 = 0x3FFF;
// PPU --------------------
// PRG_ROM ----------------
const PRG_ROM: u16 = 0x8000;
const PRG_ROM_END: u16 = 0xffff;
// PRG_ROM ----------------

impl Memory for Bus {
    fn mem_read(&self, addr: u16) -> u8 {
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
            PPU_REG..=PPU_REG_MIRROR_END => {
                // [0x2000..0x3FFF] -> [0x2000..0x2008]
                let _mirrored_addr = addr & 0x2007;
                unimplemented!("PPU not yet supported");
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
            PRG_ROM..=PRG_ROM_END => {
                panic!("Attempted to write in cartridge Read Only Memory");
            }
            PPU_REG..=PPU_REG_MIRROR_END => {
                // [0x2000..0x2008]
                let _mirrored_addr = addr & 0x2007;
                unimplemented!("PPU not yet supported");
            }
            _ => {
                println!("Memory write at {:X} ignored", addr);
            }
        }
    }
}
