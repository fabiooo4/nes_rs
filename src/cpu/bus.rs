use super::Memory;

pub struct Bus {
    cpu_vram: [u8; 2048], // 11 Bits
}

impl Bus {
    pub fn new() -> Self {
        Bus {
            cpu_vram: [0; 2048],
        }
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

impl Memory for Bus {
    fn mem_read(&self, addr: u16) -> u8 {
        match addr {
            RAM..=RAM_MIRROR_END => {
                // [0x0000..0x0800]
                let mirrored_addr = addr & 0x7ff;
                self.cpu_vram[mirrored_addr as usize]
            }
            PPU_REG..=PPU_REG_MIRROR_END => {
                // [0x2000..0x2008]
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
                self.cpu_vram[mirrored_addr as usize] = data
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
