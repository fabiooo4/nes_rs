mod registers;

use crate::{BitFlags, cpu::cartridge::Mirroring};
use registers::{
    address::AddressRegister, control::ControlRegister, mask::MaskRegister, scroll::ScrollRegister,
    status::StatusRegister,
};

#[allow(clippy::upper_case_acronyms)]
pub struct PPU {
    pub chr_rom: Vec<u8>,
    pub palette_table: [u8; 32],
    pub vram: [u8; 2048],
    pub addr: AddressRegister,
    pub ctrl: ControlRegister,
    pub mask: MaskRegister,
    pub status: StatusRegister,
    pub oam_data: [u8; 256],
    pub oam_addr: u8,
    pub scroll: ScrollRegister,

    pub mirroring: Mirroring,

    pub internal_data_buf: u8,
}

impl PPU {
    pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        PPU {
            chr_rom,
            palette_table: [0; 32],
            vram: [0; 2048],
            addr: AddressRegister::new(),
            ctrl: ControlRegister::new(),
            mask: MaskRegister::new(),
            status: StatusRegister::new(),
            oam_data: [0; 256],
            oam_addr: 0,
            scroll: ScrollRegister::new(),
            mirroring,
            internal_data_buf: 0,
        }
    }

    pub fn new_empty_rom() -> Self {
        PPU::new(vec![0; 2048], Mirroring::Horizontal)
    }

    fn increment_vram_addr(&mut self) {
        self.addr.increment(self.ctrl.vram_addr_increment());
    }

    // Horizontal:
    //   [ A ] [ a ]
    //   [ B ] [ b ]

    // Vertical:
    //   [ A ] [ B ]
    //   [ a ] [ b ]
    pub fn mirror_vram_addr(&self, addr: u16) -> u16 {
        // [0x3000..0x3eff] -> [0x2000..0x2eff]
        let mirrored_vram = addr & 0b0010111111111111;
        let vram_index = mirrored_vram - 0x2000;
        let name_table = vram_index / 0x400;

        match (&self.mirroring, name_table) {
            (Mirroring::Vertical, 2) | (Mirroring::Vertical, 3) => vram_index - 0x800,
            (Mirroring::Horizontal, 2) => vram_index - 0x400,
            (Mirroring::Horizontal, 1) => vram_index - 0x400,
            (Mirroring::Horizontal, 3) => vram_index - 0x800,
            _ => vram_index,
        }
    }
}

pub trait PPUOperations {
    fn write_to_addr(&mut self, data: u8);
    fn read_data(&mut self) -> u8;
    fn write_to_data(&mut self, data: u8);
    fn write_to_ctrl(&mut self, data: u8);
    fn write_to_mask(&mut self, data: u8);
    fn read_status(&mut self) -> u8;
    fn write_to_oam_addr(&mut self, data: u8);
    fn read_oam_data(&mut self) -> u8;
    fn write_to_oam_data(&mut self, data: u8);
    fn write_to_oam_dma(&mut self, data: &[u8; 256]);
    fn write_to_scroll(&mut self, data: u8);
}

impl PPUOperations for PPU {
    /// Writes to the PPU address register
    fn write_to_addr(&mut self, data: u8) {
        self.addr.update(data);
    }

    /// Writes to the PPU control register
    fn write_to_ctrl(&mut self, data: u8) {
        self.ctrl.set_from_byte(data);
    }

    /// Writes to the PPU mask register
    fn write_to_mask(&mut self, data: u8) {
        self.mask.set_from_byte(data);
    }

    /// Read the PPU status register
    fn read_status(&mut self) -> u8 {
        let res = self.status.as_byte();
        self.status.reset_vblank();
        self.addr.reset_latch();
        res
    }

    fn read_data(&mut self) -> u8 {
        let addr = self.addr.get();
        self.increment_vram_addr();

        match addr {
            0..=0x1fff => {
                let res = self.internal_data_buf;
                self.internal_data_buf = self.chr_rom[addr as usize];
                res
            }
            0x2000..=0x2fff => {
                let res = self.internal_data_buf;
                self.internal_data_buf = self.vram[self.mirror_vram_addr(addr) as usize];
                res
            }
            0x3000..=0x3eff => panic!(
                "addr space 0x3000..0x3eff is not expected to be used, requested = {} ",
                addr
            ),
            0x3f00..=0x3fff => self.palette_table[(addr - 0x3f00) as usize],
            _ => panic!("unexpected access to mirrored space {}", addr),
        }
    }

    fn write_to_data(&mut self, data: u8) {
        let addr = self.addr.get();
        self.increment_vram_addr();

        match addr {
            0..=0x1fff => {
                println!("attempt to write to chr rom space {}", addr);
            }
            0x2000..=0x2fff => {
                self.vram[self.mirror_vram_addr(addr) as usize] = data;
            }
            0x3000..=0x3eff => unimplemented!(
                "addr space 0x3000..0x3eff is not expected to be used, requested = {} ",
                addr
            ),
            //Addresses $3F10/$3F14/$3F18/$3F1C are mirrors of $3F00/$3F04/$3F08/$3F0C
            0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => {
                let add_mirror = addr - 0x10;
                self.palette_table[(add_mirror - 0x3f00) as usize] = data;
            }
            0x3f00..=0x3fff => self.palette_table[(addr - 0x3f00) as usize] = data,
            _ => panic!("unexpected access to mirrored space {}", addr),
        }
    }

    fn write_to_oam_addr(&mut self, data: u8) {
        self.oam_addr = data;
    }

    fn read_oam_data(&mut self) -> u8 {
        self.oam_data[self.oam_addr as usize]
    }

    fn write_to_oam_data(&mut self, data: u8) {
        self.oam_data[self.oam_addr as usize] = data;
        self.oam_addr = self.oam_addr.wrapping_add(1);
    }

    fn write_to_scroll(&mut self, data: u8) {
        self.scroll.write(data);
    }

    fn write_to_oam_dma(&mut self, data: &[u8; 256]) {
        for byte in data {
            self.write_to_oam_data(*byte);
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_ppu_vram_writes() {
        let mut ppu = PPU::new_empty_rom();
        ppu.write_to_addr(0x23);
        ppu.write_to_addr(0x45);
        ppu.write_to_data(0x69);

        assert_eq!(ppu.vram[ppu.mirror_vram_addr(0x2345) as usize], 0x69)
    }

    #[test]
    fn test_ppu_vram_reads() {
        let mut ppu = PPU::new_empty_rom();
        ppu.write_to_ctrl(0); // Ensure that vram is incremented by 1

        ppu.vram[ppu.mirror_vram_addr(0x2222) as usize] = 0x69;
        ppu.write_to_addr(0x22);
        ppu.write_to_addr(0x22);

        assert_eq!(ppu.read_data(), 0x00); // Load into buffer
        assert_eq!(ppu.addr.get(), 0x2222 + 1);
        assert_eq!(ppu.read_data(), 0x69);
    }

    #[test]
    fn test_ppu_vram_reads_cross_page() {
        let mut ppu = PPU::new_empty_rom();
        ppu.ctrl.vram_addr_increment = false; // Ensure that vram is incremented by 1

        ppu.vram[ppu.mirror_vram_addr(0x22FF) as usize] = 0x69;
        ppu.vram[ppu.mirror_vram_addr(0x2300) as usize] = 0x6A;
        ppu.write_to_addr(0x22);
        ppu.write_to_addr(0xFF);

        assert_eq!(ppu.read_data(), 0x00); // Load into buffer

        assert_eq!(ppu.addr.get(), 0x22FF + 1);
        assert_eq!(ppu.read_data(), 0x69);

        assert_eq!(ppu.addr.get(), 0x22FF + 2);
        assert_eq!(ppu.read_data(), 0x6A);
    }

    #[test]
    fn test_ppu_vram_reads_step_32() {
        let mut ppu = PPU::new_empty_rom();
        ppu.ctrl.vram_addr_increment = true; // Ensure that vram is incremented by 32

        ppu.vram[ppu.mirror_vram_addr(0x2222) as usize] = 0x69;
        ppu.vram[ppu.mirror_vram_addr(0x2222 + 32) as usize] = 0x6A;
        ppu.vram[ppu.mirror_vram_addr(0x2222 + 32 * 2) as usize] = 0x6B;
        ppu.write_to_addr(0x22);
        ppu.write_to_addr(0x22);

        assert_eq!(ppu.read_data(), 0x00); // Load into buffer

        assert_eq!(ppu.addr.get(), 0x2222 + 32);
        assert_eq!(ppu.read_data(), 0x69);

        assert_eq!(ppu.addr.get(), 0x2222 + 32 * 2);
        assert_eq!(ppu.read_data(), 0x6A);

        assert_eq!(ppu.addr.get(), 0x2222 + 32 * 3);
        assert_eq!(ppu.read_data(), 0x6B);
    }

    // Horizontal: https://wiki.nesdev.com/w/index.php/Mirroring
    //   [0x2000 A ] [0x2400 a ]
    //   [0x2800 B ] [0x2C00 b ]
    #[test]
    fn test_vram_horizontal_mirror() {
        let mut ppu = PPU::new_empty_rom();
        ppu.write_to_addr(0x24);
        ppu.write_to_addr(0x05);

        ppu.write_to_data(0x66); // Write to a

        ppu.write_to_addr(0x28);
        ppu.write_to_addr(0x05);

        ppu.write_to_data(0x77); // Write to B

        ppu.write_to_addr(0x20);
        ppu.write_to_addr(0x05);

        ppu.read_data(); // Load into buffer
        assert_eq!(ppu.read_data(), 0x66); // Read from A

        ppu.write_to_addr(0x2C);
        ppu.write_to_addr(0x05);

        ppu.read_data(); // Load into buffer
        assert_eq!(ppu.read_data(), 0x77); // Read from b   
    }

    // Vertical: https://wiki.nesdev.com/w/index.php/Mirroring
    //   [0x2000 A ] [0x2400 B ]
    //   [0x2800 a ] [0x2C00 b ]
    #[test]
    fn test_vram_vertical_mirror() {
        let mut ppu = PPU::new(vec![0; 2048], Mirroring::Vertical);
        ppu.write_to_addr(0x28);
        ppu.write_to_addr(0x3F);

        ppu.write_to_data(0x69); // Write to a

        ppu.write_to_addr(0x24);
        ppu.write_to_addr(0x11);

        ppu.write_to_data(0x6A); // Write to B

        ppu.write_to_addr(0x20);
        ppu.write_to_addr(0x3F);

        ppu.read_data(); // Load into buffer
        assert_eq!(ppu.read_data(), 0x69); // Read from A

        ppu.write_to_addr(0x2C);
        ppu.write_to_addr(0x11);

        ppu.read_data(); // Load into buffer
        assert_eq!(ppu.read_data(), 0x6A); // Read from b
    }

    #[test]
    fn test_read_status_resets_latch() {
        let mut ppu = PPU::new_empty_rom();
        ppu.vram[ppu.mirror_vram_addr(0x2223) as usize] = 0x69;

        ppu.write_to_addr(0x21);
        ppu.write_to_addr(0x22);
        ppu.write_to_addr(0x23);

        ppu.read_data(); // Load into buffer
        assert_ne!(ppu.read_data(), 0x69);

        ppu.read_status();

        ppu.write_to_addr(0x22);
        ppu.write_to_addr(0x23);

        ppu.read_data(); // Load into buffer
        assert_eq!(ppu.read_data(), 0x69);
    }

    #[test]
    fn test_ppu_vram_mirroring() {
        let mut ppu = PPU::new_empty_rom();
        ppu.write_to_ctrl(0);
        ppu.vram[0x0305] = 0x66;

        ppu.write_to_addr(0x63); //0x6305 -> 0x2305
        ppu.write_to_addr(0x05);

        ppu.read_data(); // Load into buffer
        assert_eq!(ppu.read_data(), 0x66);
    }

    #[test]
    fn test_read_status_resets_vblank() {
        let mut ppu = PPU::new_empty_rom();
        ppu.status.vblank = true;

        let status = ppu.read_status();

        assert_eq!(status >> 7, 1);
        assert_eq!(ppu.status.as_byte() >> 7, 0);
    }

    #[test]
    fn test_oam_read_write() {
        let mut ppu = PPU::new_empty_rom();
        ppu.write_to_oam_addr(0x10);
        ppu.write_to_oam_data(0x66);
        ppu.write_to_oam_data(0x77);

        ppu.write_to_oam_addr(0x10);
        assert_eq!(ppu.read_oam_data(), 0x66);

        ppu.write_to_oam_addr(0x11);
        assert_eq!(ppu.read_oam_data(), 0x77);
    }

    #[test]
    fn test_oam_dma() {
        let mut ppu = PPU::new_empty_rom();

        let mut data = [0x69; 256];
        data[0] = 0x77;
        data[255] = 0x88;

        ppu.write_to_oam_addr(0x10);
        ppu.write_to_oam_dma(&data);

        ppu.write_to_oam_addr(0xf); // Wrap around
        assert_eq!(ppu.read_oam_data(), 0x88);

        ppu.write_to_oam_addr(0x10);
        assert_eq!(ppu.read_oam_data(), 0x77);

        ppu.write_to_oam_addr(0x11);
        assert_eq!(ppu.read_oam_data(), 0x69);
    }
}
