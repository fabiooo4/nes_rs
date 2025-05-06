use crate::BitFlags;

/// 7  bit  0
/// ---- ----
/// VPHB SINN
/// |||| ||||
/// |||| ||++- Base nametable address
/// |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
/// |||| |+--- VRAM address increment per CPU read/write of PPUDATA
/// |||| |     (0: add 1, going across; 1: add 32, going down)
/// |||| +---- Sprite pattern table address for 8x8 sprites
/// ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
/// |||+------ Background pattern table address (0: $0000; 1: $1000)
/// ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels)
/// |+-------- PPU master/slave select
/// |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
/// +--------- Generate an NMI at the start of the
///            vertical blanking interval (0: off; 1: on)
pub struct ControlRegister {
    pub nametable1: bool,
    pub nametable2: bool,
    pub vram_addr_increment: bool,
    pub sprite_pattern_addr: bool,
    pub backround_pattern_addr: bool,
    pub sprite_size: bool,
    pub master_slave_select: bool,
    pub generate_nmi: bool,
}

impl ControlRegister {
    #![allow(clippy::new_without_default)]
    pub fn new() -> Self {
        ControlRegister {
            nametable1: false,
            nametable2: false,
            vram_addr_increment: false,
            sprite_pattern_addr: false,
            backround_pattern_addr: false,
            sprite_size: false,
            master_slave_select: false,
            generate_nmi: false,
        }
    }

    /// Get the vram address increment
    pub fn vram_addr_increment(&self) -> u8 {
        match self.vram_addr_increment {
            true => 32,
            false => 1,
        }
    }
}

impl BitFlags for ControlRegister {
    fn as_byte(&self) -> u8 {
        (self.generate_nmi as u8) << 7
            | (self.master_slave_select as u8) << 6
            | (self.sprite_size as u8) << 5
            | (self.backround_pattern_addr as u8) << 4
            | (self.sprite_pattern_addr as u8) << 3
            | (self.vram_addr_increment as u8) << 2
            | (self.nametable2 as u8) << 1
            | self.nametable1 as u8
    }

    fn set_from_byte(&mut self, byte: u8) {
        self.generate_nmi = byte & 0b10000000 == 0b10000000;
        self.master_slave_select = byte & 0b01000000 == 0b01000000;
        self.sprite_size = byte & 0b00100000 == 0b00100000;
        self.backround_pattern_addr = byte & 0b00010000 == 0b00010000;
        self.sprite_pattern_addr = byte & 0b00001000 == 0b00001000;
        self.vram_addr_increment = byte & 0b00000100 == 0b00000100;
        self.nametable2 = byte & 0b00000010 == 0b00000010;
        self.nametable1 = byte & 0b00000001 == 0b00000001;
    }
}
