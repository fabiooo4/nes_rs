use crate::BitFlags;

/// 7  bit  0
/// ---- ----
/// VSOx xxxx
/// |||| ||||
/// |||+-++++- (PPU open bus or 2C05 PPU identifier)
/// ||+------- Sprite overflow flag
/// |+-------- Sprite 0 hit flag
/// +--------- Vblank flag, cleared on read. Unreliable; see below.
pub struct StatusRegister {
    pub sprite_overflow: bool,
    pub sprite_0_hit: bool,
    pub vblank: bool,
}

impl StatusRegister {
    #![allow(clippy::new_without_default)]
    pub fn new() -> Self {
        StatusRegister {
            sprite_overflow: false,
            sprite_0_hit: false,
            vblank: false,
        }
    }

    pub fn reset_vblank(&mut self) {
        self.vblank = false;
    }
}

impl BitFlags for StatusRegister {
    fn as_byte(&self) -> u8 {
        (self.vblank as u8) << 7
            | (self.sprite_0_hit as u8) << 6
            | (self.sprite_overflow as u8) << 5
    }

    fn set_from_byte(&mut self, byte: u8) {
        self.vblank = byte & 0b10000000 == 0b10000000;
        self.sprite_0_hit = byte & 0b01000000 == 0b01000000;
        self.sprite_overflow = byte & 0b00100000 == 0b00100000;
    }
}
