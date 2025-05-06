use crate::BitFlags;

/// 7  bit  0
/// ---- ----
/// BGRs bMmG
/// |||| ||||
/// |||| |||+- Greyscale (0: normal color, 1: greyscale)
/// |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
/// |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
/// |||| +---- 1: Enable background rendering
/// |||+------ 1: Enable sprite rendering
/// ||+------- Emphasize red (green on PAL/Dendy)
/// |+-------- Emphasize green (red on PAL/Dendy)
/// +--------- Emphasize blue
pub struct MaskRegister {
    pub greyscale: bool,
    pub letfmost_8px_bg: bool,
    pub leftmost_8px_sprite: bool,
    pub bg_rendering: bool,
    pub sprite_rendering: bool,
    pub emphasize_red: bool,
    pub emphasize_green: bool,
    pub emphasize_blue: bool,
}

impl MaskRegister {
    #![allow(clippy::new_without_default)]
    pub fn new() -> Self {
        MaskRegister {
            greyscale: false,
            letfmost_8px_bg: false,
            leftmost_8px_sprite: false,
            bg_rendering: false,
            sprite_rendering: false,
            emphasize_red: false,
            emphasize_green: false,
            emphasize_blue: false,
        }
    }
}

impl BitFlags for MaskRegister {
    fn as_byte(&self) -> u8 {
        (self.greyscale as u8)
            | (self.letfmost_8px_bg as u8) << 1
            | (self.leftmost_8px_sprite as u8) << 2
            | (self.bg_rendering as u8) << 3
            | (self.sprite_rendering as u8) << 4
            | (self.emphasize_red as u8) << 5
            | (self.emphasize_green as u8) << 6
            | (self.emphasize_blue as u8) << 7
    }

    fn set_from_byte(&mut self, byte: u8) {
        self.greyscale = byte & 0b00000001 == 0b00000001;
        self.letfmost_8px_bg = byte & 0b00000010 == 0b00000010;
        self.leftmost_8px_sprite = byte & 0b00000100 == 0b00000100;
        self.bg_rendering = byte & 0b00001000 == 0b00001000;
        self.sprite_rendering = byte & 0b00010000 == 0b00010000;
        self.emphasize_red = byte & 0b00100000 == 0b00100000;
        self.emphasize_green = byte & 0b01000000 == 0b01000000;
        self.emphasize_blue = byte & 0b10000000 == 0b10000000;
    }
}
