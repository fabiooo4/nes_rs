use crate::BitFlags;

#[derive(Debug)]
pub struct JoypadButtons {
    pub right: bool,
    pub left: bool,
    pub down: bool,
    pub up: bool,
    pub start: bool,
    pub select: bool,
    pub button_b: bool,
    pub button_a: bool,
}

#[derive(Clone, Copy, Debug)]
#[rustfmt::skip]
pub enum JoypadButtonMask {
    Right   = 0b10000000,
    Left    = 0b01000000,
    Down    = 0b00100000,
    Up      = 0b00010000,
    Start   = 0b00001000,
    Select  = 0b00000100,
    ButtonB = 0b00000010,
    ButtonA = 0b00000001,
}

impl JoypadButtons {
    #![allow(clippy::new_without_default)]
    pub fn new() -> Self {
        JoypadButtons {
            right: false,
            left: false,
            down: false,
            up: false,
            start: false,
            select: false,
            button_b: false,
            button_a: false,
        }
    }
}

impl BitFlags for JoypadButtons {
    fn as_byte(&self) -> u8 {
        (self.right as u8) << 7
            | (self.left as u8) << 6
            | (self.down as u8) << 5
            | (self.up as u8) << 4
            | (self.start as u8) << 3
            | (self.select as u8) << 2
            | (self.button_b as u8) << 1
            | self.button_a as u8
    }

    #[rustfmt::skip]
    fn set_from_byte(&mut self, byte: u8) {
        self.right    = byte & 0b10000000 == 0b10000000;
        self.left     = byte & 0b01000000 == 0b01000000;
        self.down     = byte & 0b00100000 == 0b00100000;
        self.up       = byte & 0b00010000 == 0b00010000;
        self.start    = byte & 0b00001000 == 0b00001000;
        self.select   = byte & 0b00000100 == 0b00000100;
        self.button_b = byte & 0b00000010 == 0b00000010;
        self.button_a = byte & 0b00000001 == 0b00000001;
    }
}
