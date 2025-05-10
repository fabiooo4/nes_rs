#[cfg(test)]
#[macro_use]
#[no_link]
extern crate rustasm6502;

pub mod cpu;
pub mod ppu;
pub mod render;
pub mod joypad;

pub trait BitFlags {
    fn as_byte(&self) -> u8;
    fn set_from_byte(&mut self, byte: u8);
}
