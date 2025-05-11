use clap::Parser;
use lazy_static::lazy_static;

#[cfg(test)]
#[macro_use]
#[no_link]
extern crate rustasm6502;

pub mod cpu;
pub mod joypad;
pub mod ppu;
pub mod render;

#[derive(Parser, Debug, Clone)]
#[command(version, about, long_about = None)]
pub struct Args {
    /// Path to the rom to execute
    #[arg(required = true)]
    pub rom: String,

    /// Run with step by step execution and a snapshot of cpu and memory
    #[arg(short, long)]
    pub debug: bool,

    /// Log instructions to stdout
    #[arg(short, long, conflicts_with = "debug")]
    pub log: bool,
}

lazy_static! {
    pub static ref ARGS: Args = Args::parse();
}

pub trait BitFlags {
    fn as_byte(&self) -> u8;
    fn set_from_byte(&mut self, byte: u8);
}
