use std::{fs, process::exit};

use nes_rs::{
    cpu::{CPU, Memory, cartridge::Rom},
    render::{
        frame::{self},
        show_tile_bank,
    },
};
use sdl2::{
    EventPump,
    event::Event,
    keyboard::Keycode,
    pixels::{Color, PixelFormatEnum},
};

/// Handles ESC, and WASD
fn handle_user_input(cpu: &mut CPU, event_pump: &mut EventPump) {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. }
            | Event::KeyDown {
                keycode: Some(Keycode::Escape),
                ..
            } => exit(0),
            Event::KeyDown {
                keycode: Some(Keycode::W),
                ..
            } => {
                cpu.mem_write(0xFF, 0x77);
            }
            Event::KeyDown {
                keycode: Some(Keycode::A),
                ..
            } => {
                cpu.mem_write(0xFF, 0x61);
            }
            Event::KeyDown {
                keycode: Some(Keycode::S),
                ..
            } => {
                cpu.mem_write(0xFF, 0x73);
            }
            Event::KeyDown {
                keycode: Some(Keycode::D),
                ..
            } => {
                cpu.mem_write(0xFF, 0x64);
            }
            _ => {}
        }
    }
}

/// Converts a byte into color
fn color(color: u8) -> Color {
    match color {
        0 => Color::BLACK,
        1 => Color::WHITE,
        2 | 9 => sdl2::pixels::Color::GREY,
        3 | 10 => sdl2::pixels::Color::RED,
        4 | 11 => sdl2::pixels::Color::GREEN,
        5 | 12 => sdl2::pixels::Color::BLUE,
        6 | 13 => sdl2::pixels::Color::MAGENTA,
        7 | 14 => sdl2::pixels::Color::YELLOW,
        _ => Color::CYAN,
    }
}

/// Checks if screen has changed before updating
///
/// # Return
/// Returns `true` if the screen needs to be updated,
/// `false` otherwise
fn read_screen_state(cpu: &mut CPU, frame: &mut [u8; 32 * 3 * 32]) -> bool {
    let mut frame_idx = 0;
    let mut update = false;
    for i in 0x0200..0x600 {
        let color_idx = cpu.mem_read(i as u16);
        let (r, g, b) = color(color_idx).rgb();
        if frame[frame_idx] != r || frame[frame_idx + 1] != g || frame[frame_idx + 2] != b {
            frame[frame_idx] = r;
            frame[frame_idx + 1] = g;
            frame[frame_idx + 2] = b;
            update = true;
        }
        frame_idx += 3;
    }
    update
}

fn main() {
    // SDL init -----------------------------------------------------------
    let sdl_content = sdl2::init().unwrap();
    let video_subsystem = sdl_content.video().unwrap();

    let scale = 1;
    let window = video_subsystem
        .window(
            "NES Emulator",
            (frame::WIDTH * 3 * scale) as u32,
            (frame::HEIGHT * 3 * scale) as u32,
        )
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    let mut event_pump = sdl_content.event_pump().unwrap();
    canvas.set_scale(scale as f32, scale as f32).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(
            PixelFormatEnum::RGB24,
            frame::WIDTH as u32,
            frame::HEIGHT as u32,
        )
        .unwrap();
    // SDL init -----------------------------------------------------------

    let rom = Rom::new(&fs::read("Alter_Ego.nes").expect("Unable to open ROM"))
        .unwrap();

    // Display tile
    let bank_frame = show_tile_bank(&rom.chr_rom, 1);
    texture.update(None, &bank_frame.data, 256 * 3).unwrap();

    canvas.copy(&texture, None, None).unwrap();
    canvas.present();

    loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => std::process::exit(0),
                _ => { /* do nothing */ }
            }
        }
    }
}
