use std::{fs, process::exit, thread::sleep, time::Duration};

use nes_rs::cpu::{CPU, Memory, cartridge::Rom, log::log};
use rand::Rng;
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
fn read_screen_state(cpu: &CPU, frame: &mut [u8; 32 * 3 * 32]) -> bool {
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

    let scale = 20;
    let window = video_subsystem
        .window("NES", 32 * scale, 32 * scale)
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    let mut event_pump = sdl_content.event_pump().unwrap();
    canvas.set_scale(scale as f32, scale as f32).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(PixelFormatEnum::RGB24, 32, 32)
        .unwrap();
    // SDL init -----------------------------------------------------------

    let rom = Rom::new(&fs::read("nestest.nes").expect("Unable to open ROM")).unwrap();
    let mut cpu = CPU::new(rom);
    cpu.reset();
    cpu.program_counter = 0xC000;

    /* let mut rng = rand::rng();
    let mut screen = [0; 32 * 3 * 32]; */
    cpu.run_with_callback(move |cpu| {
        println!("{}", log(cpu));

        /*
        // This game code assumes the following table:
        //
        // | Address space    | Type      | Description                          |
        // |------------------|-----------|--------------------------------------|
        // | 0xFE             | Input     | Random number generator              |
        // | 0xFF             | Input     | Code of the last pressed button      |
        // | [0x0200..0x0600] | Output    | Screen.                              |
        // |                  |           | Each chell represents the color of a |
        // |                  |           | pixel in a 32x32 matrix that starts  |
        // |                  |           | from the top left corner.            |
        // | [0x0600..]       | Game Code | Execution code                       |

        cpu.mem_write(0xFE, rng.random_range(0..=0xF));
        handle_user_input(cpu, &mut event_pump);

        if read_screen_state(cpu, &mut screen) {
            texture.update(None, &screen, 32 * 3).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();
        }

        sleep(Duration::new(0, 50_000)); */
    });
}
