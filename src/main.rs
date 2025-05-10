use nes_rs::{
    cpu::{CPU, bus::Bus, cartridge::Rom},
    joypad::{
        Joypad,
        buttons::{JoypadButtonMask, JoypadButtons},
    },
    ppu::PPU,
    render::{
        frame::{self, Frame},
        render,
    },
};
use sdl2::{event::Event, keyboard::Keycode, pixels::PixelFormatEnum};
use std::{collections::HashMap, fs};

fn main() {
    // SDL init -----------------------------------------------------------
    let sdl_content = sdl2::init().unwrap();
    let video_subsystem = sdl_content.video().unwrap();

    let scale: f32 = 1.0;
    let window = video_subsystem
        .window(
            "NES Emulator",
            (frame::WIDTH as f32 * 3.0 * scale) as u32,
            (frame::HEIGHT as f32 * 3.0 * scale) as u32,
        )
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    let mut event_pump = sdl_content.event_pump().unwrap();
    canvas.set_scale(scale, scale).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator
        .create_texture_target(
            PixelFormatEnum::RGB24,
            frame::WIDTH as u32,
            frame::HEIGHT as u32,
        )
        .unwrap();
    // SDL init -----------------------------------------------------------

    // Key mapping --------------------------------------------------------
    let joy1_keymap: HashMap<Keycode, JoypadButtonMask> = HashMap::from([
        (Keycode::S, JoypadButtonMask::Down),
        (Keycode::W, JoypadButtonMask::Up),
        (Keycode::D, JoypadButtonMask::Right),
        (Keycode::A, JoypadButtonMask::Left),
        (Keycode::E, JoypadButtonMask::Select),
        (Keycode::R, JoypadButtonMask::Start),
        (Keycode::Space, JoypadButtonMask::ButtonA),
        (Keycode::LShift, JoypadButtonMask::ButtonB),
    ]);
    let joy2_keymap: HashMap<Keycode, JoypadButtonMask> = HashMap::from([
        (Keycode::Down, JoypadButtonMask::Down),
        (Keycode::Up, JoypadButtonMask::Up),
        (Keycode::Right, JoypadButtonMask::Right),
        (Keycode::Left, JoypadButtonMask::Left),
        (Keycode::RCtrl, JoypadButtonMask::Select),
        (Keycode::RShift, JoypadButtonMask::Start),
        (Keycode::Comma, JoypadButtonMask::ButtonA),
        (Keycode::Period, JoypadButtonMask::ButtonB),
    ]);
    // Key mapping --------------------------------------------------------

    let rom = Rom::new(&fs::read("Pac-Man.nes").expect("Unable to open ROM")).unwrap();
    let mut frame = Frame::new();

    let bus = Bus::new(
        rom,
        move |ppu: &PPU, joypad1: &mut Joypad, joypad2: &mut Joypad| {
            render(ppu, &mut frame);
            texture.update(None, &frame.data, 256 * 3).unwrap();

            canvas.copy(&texture, None, None).unwrap();

            canvas.present();
            for event in event_pump.poll_iter() {
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => std::process::exit(0),

                    Event::KeyDown { keycode, .. } => {
                        if let Some(key) = joy1_keymap.get(&keycode.unwrap_or(Keycode::Ampersand)) {
                            joypad1.set_button_pressed_status(*key, true);
                        }
                        if let Some(key) = joy2_keymap.get(&keycode.unwrap_or(Keycode::Ampersand)) {
                            joypad2.set_button_pressed_status(*key, true);
                        }
                    }
                    Event::KeyUp { keycode, .. } => {
                        if let Some(key) = joy1_keymap.get(&keycode.unwrap_or(Keycode::Ampersand)) {
                            joypad1.set_button_pressed_status(*key, false);
                        }
                        if let Some(key) = joy2_keymap.get(&keycode.unwrap_or(Keycode::Ampersand)) {
                            joypad2.set_button_pressed_status(*key, false);
                        }
                    }

                    _ => { /* do nothing */ }
                }
            }
        },
    );

    let mut cpu = CPU::new(bus);

    // cpu.reset();
    cpu.run();
}
