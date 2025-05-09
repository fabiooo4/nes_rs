use frame::Frame;
use palette::{bg_palette, sprite_palette};

use crate::ppu::PPU;

pub mod frame;
pub mod palette;

pub static TILE_SIZE: usize = 16;

pub fn render(ppu: &PPU, frame: &mut Frame) {
    let bank = ppu.ctrl.get_background_pattern_addr();

    // Draw background
    for i in 0..0x03c0 {
        let tile = ppu.vram[i] as u16;
        let tile_column = i % 32;
        let tile_row = i / 32;

        let tile = &ppu.chr_rom[(bank + TILE_SIZE as u16 * tile) as usize
            ..=(bank + TILE_SIZE as u16 * tile + (TILE_SIZE as u16 - 1)) as usize];

        let palette = bg_palette(ppu, tile_column, tile_row);

        for y in 0..8 {
            let mut upper_byte = tile[y];
            let mut lower_byte = tile[y + 8];

            for x in (0..8).rev() {
                let value = (lower_byte & 1) << 1 | (upper_byte & 1);

                upper_byte >>= 1;
                lower_byte >>= 1;

                let color = match value {
                    // Default background color
                    0 => palette::SYSTEM_PALLETE[ppu.palette_table[0] as usize],

                    1 => palette::SYSTEM_PALLETE[palette[1] as usize],
                    2 => palette::SYSTEM_PALLETE[palette[2] as usize],
                    3 => palette::SYSTEM_PALLETE[palette[3] as usize],
                    _ => unreachable!(),
                };

                frame.set_pixel(tile_column * 8 + x, tile_row * 8 + y, color);
            }
        }
    }

    // Draw sprites
    for i in (0..ppu.oam_data.len()).step_by(4).rev() {
        let tile_idx = ppu.oam_data[i + 1] as u16;
        let tile_x = ppu.oam_data[i + 3] as usize;
        let tile_y = ppu.oam_data[i] as usize;

        let flip_vertical = ppu.oam_data[i + 2] >> 7 & 1 == 1;
        let flip_horizontal = ppu.oam_data[i + 2] >> 6 & 1 == 1;

        let palette_idx = ppu.oam_data[i + 2] & 0b11;
        let sprite_palette = sprite_palette(ppu, palette_idx);

        let bank: u16 = ppu.ctrl.get_sprite_pattern_addr();

        let tile =
            &ppu.chr_rom[(bank + tile_idx * 16) as usize..=(bank + tile_idx * 16 + 15) as usize];

        for y in 0..8 {
            let mut upper = tile[y];
            let mut lower = tile[y + 8];
            'skip: for x in (0..8).rev() {
                let value = (lower & 1) << 1 | (upper & 1);

                upper >>= 1;
                lower >>= 1;

                let color = match value {
                    0 => continue 'skip, // Transparent
                    1 => palette::SYSTEM_PALLETE[sprite_palette[1] as usize],
                    2 => palette::SYSTEM_PALLETE[sprite_palette[2] as usize],
                    3 => palette::SYSTEM_PALLETE[sprite_palette[3] as usize],
                    _ => panic!("can't be"),
                };

                match (flip_horizontal, flip_vertical) {
                    (false, false) => frame.set_pixel(tile_x + x, tile_y + y, color),
                    (true, false) => frame.set_pixel(tile_x + 7 - x, tile_y + y, color),
                    (false, true) => frame.set_pixel(tile_x + x, tile_y + 7 - y, color),
                    (true, true) => frame.set_pixel(tile_x + 7 - x, tile_y + 7 - y, color),
                }
            }
        }
    }
}

pub fn show_tile(chr_rom: &[u8], bank: usize, tile_n: usize) -> Frame {
    assert!(bank <= 1);
    assert!(tile_n <= 512);

    let mut frame = Frame::new();
    let bank_n = bank * 0x1000;

    let tile =
        &chr_rom[(bank_n + TILE_SIZE * tile_n)..(bank_n + TILE_SIZE * tile_n + (TILE_SIZE - 1))];

    for y in 0..7 {
        let mut upper_byte = tile[y];
        let mut lower_byte = tile[y + 8];

        for x in (0..7).rev() {
            // Merge the lowest bit of the two bytes 0x0001 + 0x0000 -> 0x10
            let value = (upper_byte & 1) << 1 | (lower_byte & 1);

            upper_byte >>= 1;
            lower_byte >>= 1;

            let color = match value {
                0 => palette::SYSTEM_PALLETE[0x01],
                1 => palette::SYSTEM_PALLETE[0x02],
                2 => palette::SYSTEM_PALLETE[0x03],
                3 => palette::SYSTEM_PALLETE[0x04],
                _ => unreachable!(),
            };

            frame.set_pixel(x, y, color);
        }
    }

    frame
}

pub fn show_tile_bank(chr_rom: &[u8], bank: usize) -> Frame {
    assert!(bank <= 1);

    let mut frame = Frame::new();
    let bank_n = bank * 0x1000;

    let mut tile_x = 0;
    let mut tile_y = 0;
    let gap = 2;

    for tile_n in 0..255 {
        if tile_n != 0 && tile_n % 20 == 0 {
            tile_y += 7 + gap;
            tile_x = 0;
        }

        let tile = &chr_rom
            [(bank_n + TILE_SIZE * tile_n)..(bank_n + TILE_SIZE * tile_n + (TILE_SIZE - 1))];

        for y in 0..7 {
            let mut upper_byte = tile[y];
            let mut lower_byte = tile[y + 8];

            for x in (0..7).rev() {
                // Merge the lowest bit of the two bytes 0x0001 + 0x0000 -> 0x10
                let value = (upper_byte & 1) << 1 | (lower_byte & 1);

                upper_byte >>= 1;
                lower_byte >>= 1;

                let color = match value {
                    0 => palette::SYSTEM_PALLETE[0x01],
                    1 => palette::SYSTEM_PALLETE[0x02],
                    2 => palette::SYSTEM_PALLETE[0x03],
                    3 => palette::SYSTEM_PALLETE[0x04],
                    _ => unreachable!(),
                };

                frame.set_pixel(tile_x + x, tile_y + y, color);
            }
        }

        tile_x += 7 + gap;
    }

    frame
}
