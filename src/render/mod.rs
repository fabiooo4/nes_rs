use frame::Frame;

pub mod frame;
pub mod palette;

pub fn show_tile(chr_rom: &[u8], bank: usize, tile_n: usize) -> Frame {
    assert!(bank <= 1);
    assert!(tile_n <= 512);

    let mut frame = Frame::new();
    let tile_size = 16;
    let bank_n = bank * 0x1000;

    let tile =
        &chr_rom[(bank_n + tile_size * tile_n)..(bank_n + tile_size * tile_n + (tile_size - 1))];

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
    let tile_size = 16;
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
            [(bank_n + tile_size * tile_n)..(bank_n + tile_size * tile_n + (tile_size - 1))];

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
