pub static WIDTH: usize = 256;
pub static HEIGHT: usize = 240;
pub struct Frame {
    pub data: [u8; WIDTH * HEIGHT * 3],
}

impl Frame {
    #![allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Frame {
            data: [0; WIDTH * HEIGHT * 3],
        }
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, color: (u8, u8, u8)) {
        let base = y * WIDTH * 3 + x * 3;
        if base + 2 < self.data.len() {
            self.data[base] = color.0;
            self.data[base + 1] = color.1;
            self.data[base + 2] = color.2;
        }
    }
}
