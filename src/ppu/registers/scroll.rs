pub struct ScrollRegister {
    scroll_x: u8,
    scroll_y: u8,
    latch: bool,
}

impl ScrollRegister {
    #![allow(clippy::new_without_default)]
    pub fn new() -> Self {
        ScrollRegister {
            scroll_x: 0,
            scroll_y: 0,
            latch: false,
        }
    }

    pub fn write(&mut self, data: u8) {
        match self.latch {
            true => self.scroll_y = data,
            false => self.scroll_x = data,
        }

        self.latch = !self.latch;
    }

    pub fn reset_latch(&mut self) {
        self.latch = false;
    }
}
