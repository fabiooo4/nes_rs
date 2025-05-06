pub struct AddressRegister {
    value: (u8, u8),
    high_byte: bool,
}

impl AddressRegister {
    #![allow(clippy::new_without_default)]
    pub fn new() -> Self {
        AddressRegister {
            value: (0, 0),
            high_byte: true,
        }
    }

    /// Sets the address register with the given 2 bytes
    fn set(&mut self, data: u16) {
        let high = (data >> 8) as u8;
        let low = (data & 0xff) as u8;

        self.value.0 = high;
        self.value.1 = low;
    }

    /// Updates the address register with 1 byte
    pub fn update(&mut self, data: u8) {
        match self.high_byte {
            true => self.value.0 = data,
            false => self.value.1 = data,
        }

        // Mirroring
        if self.get() > 0x3FFF {
            self.set(self.get() & 0x3FFF);
        }

        self.high_byte = !self.high_byte;
    }

    /// Increments the address value based on the control register
    pub fn increment(&mut self, inc: u8) {
        let mut res = self.get().wrapping_add(inc as u16);

        // Mirroring
        if res > 0x3FFF {
            res &= 0x3FFF;
        }

        self.set(res);
    }

    /// Resets the pointer to the high byte
    pub fn reset_latch(&mut self) {
        self.high_byte = true;
    }

    /// Gets the stored 16 bit value
    pub fn get(&self) -> u16 {
        ((self.value.0 as u16) << 8) | self.value.1 as u16
    }
}
