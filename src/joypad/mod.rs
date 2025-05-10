use buttons::JoypadButtons;

use crate::BitFlags;

pub mod buttons;

#[derive(Debug)]
pub struct Joypad {
    /// Resets pointer to button_a
    strobe: bool,
    button_idx: u8,
    joypad_button: JoypadButtons,
}

impl Joypad {
    #![allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Joypad {
            strobe: false,
            button_idx: 0,
            joypad_button: JoypadButtons::new(),
        }
    }

    pub fn write(&mut self, data: u8) {
        self.strobe = data & 1 == 1;
        if self.strobe {
            self.button_idx = 0
        }
    }

    pub fn read(&mut self) -> u8 {
        if self.button_idx > 7 {
            return 1;
        }
        let response = (self.joypad_button.as_byte() & (1 << self.button_idx)) >> self.button_idx;
        if !self.strobe && self.button_idx <= 7 {
            self.button_idx += 1;
        }
        response
    }

    pub fn set_button_pressed_status(
        &mut self,
        key_mask: buttons::JoypadButtonMask,
        pressed: bool,
    ) {
        match key_mask {
            buttons::JoypadButtonMask::Right => self.joypad_button.right = pressed,
            buttons::JoypadButtonMask::Left => self.joypad_button.left = pressed,
            buttons::JoypadButtonMask::Down => self.joypad_button.down = pressed,
            buttons::JoypadButtonMask::Up => self.joypad_button.up = pressed,
            buttons::JoypadButtonMask::Start => self.joypad_button.start = pressed,
            buttons::JoypadButtonMask::Select => self.joypad_button.select = pressed,
            buttons::JoypadButtonMask::ButtonB => self.joypad_button.button_b = pressed,
            buttons::JoypadButtonMask::ButtonA => self.joypad_button.button_a = pressed,
        }
    }
}

#[cfg(test)]
mod test {
    use super::{buttons::JoypadButtonMask, *};

    #[test]
    fn test_strobe_mode() {
        let mut joypad = Joypad::new();
        joypad.write(1);
        joypad.set_button_pressed_status(buttons::JoypadButtonMask::ButtonA, true);
        for _ in 0..10 {
            assert_eq!(joypad.read(), 1);
        }
    }

    #[test]
    fn test_strobe_mode_on_off() {
        let mut joypad = Joypad::new();

        joypad.write(0);
        assert!(!joypad.strobe);

        joypad.set_button_pressed_status(JoypadButtonMask::Right, true);
        joypad.set_button_pressed_status(JoypadButtonMask::Left, true);
        joypad.set_button_pressed_status(JoypadButtonMask::Select, true);
        joypad.set_button_pressed_status(JoypadButtonMask::ButtonB, true);

        for _ in 0..=1 {
            assert_eq!(joypad.read(), 0);
            assert_eq!(joypad.read(), 1);
            assert_eq!(joypad.read(), 1);
            assert_eq!(joypad.read(), 0);
            assert_eq!(joypad.read(), 0);
            assert_eq!(joypad.read(), 0);
            assert_eq!(joypad.read(), 1);
            assert_eq!(joypad.read(), 1);

            for _ in 0..10 {
                assert_eq!(joypad.read(), 1);
            }
            joypad.write(1);
            joypad.write(0);
        }
    }
}
