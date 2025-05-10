<p align="center">
    <img src="https://github.com/user-attachments/assets/584250a8-6d06-4846-95f6-1f9f62dbd7d5" width="400">
</p>
<br/>

---

This is a simple NES emulator written in Rust. It is a work in progress and is not
yet fully functional. The goal of this project is to learn about emulation and the
NES architecture.

## Features

- [x] CPU emulation
- [x] BUS emulation
- [x] ROM loading
- [ ] PPU emulation
- [x] Input handling
- [ ] APU emulation

## Installation

### Pre-requisites

- [SDL2](https://wiki.libsdl.org/SDL2/Installation)

### Build

```bash
git clone https://github.com/fabiooo4/nes_rs.git
cd nes_rs
cargo build
```

### Run

```bash
cargo run --release
```

#### Debug mode with step by step execution

```bash
cargo run
```

## References

- [Easy 6502](https://skilldrick.github.io/easy6502/) - 6502 Assembly guide
- [The NES Dev Instruction Reference](https://www.nesdev.org/obelisk-6502-guide/reference.html#AND)
- [6502 Instruction Reference](http://www.6502.org/tutorials/6502opcodes.html)
- [Obelisk 6502 Guide](https://www.nesdev.org/obelisk-6502-guide/) - General 6502 guide
- [The NES Dev Wiki](https://www.nesdev.org/wiki/Nesdev_Wiki)
- [NES Dev Undocupented Opcodes](https://www.nesdev.org/undocumented_opcodes.txt)
