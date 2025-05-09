use std::{fs, str::from_utf8};
use nes_rs::cpu::{CPU, cartridge::Rom, log::log};

#[test]
fn test_all_opcodes() {
    let mut rom = Rom::new(&fs::read("./tests/nestest/nestest.nes").unwrap()).unwrap();
    let test_file = fs::read("./tests/nestest/nestest_no_cycle.log").unwrap();

    // Remove the unimplemented APU testing part
    let mut removed = rom.prg_rom[1675..1699].to_vec();
    removed.fill(0);

    rom.prg_rom = [
        &rom.prg_rom[..1675],
        &removed,
        &rom.prg_rom[1699..],
    ]
    .concat();

    let mut cpu = CPU::new(rom);
    cpu.reset();
    cpu.program_counter = 0xC000;

    let mut expected_log: Vec<&str> = from_utf8(&test_file)
        .unwrap()
        .split('\n').collect();

    // Remove unimplemented test logs
    for _ in 0..12 {
        expected_log.pop();
    }

    let mut my_log: Vec<String> = Vec::with_capacity(expected_log.len());
    let expected_log = expected_log.join("\n");

    cpu.run_with_callback(|cpu| {
        let log = log(cpu);

        my_log.push(log);
    });

    // Remove the last BRK
    my_log.pop();
    let my_log = my_log.join("\n");

    assert!(my_log == expected_log);
}
