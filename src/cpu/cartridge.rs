const NES_TAG: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
const PRG_ROM_PAGE_SIZE: usize = 16384;
const CHR_ROM_PAGE_SIZE: usize = 8192;

#[derive(Debug, PartialEq)]
pub enum Mirroring {
    Vertical,
    Horizontal,
    FourScreen,
}

pub struct Rom {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub mapper: u8,
    pub screen_mirroring: Mirroring,
}

impl Rom {
    pub fn new(raw: &[u8]) -> Result<Rom, &str> {
        // Check if the binary is iNES format
        if raw[0..4] != NES_TAG {
            return Err("Binary file is not in iNES format");
        }

        // Check iNES version
        if raw[7] & 0b0000_1100 != 0 {
            // return Err("NES2.0 format is not supported");
        }

        // Type of mapper
        let mapper: u8 = (raw[6] >> 4) | raw[7] & 0b1111_0000;

        let four_screen = (raw[6] >> 3) & 1 == 1;
        let mirroring = raw[6] & 1 == 1;

        let screen_mirroring = match (four_screen, mirroring) {
            (true, _) => Mirroring::FourScreen,
            (false, true) => Mirroring::Vertical,
            (false, false) => Mirroring::Horizontal,
        };

        // If there is a trainer skip it
        let skip_trainer: bool = raw[6] & 0b100 != 0;

        let prg_rom_size: usize = raw[4] as usize * PRG_ROM_PAGE_SIZE;
        let chr_rom_size: usize = raw[5] as usize * CHR_ROM_PAGE_SIZE;

        let prg_rom_start: usize = 16 + if skip_trainer { 512 } else { 0 };
        let chr_rom_start: usize = prg_rom_start + prg_rom_size;

        Ok(Rom {
            prg_rom: raw[prg_rom_start..(prg_rom_start + prg_rom_size)].to_vec(),
            chr_rom: raw[chr_rom_start..(chr_rom_start + chr_rom_size)].to_vec(),
            mapper,
            screen_mirroring,
        })
    }
}

pub mod test {

    use super::*;

    struct TestRom {
        header: Vec<u8>,
        trainer: Option<Vec<u8>>,
        pgp_rom: Vec<u8>,
        chr_rom: Vec<u8>,
    }

    fn create_rom(rom: TestRom) -> Vec<u8> {
        let mut result = Vec::with_capacity(
            rom.header.len()
                + rom.trainer.as_ref().map_or(0, |t| t.len())
                + rom.pgp_rom.len()
                + rom.chr_rom.len(),
        );

        result.extend(&rom.header);
        if let Some(t) = rom.trainer {
            result.extend(t);
        }
        result.extend(&rom.pgp_rom);
        result.extend(&rom.chr_rom);

        result
    }

    pub fn test_rom(program: Vec<u8>) -> Rom {
        let mut pgp_rom_contents = program;
        pgp_rom_contents.resize(2 * PRG_ROM_PAGE_SIZE, 0);

        let test_rom = create_rom(TestRom {
            header: vec![
                0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 00, 00, 00, 00, 00, 00, 00, 00, 00,
            ],
            trainer: None,
            pgp_rom: pgp_rom_contents,
            chr_rom: vec![2; CHR_ROM_PAGE_SIZE],
        });

        Rom::new(&test_rom).unwrap()
    }

    #[test]
    fn test() {
        let test_rom = create_rom(TestRom {
            header: vec![
                0x4E, 0x45, 0x53, 0x1A, 0x02, 0x01, 0x31, 00, 00, 00, 00, 00, 00, 00, 00, 00,
            ],
            trainer: None,
            pgp_rom: vec![1; 2 * PRG_ROM_PAGE_SIZE],
            chr_rom: vec![2; CHR_ROM_PAGE_SIZE],
        });

        let rom: Rom = Rom::new(&test_rom).unwrap();

        assert_eq!(rom.chr_rom, vec!(2; CHR_ROM_PAGE_SIZE));
        assert_eq!(rom.prg_rom, vec!(1; 2 * PRG_ROM_PAGE_SIZE));
        assert_eq!(rom.mapper, 3);
        assert_eq!(rom.screen_mirroring, Mirroring::Vertical);
    }

    #[test]
    fn test_with_trainer() {
        let test_rom = create_rom(TestRom {
            header: vec![
                0x4E,
                0x45,
                0x53,
                0x1A,
                0x02,
                0x01,
                0x31 | 0b100,
                00,
                00,
                00,
                00,
                00,
                00,
                00,
                00,
                00,
            ],
            trainer: Some(vec![0; 512]),
            pgp_rom: vec![1; 2 * PRG_ROM_PAGE_SIZE],
            chr_rom: vec![2; CHR_ROM_PAGE_SIZE],
        });

        let rom: Rom = Rom::new(&test_rom).unwrap();

        assert_eq!(rom.chr_rom, vec!(2; CHR_ROM_PAGE_SIZE));
        assert_eq!(rom.prg_rom, vec!(1; 2 * PRG_ROM_PAGE_SIZE));
        assert_eq!(rom.mapper, 3);
        assert_eq!(rom.screen_mirroring, Mirroring::Vertical);
    }

    #[test]
    fn test_nes2_is_not_supported() {
        let test_rom = create_rom(TestRom {
            header: vec![
                0x4E, 0x45, 0x53, 0x1A, 0x01, 0x01, 0x31, 0x8, 00, 00, 00, 00, 00, 00, 00, 00,
            ],
            trainer: None,
            pgp_rom: vec![1; PRG_ROM_PAGE_SIZE],
            chr_rom: vec![2; CHR_ROM_PAGE_SIZE],
        });
        let rom = Rom::new(&test_rom);
        match rom {
            Result::Ok(_) => unreachable!("should not load rom"),
            Result::Err(str) => assert_eq!(str, "NES2.0 format is not supported"),
        }
    }
}
