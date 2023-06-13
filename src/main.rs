use std::fs::File;
use std::io::prelude::*;
use std::mem::swap;
use std::num::{NonZeroI8, NonZeroU16};
use std::path::Path;

enum AddrDisplacement {
    Zero,
    Signed(NonZeroI8),
    Word(NonZeroU16),
}

impl From<u16> for AddrDisplacement {
    fn from(value: u16) -> Self {
        match NonZeroU16::new(value)
        {
            Some(value) => Self::Word(value),
            None => Self::Zero,
        }
    }
}

impl From<Option<u16>> for AddrDisplacement {
    fn from(value: Option<u16>) -> Self {
        match value {
            None => Self::Zero,
            Some(value) => value.into(),
        }
    }
}

impl From<i8> for AddrDisplacement {
    fn from(value: i8) -> Self {
        match NonZeroI8::new(value)
        {
            Some(value) => Self::Signed(value),
            None => Self::Zero,
        }
    }
}

impl From<Option<i8>> for AddrDisplacement {
    fn from(value: Option<i8>) -> Self {
        match value {
            None => Self::Zero,
            Some(value) => value.into(),
        }
    }
}

// Use opcode enum for exhaustive matches(with From trait)
fn get_reg_name(reg: u8, wflag: u8) -> &'static str {
    match (reg, wflag) {
        (0b000, 0) => "al",
        (0b000, 1) => "ax",
        (0b001, 0) => "cl",
        (0b001, 1) => "cx",
        (0b010, 0) => "dl",
        (0b010, 1) => "dx",
        (0b011, 0) => "bl",
        (0b011, 1) => "bx",
        (0b100, 0) => "ah",
        (0b100, 1) => "sp",
        (0b101, 0) => "ch",
        (0b101, 1) => "bp",
        (0b110, 0) => "dh",
        (0b110, 1) => "si",
        (0b111, 0) => "bh",
        (0b111, 1) => "di",
        _ => unreachable!(),
    }
}

fn get_effective_addr(rm: u8, mode: u8, disp: AddrDisplacement) -> String {
    use AddrDisplacement::*;

    let disp_str = match disp {
        Zero => String::new(),
        Signed(disp) => {
            let sign = if disp.get() > 0 { "+" } else { "-" };
            format!("{sign} {disp}")
        }
        Word(disp) => format!(" + {disp}"),
    };

    let rm_str = match rm {
        0b000 => "bx + si",
        0b001 => "bx + di",
        0b010 => "bp + si",
        0b011 => "bp + di",
        0b100 => "si",
        0b101 => "di",
        0b110 => {
            if mode == 0b00 {
                ""
            } else {
                "bp"
            }
        }
        0b111 => "bx",
        _ => unreachable!(),
    };

    format!("[{rm_str}{disp_str}]")
}

// Gets the next byte from the instruction stream and increments i
fn get_byte(i: &mut usize, instructions: &[u8]) -> u8 {
    *i += 1;
    instructions[*i]
}

fn get_byte_signed(i: &mut usize, instructions: &[u8]) -> i8 {
    let byte = get_byte(i, instructions);
    byte as i8
}

// Gets the next word from the instruction stream and increments i
fn get_word(i: &mut usize, instructions: &[u8]) -> u16 {
    let lo_byte = get_byte(i, instructions) as u16;
    let hi_byte = get_byte(i, instructions) as u16;
    hi_byte << 8 | lo_byte
}

fn main() -> std::io::Result<()> {
    let part_one_tests = Path::new("../../computer_enhance/perfaware/part1");

    // listing_0037_single_register_mov
    // listing_0038_many_register_mov
    // listing_0039_more_movs
    // listing_0040_challenge_movs
    let listing_path = part_one_tests.join(Path::new("listing_0038_many_register_mov"));

    let mut file = match File::open(&listing_path) {
        Err(why) => panic!("couldn't open {}: {}", listing_path.display(), why),
        Ok(file) => file,
    };

    let mut instructions = Vec::<u8>::new();
    file.read_to_end(&mut instructions)?;

    println!("bits 16");

    let mut i = 0;
    while i < instructions.len() {
        let byte1 = instructions[i];

        let mut opcode = 0;
        for op_bit in (0..8).rev() {
            opcode <<= 1;
            opcode |= (byte1 >> op_bit) & 1;

            let line_opt = match opcode {
                0b100010u8 => {
                    // Mov (Register/memory to/from register)
                    let opcode_name = "mov";

                    let dflag = (byte1 >> 1) & 1;
                    let wflag = byte1 & 1;

                    let byte2 = get_byte(&mut i, &instructions);

                    let mode = byte2 >> 6;
                    let reg = (byte2 >> 3) & 0b111;
                    let reg_str = get_reg_name(reg, wflag);

                    let rm = byte2 & 0b111;

                    let rm_str = match mode {
                        0b00 => {
                            let disp = if rm == 110 {
                                // 16-bit displacement for direct address mode
                                get_word(&mut i, &instructions)
                            } else {
                                0
                            };
                            get_effective_addr(rm, mode, disp.into())
                        }
                        0b01 => {
                            // 8-bit signed displacement
                            let disp = get_byte_signed(&mut i, &instructions);
                            get_effective_addr(rm, mode, disp.into())
                        }
                        0b10 => {
                            // 16-bit displacement
                            let disp = get_word(&mut i, &instructions);
                            get_effective_addr(rm, mode, disp.into())
                        }
                        0b11 => String::from(get_reg_name(rm, wflag)),
                        _ => panic!("unknown mov mode field code"),
                    };

                    let mut source = reg_str;
                    let mut dest = rm_str.as_str();

                    if dflag == 1 {
                        swap(&mut source, &mut dest);
                    }

                    Some(format!("{opcode_name} {dest}, {source}"))
                }
                0b1100011 => {
                    // Mov (Immediate to register/memory)
                    let opcode_name = "mov";

                    Some(format!("{opcode_name} unknown"))
                }
                0b1011 => {
                    // Mov (Immediate to register)
                    let opcode_name = "mov";

                    let wflag = (byte1 >> 3) & 1;
                    let reg = byte1 & 0b111;
                    let reg_str = get_reg_name(reg, wflag);

                    let low_byte = get_byte(&mut i, &instructions);

                    let mut immediate = low_byte as u16;

                    if wflag == 1 {
                        let hi_byte = get_byte(&mut i, &instructions) as u16;
                        immediate |= hi_byte << 8;
                    }

                    Some(format!("{opcode_name} {reg_str}, {immediate}"))
                }
                _ => None,
            };

            if let Some(line) = line_opt {
                println!("{line}");
                break;
            }

            if op_bit == 0 {
                // Opcode didn't match
                println!("unknown");
            }
        }

        i += 1;
    }

    Ok(())
}
