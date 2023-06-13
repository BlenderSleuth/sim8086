use std::fs::File;
use std::io::prelude::*;
use std::mem::swap;
use std::num::{NonZeroI16, NonZeroI8};
use std::path::Path;

#[derive(PartialEq)]
enum AddressDisplacement {
    Zero,
    Byte(NonZeroI8),
    Word(NonZeroI16),
}

impl AddressDisplacement {
    fn to_string(&self) -> String {
        use AddressDisplacement::*;

        match self {
            Zero => String::new(),
            Byte(disp) => disp.to_string(),
            Word(disp) => disp.to_string(),
        }
    }

    fn to_string_with_sign(&self) -> String {
        use AddressDisplacement::*;

        let disp = match self {
            Zero => return String::new(),
            Byte(disp) => disp.get() as i16,
            Word(disp) => disp.get(),
        };

        let sign = if disp > 0 { "+" } else { "-" };
        let disp_abs = disp.abs();

        format!("{sign} {disp_abs}")
    }

    fn to_direct_address_string(&self) -> String {
        use AddressDisplacement::*;

        match self {
            Zero => String::new(),
            Byte(_) | Word(_) => format!("[{}]", self.to_string()),
        }
    }
}

impl From<i16> for AddressDisplacement {
    fn from(value: i16) -> Self {
        match NonZeroI16::new(value) {
            Some(value) => Self::Word(value),
            None => Self::Zero,
        }
    }
}

impl From<u16> for AddressDisplacement {
    fn from(value: u16) -> Self {
        (value as i16).into()
    }
}

impl From<i8> for AddressDisplacement {
    fn from(value: i8) -> Self {
        match NonZeroI8::new(value) {
            Some(value) => Self::Byte(value),
            None => Self::Zero,
        }
    }
}

impl From<u8> for AddressDisplacement {
    fn from(value: u8) -> Self {
        (value as i8).into()
    }
}

// REG or R/M field
enum Register {
    AX,
    AH,
    AL,
    BX,
    BH,
    BL,
    CX,
    CH,
    CL,
    DX,
    DH,
    DL,
    SP,
    BP,
    SI,
    DI,
}

impl Register {
    // REG is truncated to 3 bits
    fn new(reg: u8, wflag: bool) -> Self {
        let reg = reg & 0b111;
        Self::new_checked(reg, wflag)
    }

    fn new_checked(reg: u8, wflag: bool) -> Self {
        assert!(reg <= 0b111);
        match (reg, wflag) {
            (0b000, false) => Self::AL,
            (0b000, true) => Self::AX,
            (0b001, false) => Self::CL,
            (0b001, true) => Self::CX,
            (0b010, false) => Self::DL,
            (0b010, true) => Self::DX,
            (0b011, false) => Self::BL,
            (0b011, true) => Self::BX,
            (0b100, false) => Self::AH,
            (0b100, true) => Self::SP,
            (0b101, false) => Self::CH,
            (0b101, true) => Self::BP,
            (0b110, false) => Self::DH,
            (0b110, true) => Self::SI,
            (0b111, false) => Self::BH,
            (0b111, true) => Self::DI,
            _ => unreachable!(),
        }
    }

    fn to_string(&self) -> &'static str {
        use Register::*;
        match self {
            AL => "al",
            AX => "ax",
            CL => "cl",
            CX => "cx",
            DL => "dl",
            DX => "dx",
            BL => "bl",
            BX => "bx",
            AH => "ah",
            SP => "sp",
            CH => "ch",
            BP => "bp",
            DH => "dh",
            SI => "si",
            BH => "bh",
            DI => "di",
        }
    }
}

enum SegmentRegister {
    ES,
    CS,
    SS,
    DS,
}

impl SegmentRegister {
    fn new(sr: u8) -> Self {
        let sr = sr & 0b11;
        Self::new_checked(sr)
    }

    fn new_checked(sr: u8) -> Self {
        use SegmentRegister::*;

        assert!(sr <= 0b11);

        match sr {
            0b00 => ES,
            0b01 => CS,
            0b10 => SS,
            0b11 => DS,
            _ => unreachable!(),
        }
    }

    fn to_string(&self) -> &'static str {
        use SegmentRegister::*;
        match self {
            ES => "es",
            CS => "cs",
            SS => "ss",
            DS => "ds",
        }
    }
}

struct AddressCalculation {
    rm: RegDisplacement,
    disp: AddressDisplacement,
    segment_override: Option<SegmentRegister>,
}

impl AddressCalculation {
    fn to_string(&self) -> String {
        if self.disp == AddressDisplacement::Zero {
            format!("[{}]", self.rm.to_string())
        } else {
            format!(
                "[{} {}]",
                self.rm.to_string(),
                self.disp.to_string_with_sign()
            )
        }
    }
}

// R/M field
enum RegDisplacement {
    BXpSI,
    BXpDI,
    BPpSI,
    BPpDI,
    SI,
    DI,
    BP,
    BX,
}

impl RegDisplacement {
    // R/M is truncated to 3 bits
    #[allow(dead_code)]
    fn new(rm: u8) -> Self {
        let rm = rm & 0b111;
        Self::new_checked(rm)
    }

    fn new_checked(rm: u8) -> Self {
        use RegDisplacement::*;

        assert!(rm <= 0b111);

        match rm {
            0b000 => BXpSI,
            0b001 => BXpDI,
            0b010 => BPpSI,
            0b011 => BPpDI,
            0b100 => SI,
            0b101 => DI,
            0b110 => BP,
            0b111 => BX,
            _ => unreachable!(),
        }
    }

    fn to_string(&self) -> &'static str {
        use RegDisplacement::*;

        match self {
            BXpSI => "bx + si",
            BXpDI => "bx + di",
            BPpSI => "bp + si",
            BPpDI => "bp + di",
            SI => "si",
            DI => "di",
            BP => "bp",
            BX => "bx",
        }
    }
}

enum RegisterMemory {
    DirectAddress(AddressDisplacement),
    Memory(AddressCalculation),
    Register(Register),
}

impl RegisterMemory {
    // R/M is truncated to 3 bits, mode is truncated to 2 bits
    #[allow(dead_code)]
    fn new(rm: u8, mode: u8, wflag: bool, disp: AddressDisplacement, segment_override: Option<SegmentRegister>) -> Self {
        let rm = rm & 0b111;
        let mode = mode & 0b111;
        Self::new_checked(rm, mode, wflag, disp, segment_override)
    }

    // R/M is truncated to 3 bits, mode is truncated to 2 bits
    fn new_from_instructions(
        rm: u8,
        mode: u8,
        wflag: bool,
        segment_override: Option<SegmentRegister>,
        i: &mut usize,
        instructions: &[u8],
    ) -> Self {
        let rm = rm & 0b111;
        let mode = mode & 0b111;

        let disp = match (mode, rm) {
            // Byte displacement mode
            (0b01, _) => {
                // 8-bit displacement
                get_byte(i, &instructions).into()
            }
            // Word displacement mode or Direct Address mode
            (0b10, _) | (0b00, 0b110) => {
                // 16-bit displacement
                get_word(i, &instructions).into()
            }
            _ => AddressDisplacement::Zero,
        };

        Self::new_checked(rm, mode, wflag, disp, segment_override)
    }

    fn new_checked(rm: u8, mode: u8, wflag: bool, disp: AddressDisplacement, segment_override: Option<SegmentRegister>) -> Self {
        assert!(rm <= 0b111);
        assert!(mode <= 0b11);

        match (rm, mode) {
            (0b110, 0b00) => RegisterMemory::DirectAddress(disp),
            (_, 0b00) | (_, 0b10) | (_, 0b01) => RegisterMemory::Memory(AddressCalculation {
                rm: RegDisplacement::new_checked(rm),
                disp,
                segment_override,
            }),
            (_, 0b11) => RegisterMemory::Register(Register::new_checked(rm, wflag)),
            _ => unreachable!(),
        }
    }

    fn to_string(&self) -> String {
        use RegisterMemory::*;

        match self {
            DirectAddress(disp) => disp.to_direct_address_string(),
            Memory(addr_calc) => { 
                let addr_calc_str = addr_calc.to_string();
                match &addr_calc.segment_override {
                    Some(segment) => format!("{}:{addr_calc_str}", segment.to_string()),
                    None => addr_calc_str,
                }
            }
            Register(reg) => format!("[{}]", reg.to_string())
        }
    }
}

// Gets the next byte from the instruction stream and increments i
fn get_byte(i: &mut usize, instructions: &[u8]) -> u8 {
    *i += 1;
    instructions[*i]
}

// Gets the next word from the instruction stream and increments i
fn get_word(i: &mut usize, instructions: &[u8]) -> u16 {
    let lo_byte = get_byte(i, instructions) as u16;
    let hi_byte = get_byte(i, instructions) as u16;
    hi_byte << 8 | lo_byte
}

// Gets signed data
fn get_data(wflag: bool, i: &mut usize, instructions: &[u8]) -> u16 {
    if wflag {
        get_word(i, instructions)
    } else {
        get_byte(i, instructions) as u16
    }
}

fn get_signed_data(wflag: bool, i: &mut usize, instructions: &[u8]) -> i16 {
    if wflag {
        get_word(i, instructions) as i16
    } else {
        get_byte(i, instructions) as i16
    }
}

// Returns a bool flag corresponding to the value of a specified bit
fn flag(value: u8, bit: u8) -> bool {
    ((value >> bit) & 1) != 0
}

fn main() -> std::io::Result<()> {
    let part_one_tests = Path::new("../../computer_enhance/perfaware/part1");

    // listing_0037_single_register_mov
    // listing_0038_many_register_mov
    // listing_0039_more_movs
    // listing_0040_challenge_movs
    // listing_0041_add_sub_cmp_jnz
    // listing_0042_completionist_decode
    let listing_path = part_one_tests.join(Path::new("listing_0042_completionist_decode"));

    // output
    let test_path = Path::new("output");

    let mut file = match File::open(&test_path) {
        Err(why) => panic!("couldn't open {}: {}", listing_path.display(), why),
        Ok(file) => file,
    };

    let mut instructions = Vec::<u8>::new();
    file.read_to_end(&mut instructions)?;

    println!("bits 16");

    let mut segment_override: Option<SegmentRegister> = None;
    let mut i = 0;
    while i < instructions.len() {
        let byte1 = instructions[i];

        let mut opcode = 0;
        for op_bit in (0..8).rev() {
            opcode <<= 1;
            opcode |= (byte1 >> op_bit) & 1;

            let line_opt = match opcode {
                // Segment override
                0b00100110 => {
                    // ES
                    segment_override = Some(SegmentRegister::ES);
                    break;
                }
                0b00101110 => {
                    // CS
                    segment_override = Some(SegmentRegister::CS);
                    break;
                }
                0b00110110 => {
                    // SS
                    segment_override = Some(SegmentRegister::SS);
                    break;
                }
                0b00111110 => {
                    // DS
                    segment_override = Some(SegmentRegister::DS);
                    break;
                }
                // MOV
                0b100010 => {
                    // Mov (Register/memory to/from register)
                    let opcode_name = "mov";

                    let dflag = flag(byte1, 1);
                    let wflag = flag(byte1, 0);

                    let byte2 = get_byte(&mut i, &instructions);

                    let mode_bits = byte2 >> 6;
                    let reg_bits = byte2 >> 3;
                    let rm_bits = byte2;

                    let rm = RegisterMemory::new_from_instructions(
                        rm_bits,
                        mode_bits,
                        wflag,
                        segment_override.take(),
                        &mut i,
                        &instructions,
                    );
                    let rm_str = rm.to_string();

                    let reg = Register::new(reg_bits, wflag);
                    let reg_str = reg.to_string();

                    let mut source = reg_str;
                    let mut dest = rm_str.as_str();
                    if dflag {
                        swap(&mut source, &mut dest);
                    }

                    Some(format!("{opcode_name} {dest}, {source}"))
                }
                0b1100011 => {
                    // Mov (Immediate to register/memory)
                    let opcode_name = "mov";

                    let wflag = flag(byte1, 0);

                    let byte2 = get_byte(&mut i, &instructions);

                    let mode_bits = byte2 >> 6;
                    let rm_bits = byte2;

                    let rm = RegisterMemory::new_from_instructions(
                        rm_bits,
                        mode_bits,
                        wflag,
                        segment_override.take(),
                        &mut i,
                        &instructions,
                    );
                    let rm_str = rm.to_string();

                    let immediate = get_signed_data(wflag, &mut i, &instructions);

                    let immediate_type = if wflag { "word" } else { "byte" };

                    Some(format!("{opcode_name} {rm_str}, {immediate_type} {immediate}"))
                }
                0b1011 => {
                    // Mov (Immediate to register)
                    let opcode_name = "mov";

                    let wflag = flag(byte1, 3);
                    let reg_bits = byte1;
                    let reg = Register::new(reg_bits, wflag);
                    let reg_str = reg.to_string();

                    let immediate = get_signed_data(wflag, &mut i, &instructions);

                    Some(format!("{opcode_name} {reg_str}, {immediate}"))
                }
                0b101000 => {
                    // Mov (Memory to accumulator / Accumulator to memory)
                    let opcode_name = "mov";

                    let dflag = flag(byte1, 1);
                    let wflag = flag(byte1, 0);

                    let addr = get_data(wflag, &mut i, &instructions);
                    let addr_str = format!("[{addr}]");

                    let mut source = "ax";
                    let mut dest = addr_str.as_str();
                    if dflag {
                        swap(&mut source, &mut dest);
                    }

                    Some(format!("{opcode_name} {source}, {dest}"))
                }
                0b10001110 | 0b10001100 => {
                    // Mov (Register/memory to segment register / Segment register to register/memory)
                    let opcode_name = "mov";

                    let dflag = flag(byte1, 1);
                    let wflag = true;

                    let byte2 = get_byte(&mut i, &instructions);

                    let mode_bits = byte2 >> 6;
                    let sr_bits = byte2 >> 3;
                    let rm_bits = byte2;

                    let sr = SegmentRegister::new(sr_bits);
                    let sr_str = sr.to_string();

                    let rm = RegisterMemory::new_from_instructions(
                        rm_bits,
                        mode_bits,
                        wflag,
                        segment_override.take(),
                        &mut i,
                        &instructions,
                    );
                    let rm_str = rm.to_string();

                    let mut source = sr_str;
                    let mut dest = rm_str.as_str();
                    if dflag {
                        swap(&mut source, &mut dest);
                    }

                    Some(format!("{opcode_name} {dest}, {source}"))
                }
                // ADD
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
