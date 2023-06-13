use std::fs::File;
use std::io::prelude::*;
use std::mem::swap;
use std::num::{NonZeroI8, NonZeroI16};
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
}

impl From<i16> for AddressDisplacement {
    fn from(value: i16) -> Self {
        match NonZeroI16::new(value)
        {
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
        match NonZeroI8::new(value)
        {
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
    AX, AH, AL, BX, BH, BL,
    CX, CH, CL, DX, DH, DL,
    SP, BP, SI, DI
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

struct AddressCalculation {
    rm: RegDisplacement,
    disp: AddressDisplacement,
}

impl AddressCalculation {
    fn to_string(&self) -> String {
        if self.disp == AddressDisplacement::Zero
        {
            String::from(self.rm.to_string())
        } else {
            format!("{} {}", self.rm.to_string(), self.disp.to_string_with_sign())
        }
    }
}

// R/M field
enum RegDisplacement {
    BXpSI, BXpDI, BPpSI, BPpDI,
    SI, DI, BP, BX,
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
    Register(Register)
}

impl RegisterMemory {
    // R/M is truncated to 3 bits, mode is truncated to 2 bits
    #[allow(dead_code)]
    fn new(rm: u8, mode: u8, wflag: bool, disp: AddressDisplacement) -> Self {
        let rm = rm & 0b111;
        let mode = mode & 0b111;
        Self::new_checked(rm, mode, wflag, disp)
    }
    
    // R/M is truncated to 3 bits, mode is truncated to 2 bits
    fn new_from_instructions(rm: u8, mode: u8, wflag: bool, i: &mut usize, instructions: &[u8]) -> Self {
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
            _ => AddressDisplacement::Zero
        };

        Self::new_checked(rm, mode, wflag, disp)
    }

    fn new_checked(rm: u8, mode: u8, wflag: bool, disp: AddressDisplacement) -> Self {
        assert!(rm <= 0b111);
        assert!(mode <= 0b11);

        match (rm, mode) {
            (0b110, 0b00) => {
                RegisterMemory::DirectAddress(disp)
            }
            (_, 0b00) | (_, 0b10) | (_, 0b01)  => {
                RegisterMemory::Memory(AddressCalculation {
                    rm: RegDisplacement::new_checked(rm),
                    disp
                })
            }
            (_, 0b11) => RegisterMemory::Register(Register::new_checked(rm, wflag)),
            _ => unreachable!()
        }
    }
    
    fn to_string(&self) -> String {
        use RegisterMemory::*;
        
        let disp_str = match self {
            DirectAddress(disp) => disp.to_string(),
            Memory(addr_calc) => addr_calc.to_string(),
            Register(reg) => return String::from(reg.to_string()),
        };
        
        format!("[{disp_str}]")
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

fn get_immediate(wflag: bool, i: &mut usize, instructions: &[u8]) -> i16 {
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
    let listing_path = part_one_tests.join(Path::new("listing_0040_challenge_movs"));

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

                    let dflag = flag(byte1, 1);
                    let wflag = flag(byte1, 0);

                    let byte2 = get_byte(&mut i, &instructions);

                    let mode_bits = byte2 >> 6;
                    let reg_bits = byte2 >> 3;
                    let rm_bits = byte2;
                    
                    let rm = RegisterMemory::new_from_instructions(rm_bits, mode_bits, wflag, &mut i, &instructions);
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

                    let rm = RegisterMemory::new_from_instructions(rm_bits, mode_bits, wflag, &mut i, &instructions);
                    let rm_str = rm.to_string();
                    
                    let immediate = get_immediate(wflag, &mut i, &instructions);
                    
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

                    let low_byte = get_byte(&mut i, &instructions);

                    let mut immediate = low_byte as u16;

                    if wflag {
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
