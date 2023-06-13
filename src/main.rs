#![allow(dead_code)]

use std::fs::File;
use std::io::prelude::*;
use std::num::{NonZeroI16, NonZeroI8};
use std::path::Path;
use std::fmt::{self, Formatter};

#[derive(PartialEq)]
enum AddressDisplacement {
    Zero,
    Byte(NonZeroI8),
    Word(NonZeroI16),
}

impl fmt::Display for AddressDisplacement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use AddressDisplacement::*;

        let disp = match self {
            Zero => return write!(f, ""),
            Byte(disp) => disp.get() as i16,
            Word(disp) => disp.get(),
        };

        let sign = if disp > 0 { "+" } else { "-" };
        let disp_abs = disp.abs();

        write!(f, "{sign} {disp_abs}")
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

impl AddressDisplacement {
    fn to_signed(&self) -> i16 {
        use AddressDisplacement::*;
        match self {
            Zero => 0,
            Byte(disp) => disp.get() as i16,
            Word(disp) => disp.get(),
        }
    }
    
    fn to_unsigned(&self) -> u16 {
        use AddressDisplacement::*;
        match self {
            Zero => 0,
            Byte(disp) => disp.get() as u16,
            Word(disp) => disp.get() as u16,
        }
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
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Register::*;
        write!(f, "{}", match self {
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
        })
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
}

impl fmt::Display for SegmentRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use SegmentRegister::*;
        write!(f, "{}", match self {
            ES => "es",
            CS => "cs",
            SS => "ss",
            DS => "ds",
        })
    }
}

struct AddressCalculation {
    rm: RegDisplacement,
    disp: AddressDisplacement,
    segment_override: Option<SegmentRegister>,
}

impl fmt::Display for AddressCalculation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use AddressDisplacement::*;
        match self.disp {
            Zero => write!(f, "[{}]", self.rm),
            Byte(_) | Word(_) => write!(f, "[{} {}]", self.rm, self.disp)
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
}

impl fmt::Display for RegDisplacement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use RegDisplacement::*;

        write!(f, "{}", match self {
            BXpSI => "bx + si",
            BXpDI => "bx + di",
            BPpSI => "bp + si",
            BPpDI => "bp + di",
            SI => "si",
            DI => "di",
            BP => "bp",
            BX => "bx",
        })
    }
}

enum RegisterMemory {
    DirectAddress(u16),
    Memory(AddressCalculation),
    Register(Register),
}

impl RegisterMemory {
    // R/M is truncated to 3 bits, mode is truncated to 2 bits
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
            (0b110, 0b00) => RegisterMemory::DirectAddress(disp.to_unsigned()),
            (_, 0b00 | 0b10 | 0b01) => RegisterMemory::Memory(AddressCalculation {
                rm: RegDisplacement::new_checked(rm),
                disp,
                segment_override,
            }),
            (_, 0b11) => RegisterMemory::Register(Register::new_checked(rm, wflag)),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for RegisterMemory {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use RegisterMemory::*;

        match self {
            DirectAddress(disp) => write!(f, "[{disp}]"),
            Memory(addr_calc) => {
                match &addr_calc.segment_override {
                    Some(segment) => write!(f, "{segment}:{addr_calc}"),
                    None => write!(f, "{addr_calc}")
                }
            }
            Register(reg) => write!(f, "{}", reg.to_string())
        }
    }
}

// Gets the next byte from the instruction stream and increments i
fn get_byte(i: &mut usize, instructions: &[u8]) -> u8 {
    let inst = instructions[*i];
    *i += 1;
    inst
}

fn get_signed_byte(i: &mut usize, instructions: &[u8]) -> i8 {
    get_byte(i, instructions) as i8
}

// Gets the next word from the instruction stream and increments i
fn get_word(i: &mut usize, instructions: &[u8]) -> u16 {
    let lo_byte = get_byte(i, instructions) as u16;
    let hi_byte = get_byte(i, instructions) as u16;
    hi_byte << 8 | lo_byte
}

fn get_signed_word(i: &mut usize, instructions: &[u8]) -> i16 {
    get_word(i, instructions) as i16
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
        get_signed_word(i, instructions)
    } else {
        get_signed_byte(i, instructions) as i16
    }
}

// Returns a bool flag corresponding to the value of a specified bit
fn flag(value: u8, bit: u8) -> bool {
    ((value >> bit) & 1) != 0
}

fn opcode_cmp(value: u8, mask: u8, format: u8) -> bool {
    value & mask == format & mask
}

enum Immediate {
    Byte(i8),
    Word(i16),
}

impl Immediate {
    fn new(wflag: bool, i: &mut usize, instructions: &[u8]) -> Self {
        if wflag {
            Immediate::Word(get_signed_word(i, &instructions))
        } else {
            Immediate::Byte(get_signed_byte(i, &instructions))
        }
    }
}

impl fmt::Display for Immediate {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Immediate::*;
        match self {
            Byte(value) =>  write!(f, "byte {value}"),
            Word(value) =>  write!(f, "word {value}"),
        }
    }
}

struct MovInstruction {
    src: MoveSource,
    dest: MoveDestination
}

impl fmt::Display for MovInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "mov {}, {}", self.dest, self.src)
    }
}

enum OperatorSource {
    Immediate(Immediate),
    RM(RegisterMemory),
}

impl fmt::Display for OperatorSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use OperatorSource::*;
        match self {
            Immediate(immediate) => write!(f, "{immediate}"),
            RM(rm) => write!(f, "{rm}"),
        }
    }
}

enum MoveSource {
    Immediate(Immediate),
    RM(RegisterMemory),
    SegmentRegister(SegmentRegister)
}

impl fmt::Display for MoveSource {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use MoveSource::*;
        match self {
            Immediate(immediate) => write!(f, "{immediate}"),
            RM(rm) => write!(f, "{rm}"),
            SegmentRegister(segment) => write!(f, "{segment}"),
        }
    }
}

enum MoveDestination {
    RM(RegisterMemory),
    SegmentRegister(SegmentRegister)
}

impl fmt::Display for MoveDestination{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use MoveDestination::*;
        match self {
            RM(rm) => write!(f, "{rm}"),
            SegmentRegister(segment) => write!(f, "{segment}"),
        }
    }
}

enum Instruction {
    Mov(MovInstruction),
    Unknown,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Instruction::*;
        match self {
            Mov(args) => write!(f, "{args}"),
            Unknown => write!(f, "unknown"),
        }
    }
}

struct InstructionIterator<'a> {
    i: usize,
    instructions: &'a [u8],
    segment_override: Option<SegmentRegister>,
}

impl<'a> InstructionIterator<'a> {
    fn new(instructions: &'a [u8]) -> Self {
        Self {
            i: 0,
            instructions,
            segment_override: None
        }
    }
}

impl<'a> Iterator for InstructionIterator<'a> {
    type Item = Instruction;
    
    fn next(&mut self) -> Option<Self::Item> {
        use Instruction::*;
        
        // Early out for end of stream
        if self.i >= self.instructions.len() {
            return None;
        }
        
        // Keep mutable reference
        let i = &mut self.i;
        
        Some(loop {
            let byte1 = get_byte(i, self.instructions);

            // Mov (Register/memory to/from register)
            if opcode_cmp(byte1, 0b11111100, 0b10001000) {
                let dflag = flag(byte1, 1);
                let wflag = flag(byte1, 0);

                let byte2 = get_byte(i, self.instructions);

                let mode_bits = byte2 >> 6;
                let reg_bits = byte2 >> 3;
                let rm_bits = byte2;

                let rm = RegisterMemory::new_from_instructions(
                    rm_bits,
                    mode_bits,
                    wflag,
                    self.segment_override.take(),
                    i,
                    self.instructions,
                );

                let reg = Register::new(reg_bits, wflag);

                if dflag {
                    break Mov(MovInstruction {
                        src: MoveSource::RM(rm),
                        dest: MoveDestination::RM(RegisterMemory::Register(reg)),
                    })
                } else {
                    break Mov(MovInstruction {
                        src: MoveSource::RM(RegisterMemory::Register(reg)),
                        dest: MoveDestination::RM(rm),
                    })
                }
            }
            // Mov (Immediate to register/memory)
            else if opcode_cmp(byte1, 0b11111110, 0b11000110) {
                let wflag = flag(byte1, 0);

                let byte2 = get_byte(i, self.instructions);

                let mode_bits = byte2 >> 6;
                let rm_bits = byte2;

                let rm = RegisterMemory::new_from_instructions(
                    rm_bits,
                    mode_bits,
                    wflag,
                    self.segment_override.take(),
                    i,
                    self.instructions,
                );

                break Mov(MovInstruction {
                    src: MoveSource::Immediate(Immediate::new(wflag, i, self.instructions)),
                    dest: MoveDestination::RM(rm),
                })
            }
            // Mov (Immediate to register)
            else if opcode_cmp(byte1, 0b11110000, 0b10110000) {
                let wflag = flag(byte1, 3);
                let reg_bits = byte1;
                let reg = Register::new(reg_bits, wflag);

                break Mov(MovInstruction {
                    src: MoveSource::Immediate(Immediate::new(wflag, i, self.instructions)),
                    dest: MoveDestination::RM(RegisterMemory::Register(reg)),
                })
            }
            // Mov (Memory to accumulator / Accumulator to memory)
            else if opcode_cmp(byte1, 0b11111100, 0b10100000) {
                let dflag = flag(byte1, 1);
                let wflag = flag(byte1, 0);

                let addr = RegisterMemory::DirectAddress(get_data(wflag, i, self.instructions));

                if dflag {
                    break Mov(MovInstruction {
                        src: MoveSource::RM(RegisterMemory::Register(Register::AX)),
                        dest: MoveDestination::RM(addr),
                    })
                } else {
                    break Mov(MovInstruction {
                        src: MoveSource::RM(addr),
                        dest: MoveDestination::RM(RegisterMemory::Register(Register::AX)),
                    })
                }
            }
            // Mov (Register/memory to segment register / Segment register to register/memory)
            else if opcode_cmp(byte1, 0b11111101, 0b10001100) {
                let dflag = flag(byte1, 1);
                let wflag = true;

                let byte2 = get_byte(i, self.instructions);

                let mode_bits = byte2 >> 6;
                let sr_bits = byte2 >> 3;
                let rm_bits = byte2;

                let sr = SegmentRegister::new(sr_bits);

                let rm = RegisterMemory::new_from_instructions(
                    rm_bits,
                    mode_bits,
                    wflag,
                    self.segment_override.take(),
                    i,
                    self.instructions,
                );

                if dflag {
                    break Mov(MovInstruction {
                        src: MoveSource::RM(rm),
                        dest: MoveDestination::SegmentRegister(sr),
                    })
                } else {
                    break Mov(MovInstruction {
                        src: MoveSource::SegmentRegister(sr),
                        dest: MoveDestination::RM(rm),
                    })
                }
            }
            // Segment override prefix
            else if opcode_cmp(byte1, 0b11100111, 0b00100110) {
                let sr_bits = byte1 >> 3;
                self.segment_override.replace(SegmentRegister::new(sr_bits));
                continue
            }
            else {
                break Unknown
            }
        })
    }
}

fn main() -> std::io::Result<()> {
    let part_one_tests = Path::new("../../computer_enhance/perfaware/part1");

    // listing_0037_single_register_mov
    // listing_0038_many_register_mov
    // listing_0039_more_movs
    // listing_0040_challenge_movs
    // listing_0041_add_sub_cmp_jnz
    // listing_0042_completionist_decode
    let listing_path = part_one_tests.join(Path::new("listing_0040_challenge_movs"));

    // output
    //let test_path = Path::new("output");

    let mut file = match File::open(&listing_path) {
        Err(why) => panic!("couldn't open {}: {}", listing_path.display(), why),
        Ok(file) => file,
    };

    let mut instructions = Vec::<u8>::new();
    file.read_to_end(&mut instructions)?;

    println!("bits 16");

    for instruction in InstructionIterator::new(&instructions) {
        println!("{instruction}");
    }

    Ok(())
}
