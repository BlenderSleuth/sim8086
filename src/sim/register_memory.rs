use std::fmt::{self, Formatter};
use super::instruction::InstructionStream;
use super::address_displacement::AddressDisplacement;

// REG or R/M field
pub enum Register {
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
    pub fn new(wflag: bool, reg: u8) -> Self {
        let reg = reg & 0b111;
        Self::new_checked(wflag, reg)
    }

    pub fn new_checked(wflag: bool, reg: u8) -> Self {
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
        write!(
            f,
            "{}",
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
        )
    }
}

pub enum SegmentRegister {
    ES,
    CS,
    SS,
    DS,
}

impl SegmentRegister {
    pub fn new(sr: u8) -> Self {
        let sr = sr & 0b11;
        Self::new_checked(sr)
    }

    pub fn new_checked(sr: u8) -> Self {
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
        write!(
            f,
            "{}",
            match self {
                ES => "es",
                CS => "cs",
                SS => "ss",
                DS => "ds",
            }
        )
    }
}

pub struct AddressCalculation {
    rm: RegDisplacement,
    displacement: AddressDisplacement,
    segment_override: Option<SegmentRegister>,
}

impl fmt::Display for AddressCalculation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use AddressDisplacement::*;
        match self.displacement {
            Zero => write!(f, "[{}]", self.rm),
            Byte(_) | Word(_) => write!(f, "[{} {}]", self.rm, self.displacement),
        }
    }
}

// R/M field
pub enum RegDisplacement {
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
    pub fn new(rm: u8) -> Self {
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

        write!(
            f,
            "{}",
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
        )
    }
}

pub enum RegisterMemory {
    DirectAddress(u16),
    Memory(AddressCalculation),
    Register(Register),
}

impl RegisterMemory {
    // Gets the accumulator register
    pub fn new_accumulator(wflag: bool) -> Self {
        RegisterMemory::Register(if wflag { Register::AX } else { Register::AL })
    }

    // R/M is truncated to 3 bits, mode is truncated to 2 bits
    pub fn new(
        wflag: bool,
        rm: u8,
        mode: u8,
        displacement: AddressDisplacement,
        segment_override: Option<SegmentRegister>,
    ) -> Self {
        let rm = rm & 0b111;
        let mode = mode & 0b111;
        Self::new_checked(wflag, rm, mode, displacement, segment_override)
    }

    pub fn new_register(wflag: bool, reg: u8) -> Self {
        RegisterMemory::Register(Register::new(wflag, reg))
    }

    // R/M is truncated to 3 bits, mode is truncated to 2 bits
    pub fn new_mod_rm(
        wflag: bool,
        mode: u8,
        rm: u8,
        segment_override: Option<SegmentRegister>,
        instructions: &mut InstructionStream,
    ) -> Self {
        let rm = rm & 0b111;
        let mode = mode & 0b111;

        let displacement = match (mode, rm) {
            // Byte displacement mode
            (0b01, _) => {
                // 8-bit displacement
                instructions.next_byte().into()
            }
            // Word displacement mode or Direct Address mode
            (0b10, _) | (0b00, 0b110) => {
                // 16-bit displacement
                instructions.next_word().into()
            }
            _ => AddressDisplacement::Zero,
        };

        Self::new_checked(wflag, rm, mode, displacement, segment_override)
    }

    pub fn new_checked(
        wflag: bool,
        rm: u8,
        mode: u8,
        displacement: AddressDisplacement,
        segment_override: Option<SegmentRegister>,
    ) -> Self {
        assert!(rm <= 0b111);
        assert!(mode <= 0b11);

        match (rm, mode) {
            (0b110, 0b00) => RegisterMemory::DirectAddress(displacement.to_unsigned()),
            (_, 0b00 | 0b10 | 0b01) => RegisterMemory::Memory(AddressCalculation {
                rm: RegDisplacement::new_checked(rm),
                displacement,
                segment_override,
            }),
            (_, 0b11) => RegisterMemory::new_register(wflag, rm),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for RegisterMemory {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use RegisterMemory::*;

        match self {
            DirectAddress(displacement) => write!(f, "[{displacement}]"),
            Memory(addr_calc) => match &addr_calc.segment_override {
                Some(segment) => write!(f, "{segment}:{addr_calc}"),
                None => write!(f, "{addr_calc}"),
            },
            Register(reg) => write!(f, "{}", reg.to_string()),
        }
    }
}

pub enum Immediate {
    Byte(i8),
    Word(i16),
}

impl Immediate {
    pub fn new(sflag: bool, wflag: bool, instructions: &mut InstructionStream) -> Self {
        if wflag {
            if sflag {
                // Sign extend a byte to a word
                Immediate::Word(instructions.next_byte_signed() as i16)
            } else {
                Immediate::Word(instructions.next_word_signed())
            }
        } else {
            Immediate::Byte(instructions.next_byte_signed())
        }
    }
}

impl fmt::Display for Immediate {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Immediate::*;
        match self {
            Byte(value) => write!(f, "byte {value}"),
            Word(value) => write!(f, "word {value}"),
        }
    }
}

pub enum ImmediateRegisterMemory {
    Immediate(Immediate),
    RM(RegisterMemory),
}

impl fmt::Display for ImmediateRegisterMemory {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use ImmediateRegisterMemory::*;
        match self {
            Immediate(immediate) => write!(f, "{immediate}"),
            RM(rm) => write!(f, "{rm}"),
        }
    }
}

pub enum ImmediateRegisterMemorySegment {
    Immediate(Immediate),
    RM(RegisterMemory),
    SegmentRegister(SegmentRegister),
}

impl From<ImmediateRegisterMemory> for ImmediateRegisterMemorySegment {
    fn from(src: ImmediateRegisterMemory) -> Self {
        use ImmediateRegisterMemorySegment::*;
        match src {
            ImmediateRegisterMemory::Immediate(value) => Immediate(value),
            ImmediateRegisterMemory::RM(rm) => RM(rm),
        }
    }
}

impl fmt::Display for ImmediateRegisterMemorySegment {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use ImmediateRegisterMemorySegment::*;
        match self {
            Immediate(immediate) => write!(f, "{immediate}"),
            RM(rm) => write!(f, "{rm}"),
            SegmentRegister(segment) => write!(f, "{segment}"),
        }
    }
}

pub enum RegisterMemorySegment {
    RM(RegisterMemory),
    SegmentRegister(SegmentRegister),
}

impl fmt::Display for RegisterMemorySegment {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use RegisterMemorySegment::*;
        match self {
            RM(rm) => write!(f, "{rm}"),
            SegmentRegister(segment) => write!(f, "{segment}"),
        }
    }
}
