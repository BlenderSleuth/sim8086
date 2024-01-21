use std::fmt::{self, Formatter};
use super::instruction::InstructionStream;
use super::address_displacement::AddressDisplacement;

#[derive(Clone, Copy)]
pub enum DataSize {
    Byte,
    Word,
}

pub trait Data {
    fn get_size(&self) -> DataSize;
}

#[derive(Clone, Copy)]
pub enum SignedData {
    Byte(i8),
    Word(i16),
}

impl SignedData {
    pub fn get_data(&self) -> u16 {
        match *self { 
            SignedData::Byte(data) => data as u16,
            SignedData::Word(data) => data as u16,
        } 
    }
}
impl Data for SignedData {
    fn get_size(&self) -> DataSize {
        match self {
            SignedData::Byte(_) => DataSize::Byte,
            SignedData::Word(_) => DataSize::Word
        }
    }
}
impl fmt::Display for SignedData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            SignedData::Byte(value) => write!(f, "{value}"),
            SignedData::Word(value) => write!(f, "{value}"),
        }
    }
}

#[derive(Clone, Copy)]
pub enum UnsignedData {
    Byte(u8),
    Word(u16),
}
impl Data for UnsignedData {
    fn get_size(&self) -> DataSize {
        match self {
            UnsignedData::Byte(_) => DataSize::Byte,
            UnsignedData::Word(_) => DataSize::Word
        }
    }
}
impl fmt::Display for UnsignedData {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            UnsignedData::Byte(value) => write!(f, "{value}"),
            UnsignedData::Word(value) => write!(f, "{value}"),
        }
    }
}

impl DataSize {
    pub fn from_wflag(wflag: bool) -> Self {
        if wflag {
            Self::Word
        } else {
            Self::Byte
        }
    }

    pub fn with_signed_data(&self, data: i16) -> SignedData {
        match self {
            Self::Byte => SignedData::Byte(data as i8),
            Self::Word => SignedData::Word(data)
        }
    }
    pub fn with_unsigned_data(&self, data: u16) -> UnsignedData {
        match self {
            Self::Byte => UnsignedData::Byte(data as u8),
            Self::Word => UnsignedData::Word(data)
        }
    }
}

impl SignedData {
    pub fn sign_extend(self) -> Self {
        match self {
            Self::Byte(data) => Self::Word(data as i16),
            Self::Word(_) => self
        }
    }
}

#[derive(Clone, Copy)]
pub enum RegisterType {
    Whole,
    High,
    Low,
}

#[derive(Clone, Copy)]
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
    // Gets the accumulator register
    pub fn new_accumulator(data_size: DataSize) -> Self {
        Self::new_a(data_size)
    }
    
    pub fn new_a(data_size: DataSize) -> Self {
        match data_size {
            DataSize::Byte => Register::AL,
            DataSize::Word => Register::AX,
        }
    }
    
    pub fn new_b(data_size: DataSize) -> Self {
        match data_size {
            DataSize::Byte => Register::BL,
            DataSize::Word => Register::BX,
        }
    }
    pub fn new_c(data_size: DataSize) -> Self {
        match data_size {
            DataSize::Byte => Register::CL,
            DataSize::Word => Register::CX,
        }
    }
    pub fn new_d(data_size: DataSize) -> Self {
        match data_size {
            DataSize::Byte => Register::DL,
            DataSize::Word => Register::DX,
        }
    }
    
    // REG is truncated to 3 bits
    pub fn new(data_size: DataSize, reg: u8) -> Self {
        let reg = reg & 0b111;
        Self::new_checked(data_size, reg)
    }

    pub fn new_checked(data_size: DataSize, reg: u8) -> Self {
        assert!(reg <= 0b111);
        match (reg, data_size) {
            (0b000, DataSize::Byte) => Self::AL,
            (0b000, DataSize::Word) => Self::AX,
            (0b001, DataSize::Byte) => Self::CL,
            (0b001, DataSize::Word) => Self::CX,
            (0b010, DataSize::Byte) => Self::DL,
            (0b010, DataSize::Word) => Self::DX,
            (0b011, DataSize::Byte) => Self::BL,
            (0b011, DataSize::Word) => Self::BX,
            (0b100, DataSize::Byte) => Self::AH,
            (0b100, DataSize::Word) => Self::SP,
            (0b101, DataSize::Byte) => Self::CH,
            (0b101, DataSize::Word) => Self::BP,
            (0b110, DataSize::Byte) => Self::DH,
            (0b110, DataSize::Word) => Self::SI,
            (0b111, DataSize::Byte) => Self::BH,
            (0b111, DataSize::Word) => Self::DI,
            _ => unreachable!(),
        }
    }

    pub fn to_whole(self) -> Register {
        match self {
            Register::AH | Register::AL => Register::AX,
            Register::BH | Register::BL => Register::BX,
            Register::CH | Register::CL => Register::CX,
            Register::DH | Register::DL => Register::DX,
            _ => self,
        }
    }
    
    pub fn get_index(&self) -> usize {
        match self {
            Register::AX | Register::AH | Register::AL => 0,
            Register::BX | Register::BH | Register::BL => 1,
            Register::CX | Register::CH | Register::CL => 2,
            Register::DX | Register::DH | Register::DL=> 3,
            Register::SP => 4,
            Register::BP => 5,
            Register::SI => 6,
            Register::DI => 7,
        }
    }
    
    pub fn get_type(&self) -> RegisterType {
        match self {
            Register::AX | Register::BX | Register::CX | Register::DX | 
            Register::SP | Register::BP | Register::SI | Register::DI => RegisterType::Whole,
            Register::AH | Register::BH | Register::CH | Register::DH => RegisterType::High,
            Register::AL | Register::BL | Register::CL | Register::DL => RegisterType::Low
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

#[derive(Clone, Copy)]
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
    
    pub fn get_index(&self) -> usize {
        match self { 
            Self::ES => 0,
            Self::CS => 1,
            Self::SS => 2,
            Self::DS => 3,
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

#[derive(Clone, Copy)]
pub enum AddressRegisterCalculation {
    BXpSI,
    BXpDI,
    BPpSI,
    BPpDI,
    SI,
    DI,
    BP,
    BX,
}

impl AddressRegisterCalculation {
    // R/M is truncated to 3 bits
    pub fn new(rm: u8) -> Self {
        let rm = rm & 0b111;
        Self::new_checked(rm)
    }

    fn new_checked(rm: u8) -> Self {
        use AddressRegisterCalculation::*;

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

impl fmt::Display for AddressRegisterCalculation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use AddressRegisterCalculation::*;

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

#[derive(Clone, Copy)]
pub struct AddressDisplacementCalculation {
    rm: AddressRegisterCalculation,
    displacement: AddressDisplacement,
    segment_override: Option<SegmentRegister>,
}

impl fmt::Display for AddressDisplacementCalculation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use AddressDisplacement::*;
        match self.displacement {
            Zero => write!(f, "[{}]", self.rm),
            Byte(_) | Word(_) => write!(f, "[{} {}]", self.rm, self.displacement),
        }
    }
}

#[derive(Clone, Copy)]
pub enum RegisterMemory {
    DirectAddress(u16),
    Memory(AddressDisplacementCalculation),
    Register(Register),
}

impl RegisterMemory {
    // Gets the accumulator register
    pub fn new_accumulator(data_size: DataSize) -> Self {
        RegisterMemory::Register(Register::new_accumulator(data_size))
    }

    // R/M is truncated to 3 bits, mode is truncated to 2 bits
    pub fn new(
        wflag: DataSize,
        rm: u8,
        mode: u8,
        displacement: AddressDisplacement,
        segment_override: Option<SegmentRegister>,
    ) -> Self {
        let rm = rm & 0b111;
        let mode = mode & 0b111;
        Self::new_checked(wflag, rm, mode, displacement, segment_override)
    }

    pub fn new_register(wflag: DataSize, reg: u8) -> Self {
        RegisterMemory::Register(Register::new(wflag, reg))
    }

    // R/M is truncated to 3 bits, mode is truncated to 2 bits
    pub fn new_mod_rm(
        wflag: DataSize,
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
        wflag: DataSize,
        rm: u8,
        mode: u8,
        displacement: AddressDisplacement,
        segment_override: Option<SegmentRegister>,
    ) -> Self {
        assert!(rm <= 0b111);
        assert!(mode <= 0b11);

        match (rm, mode) {
            (0b110, 0b00) => RegisterMemory::DirectAddress(displacement.to_unsigned()),
            (_, 0b00 | 0b10 | 0b01) => RegisterMemory::Memory(AddressDisplacementCalculation {
                rm: AddressRegisterCalculation::new_checked(rm),
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

#[derive(Clone, Copy)]
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

#[derive(Clone, Copy)]
pub struct Immediate(SignedData);

impl Immediate {
    pub fn new(sflag: bool, data_size: DataSize, instructions: &mut InstructionStream) -> Self {
        let signed_data = instructions.next_data_signed(data_size);

        Immediate(if sflag {
            signed_data.sign_extend()
        } else {
            signed_data
        })
    }

    pub fn get_data(&self) -> u16 {
        self.0.get_data()
    }
}

impl fmt::Display for Immediate {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.0 {
            SignedData::Byte(value) => write!(f, "byte {value}"),
            SignedData::Word(value) => write!(f, "word {value}"),
        }
    }
}

#[derive(Clone, Copy)]
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

#[derive(Clone, Copy)]
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

pub struct RegisterStorage {
    storage: [u16; 8],
    segment_storage: [u16; 4],
}

impl RegisterStorage {
    pub fn new() -> Self {
        Self {
            storage: [0; 8],
            segment_storage: [0; 4],
        }
    }
    
    pub fn read_register(&self, register: Register) -> u16 {
        let data = self.storage[register.get_index()];
        match register.get_type() {
            RegisterType::Whole => data,
            RegisterType::High => data >> 8,
            RegisterType::Low => data & 0x00ffu16,
        }
    }

    // Returns old value
    pub fn write_register(&mut self, register: Register, value: u16) -> u16 {
        let index = register.get_index();
        let data = &mut self.storage[index];
        let old_value = *data;
        
        match register.get_type() {
            RegisterType::Whole => *data = value,
            RegisterType::High => {
                *data &= 0x00ffu16;
                *data |= value << 8;
            },
            RegisterType::Low => {
                *data &= 0xff00u16;
                *data |= value & 0x00ffu16;
            },
        };
        
        old_value
    }

    pub fn read_segment_register(&self, register: SegmentRegister) -> u16 {
        self.segment_storage[register.get_index()]
    }
    
    // Returns old value
    pub fn write_segment_register(&mut self, register: SegmentRegister, value: u16) -> u16 {
        let index = register.get_index();
        let old_value = self.segment_storage[index];
        
        self.segment_storage[index] = value;
        
        old_value
    }
}