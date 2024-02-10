use std::fmt::{self, Formatter};
use super::register_memory::*;
use super::instruction::InstructionStream;

#[derive(Clone, Copy)]
pub enum MathOp {
    Add,
    Or,
    Adc,
    Sbb,
    And,
    Sub,
    Xor,
    Cmp,
}

impl MathOp {
    pub fn new(math_op: u8) -> Self {
        use MathOp::*;
        // 3-bit value
        let math_op = math_op & 0b111;
        match math_op {
            0b000 => Add,
            0b001 => Or,
            0b010 => Adc,
            0b011 => Sbb,
            0b100 => And,
            0b101 => Sub,
            0b110 => Xor,
            0b111 => Cmp,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy)]
// An operation for reg/mem and reg operands
pub struct RegisterMemoryRegisterOp {
    pub rm: RegisterMemory,
    pub reg: Register,
}

impl RegisterMemoryRegisterOp {
    // Register/memory with register to either instruction (Mod | Reg | R/M style)
    pub fn new_reg_mem_with_reg(
        wflag: DataSize,
        segment_override: Option<SegmentRegister>,
        instructions: &mut InstructionStream,
    ) -> (RegisterMemory, Register) {
        let byte = instructions.next_byte();

        let mode_bits = byte >> 6;
        let reg_bits = byte >> 3;
        let rm_bits = byte;

        let reg = Register::new(wflag, reg_bits);
        let rm =
            RegisterMemory::new_mod_rm(wflag, mode_bits, rm_bits, segment_override, instructions);

        (rm, reg)
    }
}

impl fmt::Display for RegisterMemoryRegisterOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}, {}", self.reg, self.rm)
    }
}


// An operation from reg/mem or immediate to reg/mem
#[derive(Clone, Copy)]
pub struct RegisterMemoryImmediateOp {
    pub dest: RegisterMemory,
    pub src: ImmediateRegisterMemory,
}

impl RegisterMemoryImmediateOp {
    // Register/memory with register to either instruction (Mod | Reg | R/M style)
    pub fn new_reg_mem_with_reg(
        dflag: bool,
        wflag: DataSize,
        segment_override: Option<SegmentRegister>,
        instructions: &mut InstructionStream,
    ) -> Self {

        let (rm, reg) = RegisterMemoryRegisterOp::new_reg_mem_with_reg(wflag, segment_override, instructions);
        let reg = RegisterMemory::Register(reg);

        if dflag {
            Self {
                dest: reg,
                src: ImmediateRegisterMemory::RM(rm),
            }
        } else {
            Self {
                dest: rm,
                src: ImmediateRegisterMemory::RM(reg),
            }
        }
    }

    // From immediate to register/memory, returning the math op
    pub fn new_immediate(
        sflag: bool,
        data_size: DataSize,
        segment_override: Option<SegmentRegister>,
        instructions: &mut InstructionStream,
    ) -> (Self, MathOp) {
        let byte = instructions.next_byte();

        let mode_bits = byte >> 6;
        let op_bits = byte >> 3;
        let rm_bits = byte;

        let rm =
            RegisterMemory::new_mod_rm(data_size, mode_bits, rm_bits, segment_override, instructions);

        (
            Self {
                src: ImmediateRegisterMemory::Immediate(Immediate::new(sflag, data_size, instructions)),
                dest: rm,
            },
            MathOp::new(op_bits),
        )
    }
    
    pub fn get_data_size(&self) -> DataSize {
        self.dest.get_data_size().or(self.src.get_data_size()).expect("Either op destination or source must enforce a data size.")
    }
}

impl fmt::Display for RegisterMemoryImmediateOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}, {}", self.dest, self.src)
    }
}

#[derive(Clone, Copy)]
pub struct MoveOp {
    pub dest: RegisterMemorySegment,
    pub src: ImmediateRegisterMemorySegment,
}

impl MoveOp {
    pub fn new_segment_register(
        dflag: bool,
        segment_override: Option<SegmentRegister>,
        instructions: &mut InstructionStream,
    ) -> Self 
    {
        // Segment registers are always wide
        let wflag = DataSize::Word;

        let byte = instructions.next_byte();

        let mode_bits = byte >> 6;
        let sr_bits = byte >> 3;
        let rm_bits = byte;

        let sr = SegmentRegister::new(sr_bits);
        let rm =
            RegisterMemory::new_mod_rm(wflag, mode_bits, rm_bits, segment_override, instructions);

        if dflag {
            Self {
                src: ImmediateRegisterMemorySegment::RM(rm),
                dest: RegisterMemorySegment::SegmentRegister(sr),
            }
        } else {
            Self {
                src: ImmediateRegisterMemorySegment::SegmentRegister(sr),
                dest: RegisterMemorySegment::RM(rm),
            }
        }
    }
    
    pub fn get_data_size(&self) -> DataSize {
        self.dest.get_data_size().or(self.src.get_data_size()).expect("Either op destination or source must enforce a data size.")
    }
}

impl From<RegisterMemoryImmediateOp> for MoveOp {
    fn from(op: RegisterMemoryImmediateOp) -> Self {
        Self {
            dest: RegisterMemorySegment::RM(op.dest),
            src: op.src.into(),
        }
    }
}

impl fmt::Display for MoveOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}, {}", self.dest, self.src)
    }
}

#[derive(Clone, Copy)]
pub enum Port {
    Fixed(UnsignedData),
    Variable(DataSize),
}

#[derive(Clone, Copy)]
pub struct InPort(pub Port);

impl fmt::Display for InPort {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Port::*;
        match self.0 {
            Fixed(data) => write!(f, "{}, {data}", Register::new_accumulator(data.get_size())),
            Variable(data_size) => write!(f, "{}, {}", Register::new_accumulator(data_size), Register::new_d(DataSize::Word)),
        }
    }
}

#[derive(Clone, Copy)]
pub struct OutPort(pub Port);

impl fmt::Display for OutPort {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Port::*;
        match self.0 {
            Fixed(data) => write!(f, "{data}, {}", Register::new_accumulator(data.get_size())),
            Variable(data_size) => write!(f, "{}, {}", Register::new_d(DataSize::Word), Register::new_accumulator(data_size)),
        }
    }
}

#[derive(Clone, Copy)]
pub enum JmpOp {
    Byte(i8),
    Word(i16),
    Indirect(RegisterMemory),
    FarIndirect(RegisterMemory),
}

impl fmt::Display for JmpOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self { 
            JmpOp::Byte(inc) => write!(f, "${:+}", inc+2),
            JmpOp::Word(inc) => write!(f, "${:+}", inc+2),
            JmpOp::Indirect(rm) => write!(f, "{rm}"),
            JmpOp::FarIndirect(rm) => write!(f, "far {rm}")
        }
        
    }
}