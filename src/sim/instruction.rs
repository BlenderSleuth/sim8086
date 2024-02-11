use std::fmt::{self, Formatter};

use super::utils;
use super::register_memory::{DataSize, UnsignedData, SignedData, RegisterMemorySegment, RegisterMemory};
use super::operations::{MoveOp, RegisterMemoryRegisterOp, RegisterMemoryImmediateOp, InPort, OutPort, MathOp, JmpOp};

pub const HALT_OPCODE: u8 = 0b11110100;

pub enum Instruction {
    // Move
    Mov(MoveOp),
    // Stack ops
    Push(RegisterMemorySegment),
    Pop(RegisterMemorySegment),
    // Exchange
    Xchg(RegisterMemoryRegisterOp),
    // I/O
    In(InPort),
    Out(OutPort),
    // Xlat
    Xlat,
    // Math ops
    Add(RegisterMemoryImmediateOp),
    Or(RegisterMemoryImmediateOp),
    Adc(RegisterMemoryImmediateOp),
    Sbb(RegisterMemoryImmediateOp),
    And(RegisterMemoryImmediateOp),
    Test(RegisterMemoryImmediateOp),
    Sub(RegisterMemoryImmediateOp),
    Xor(RegisterMemoryImmediateOp),
    Cmp(RegisterMemoryImmediateOp),
    Inc(RegisterMemory),
    Dec(RegisterMemory),
    // Call/Return
    Ret,
    // Jumps
    Jmp(JmpOp),
    Je(i8),
    Jl(i8),
    Jle(i8),
    Jb(i8),
    Jbe(i8),
    Jp(i8),
    Jo(i8),
    Js(i8),
    Jne(i8),
    Jnl(i8),
    Jg(i8),
    Jnb(i8),
    Ja(i8),
    Jnp(i8),
    Jno(i8),
    Jns(i8),
    Loop(i8),
    Loopz(i8),
    Loopnz(i8),
    Jcxz(i8),
    Interrupt(u8),
    Halt,
    Unknown,
}

impl Instruction {
    pub fn new_math_instruction(math_op: MathOp, rm_op: RegisterMemoryImmediateOp) -> Self {
        use Instruction::*;
        match math_op {
            MathOp::Add => Add(rm_op),
            MathOp::Or => Or(rm_op),
            MathOp::Adc => Adc(rm_op),
            MathOp::Sbb => Sbb(rm_op),
            MathOp::And => And(rm_op),
            MathOp::Sub => Sub(rm_op),
            MathOp::Xor => Xor(rm_op),
            MathOp::Cmp => Cmp(rm_op),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Instruction::*;
        match self {
            Mov(op) => write!(f, "mov {op}"),
            // Todo(ben): conditionally use 'word' for rm
            Push(op) => write!(f, "push word {op}"),
            Pop(op) => write!(f, "pop word {op}"),
            Xchg(op) => write!(f, "xchg {op}"),
            In(op) => write!(f, "in {op}"),
            Out(op) => write!(f, "out {op}"),
            Xlat => write!(f, "xlat"),
            Add(op) => write!(f, "add {op}"),
            Or(op) => write!(f, "or {op}"),
            Adc(op) => write!(f, "adc {op}"),
            Sbb(op) => write!(f, "sbb {op}"),
            And(op) => write!(f, "and {op}"),
            Test(op) => write!(f, "test {op}"),
            Sub(op) => write!(f, "sub {op}"),
            Xor(op) => write!(f, "xor {op}"),
            Cmp(op) => write!(f, "cmp {op}"),
            Inc(op) => write!(f, "inc {op}"),
            Dec(op) => write!(f, "dec {op}"),
            Ret => write!(f, "ret"),
            Jmp(op) => write!(f, "jmp {op}"),
            Je(inc) => write!(f, "je ${:+}", inc+2),
            Jl(inc) => write!(f, "jl ${:+}", inc+2),
            Jle(inc) => write!(f, "jle ${:+}", inc+2),
            Jb(inc) => write!(f, "jb ${:+}", inc+2),
            Jbe(inc) => write!(f, "jbe ${:+}", inc+2),
            Jp(inc) => write!(f, "jp ${:+}", inc+2),
            Jo(inc) => write!(f, "jo ${:+}", inc+2),
            Js(inc) => write!(f, "js ${:+}", inc+2),
            Jne(inc) => write!(f, "jne ${:+}", inc+2),
            Jnl(inc) => write!(f, "jnl ${:+}", inc+2),
            Jg(inc) => write!(f, "jg ${:+}", inc+2),
            Jnb(inc) => write!(f, "jnb ${:+}", inc+2),
            Ja(inc) => write!(f, "ja ${:+}", inc+2),
            Jnp(inc) => write!(f, "jnp ${:+}", inc+2),
            Jno(inc) => write!(f, "jno ${:+}", inc+2),
            Jns(inc) => write!(f, "jns ${:+}", inc+2),
            Loop(inc) => write!(f, "loop ${:+}", inc+2),
            Loopz(inc) => write!(f, "loopz ${:+}", inc+2),
            Loopnz(inc) => write!(f, "loopnz ${:+}", inc+2),
            Jcxz(inc) => write!(f, "jcxz ${:+}", inc+2),
            Interrupt(int_type) => write!(f, "int {:0x}h", int_type),
            Halt => write!(f, "hlt"),
            Unknown => write!(f, "unknown"),
        }
    }
}

pub struct InstructionStream<'a> {
    memory: &'a [u8],
    ip: &'a mut usize,
}

impl<'a> InstructionStream<'a> {
    pub fn new(memory: &'a [u8], ip: &'a mut usize) -> Self {
        Self { memory, ip }
    }
    
    pub fn next_byte(&mut self) -> u8 {
        *self.ip += 1;
        self.memory[*self.ip-1]
    }

    pub fn maybe_next_byte(&mut self) -> Option<u8> {
        *self.ip += 1;
        self.memory.get(*self.ip-1).map(|x| *x)
    }

    pub fn next_byte_signed(&mut self) -> i8 {
        self.next_byte() as i8
    }
    
    pub fn next_word(&mut self) -> u16 {
        let lo = self.next_byte();
        let hi = self.next_byte();
        utils::combine_u8s_to_u16(hi, lo)
    }

    pub fn next_word_signed(&mut self) -> i16 {
        self.next_word() as i16
    }

    pub fn next_direct_address(&mut self, data_size: DataSize) -> u16 {
        match data_size {
            DataSize::Byte => self.next_byte() as u16,
            DataSize::Word => self.next_word(),
        }
    }

    pub fn next_data_unsigned(&mut self, data_size: DataSize) -> UnsignedData {
        match data_size {
            DataSize::Byte => UnsignedData::Byte(self.next_byte()),
            DataSize::Word => UnsignedData::Word(self.next_word()),
        }
    }
    
    pub fn next_data_signed(&mut self, data_size: DataSize) -> SignedData {
        match data_size {
            DataSize::Byte => SignedData::Byte(self.next_byte_signed()),
            DataSize::Word => SignedData::Word(self.next_word_signed()),
        }
    }
}
