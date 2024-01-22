use std::fmt::{self, Formatter};

use super::register_memory::{DataSize, UnsignedData, SignedData, RegisterMemorySegment, SegmentRegister, Immediate, ImmediateRegisterMemory, ImmediateRegisterMemorySegment, Register, RegisterMemory};
use super::operations::{MoveOp, RegisterMemoryRegisterOp, RegisterMemoryImmediateOp, InPort, OutPort, MathOp, Port};

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
    Sub(RegisterMemoryImmediateOp),
    Xor(RegisterMemoryImmediateOp),
    Cmp(RegisterMemoryImmediateOp),
    // Jumps
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
    Unknown,
}

impl Instruction {
    fn new_math_instruction(math_op: MathOp, rm_op: RegisterMemoryImmediateOp) -> Self {
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
            Sub(op) => write!(f, "sub {op}"),
            Xor(op) => write!(f, "xor {op}"),
            Cmp(op) => write!(f, "cmp {op}"),
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
            Unknown => write!(f, "unknown"),
        }
    }
}

pub struct InstructionStream<'a> {
    byte_stream: &'a [u8],
    ip: &'a mut usize,
}

impl<'a> InstructionStream<'a> {
    pub fn new(byte_stream: &'a [u8], ip: &'a mut usize) -> Self {
        Self { byte_stream, ip }
    }
    
    pub fn next_byte(&mut self) -> u8 {
        *self.ip += 1;
        self.byte_stream[*self.ip-1]
    }

    pub fn maybe_next_byte(&mut self) -> Option<u8> {
        *self.ip += 1;
        self.byte_stream.get(*self.ip-1).map(|x| *x)
    }

    pub fn next_byte_signed(&mut self) -> i8 {
        self.next_byte() as i8
    }
    
    pub fn next_word(&mut self) -> u16 {
        let lo_byte = self.next_byte() as u16;
        let hi_byte = self.next_byte() as u16;
        hi_byte << 8 | lo_byte
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

pub struct InstructionDecoder<'a> {
    byte_stream: &'a [u8],
    segment_override: Option<SegmentRegister>,
}

impl<'a> InstructionDecoder<'a> {
    pub fn new(byte_stream: &'a [u8]) -> Self {
        Self {
            byte_stream,
            segment_override: None,
        }
    }
    
    pub fn decode(&mut self, ip: &mut usize) -> Option<Instruction> {
        use Instruction::*;
        
        // Compares an opcode based on a subset of bits
        fn opcode_cmp(value: u8, mask: u8, format: u8) -> bool {
            value & mask == format & mask
        }

        // Returns a bool flag corresponding to the value of a specified bit
        fn flag(value: u8, bit: u8) -> bool {
            ((value >> bit) & 1) != 0
        }

        let mut instruction_stream = InstructionStream::new(self.byte_stream, ip);
        let instructions = &mut instruction_stream;
        
        Some(loop {
            let byte1 = instructions.maybe_next_byte()?;

            // Mov (Register/memory to/from register)
            if opcode_cmp(byte1, 0b11111100, 0b10001000) {
                let dflag = flag(byte1, 1);
                let wflag = DataSize::from_wflag(flag(byte1, 0));

                let rm_op = RegisterMemoryImmediateOp::new_reg_mem_with_reg(
                    dflag,
                    wflag,
                    self.segment_override.take(),
                    instructions,
                );

                break Mov(rm_op.into());
            }
            // Mov (Immediate to register/memory)
            else if opcode_cmp(byte1, 0b11111110, 0b11000110) {
                let sflag = false;
                let wflag = DataSize::from_wflag(flag(byte1, 0));

                // Mov immediate instruction has different opcode, ignore math op
                let (rm_op, _) = RegisterMemoryImmediateOp::new_immediate(
                    sflag,
                    wflag,
                    self.segment_override.take(),
                    instructions,
                );

                break Mov(rm_op.into());
            }
            // Mov (Immediate to register)
            else if opcode_cmp(byte1, 0b11110000, 0b10110000) {
                let sflag = false;
                let data_size = DataSize::from_wflag(flag(byte1, 3));
                let reg_bits = byte1;
                let reg = Register::new(data_size, reg_bits);

                break Mov(MoveOp {
                    src: ImmediateRegisterMemorySegment::Immediate(Immediate::new(sflag, data_size, instructions)),
                    dest: RegisterMemorySegment::RM(RegisterMemory::Register(reg)),
                });
            }
            // Mov (Memory to accumulator / Accumulator to memory)
            else if opcode_cmp(byte1, 0b11111100, 0b10100000) {
                let dflag = flag(byte1, 1);
                let data_size = DataSize::from_wflag(flag(byte1, 0));

                let addr = RegisterMemory::DirectAddress(instructions.next_direct_address(data_size));
                let accumulator = RegisterMemory::new_accumulator(data_size);

                break Mov(if dflag {
                    MoveOp {
                        src: ImmediateRegisterMemorySegment::RM(accumulator),
                        dest: RegisterMemorySegment::RM(addr),
                    }
                } else {
                    MoveOp {
                        src: ImmediateRegisterMemorySegment::RM(addr),
                        dest: RegisterMemorySegment::RM(accumulator),
                    }
                });
            }
            // Mov (Register/memory to segment register / Segment register to register/memory)
            else if opcode_cmp(byte1, 0b11111101, 0b10001100) {
                let dflag = flag(byte1, 1);

                break Mov(MoveOp::new_segment_register(
                    dflag,
                    self.segment_override.take(),
                    instructions,
                ));
            }
            // Segment override prefix
            else if opcode_cmp(byte1, 0b11100111, 0b00100110) {
                let sr_bits = byte1 >> 3;
                self.segment_override.replace(SegmentRegister::new(sr_bits));
                continue;
            }
            // MathOp (Register/memory to/from register)
            else if opcode_cmp(byte1, 0b11000100, 0b00000000) {
                let math_op = MathOp::new(byte1 >> 3);
                let dflag = flag(byte1, 1);
                let wflag = DataSize::from_wflag(flag(byte1, 0));

                let rm_op = RegisterMemoryImmediateOp::new_reg_mem_with_reg(
                    dflag,
                    wflag,
                    self.segment_override.take(),
                    instructions,
                );

                break Instruction::new_math_instruction(math_op, rm_op);
            }
            // MathOp (Immediate to register/memory)
            else if opcode_cmp(byte1, 0b11111100, 0b10000000) {
                let sflag = flag(byte1, 1);
                let wflag = DataSize::from_wflag(flag(byte1, 0));

                let (rm_op, math_op) = RegisterMemoryImmediateOp::new_immediate(
                    sflag,
                    wflag,
                    self.segment_override.take(),
                    instructions,
                );

                break Instruction::new_math_instruction(math_op, rm_op);
            }
            // MathOp (Immediate to accumulator)
            else if opcode_cmp(byte1, 0b11000110, 0b00000100) {
                let math_op = MathOp::new(byte1 >> 3);
                let sflag = false;
                let data_size = DataSize::from_wflag(flag(byte1, 0));

                let immediate = Immediate::new(sflag, data_size, instructions);
                let accumulator = RegisterMemory::new_accumulator(data_size);

                let rm_op = RegisterMemoryImmediateOp {
                    dest: accumulator,
                    src: ImmediateRegisterMemory::Immediate(immediate),
                };

                break Instruction::new_math_instruction(math_op, rm_op);
            }
            // Push (Register/memory)
            else if opcode_cmp(byte1, 0b11111111, 0b11111111) {
                let wflag = DataSize::Word;

                let byte2 = instructions.next_byte();

                let mode = byte2 >> 6;
                let rm = byte2;

                break Push(RegisterMemorySegment::RM(RegisterMemory::new_mod_rm(
                    wflag,
                    mode,
                    rm,
                    self.segment_override.take(),
                    instructions,
                )));
            }
            // Push (Register)
            else if opcode_cmp(byte1, 0b11111000, 0b01010000) {
                let wflag = DataSize::Word;
                let reg = Register::new(wflag, byte1);

                break Push(RegisterMemorySegment::RM(RegisterMemory::Register(reg)));
            }
            // Push (Segment Register)
            else if opcode_cmp(byte1, 0b11100111, 0b00000110) {
                let sr = SegmentRegister::new(byte1 >> 3);
                break Push(RegisterMemorySegment::SegmentRegister(sr));
            }
            // Pop (Register/memory)
            else if opcode_cmp(byte1, 0b11111111, 0b10001111) {
                let wflag = DataSize::Word;

                let byte2 = instructions.next_byte();

                let mode = byte2 >> 6;
                let rm = byte2;

                break Pop(RegisterMemorySegment::RM(RegisterMemory::new_mod_rm(
                    wflag,
                    mode,
                    rm,
                    self.segment_override.take(),
                    instructions,
                )));
            }
            // Pop (Register)
            else if opcode_cmp(byte1, 0b11111000, 0b01011000) {
                let wflag = DataSize::Word;
                let reg = Register::new(wflag, byte1);

                break Pop(RegisterMemorySegment::RM(RegisterMemory::Register(reg)));
            }
            // Pop (Segment Register)
            else if opcode_cmp(byte1, 0b11100111, 0b00000111) {
                let sr = SegmentRegister::new(byte1 >> 3);
                break Pop(RegisterMemorySegment::SegmentRegister(sr));
            }
            // Xchg (Register/memory with register)
            else if opcode_cmp(byte1, 0b11111110, 0b10000110) {
                let wflag = DataSize::from_wflag(flag(byte1, 0));

                let (rm, reg) = RegisterMemoryRegisterOp::new_reg_mem_with_reg(
                    wflag,
                    self.segment_override.take(),
                    instructions,
                );

                break Xchg(RegisterMemoryRegisterOp { rm, reg });
            }
            // Xchg (Register with accumulator)
            else if opcode_cmp(byte1, 0b11111000, 0b10010000) {
                let wflag = DataSize::Word;
                let rm = RegisterMemory::new_register(wflag, byte1);
                break Xchg(RegisterMemoryRegisterOp { rm, reg: Register::AX });
            }
            // In/out (fixed/variable port)
            else if opcode_cmp(byte1, 0b11110100, 0b11100100) {
                let data_size = DataSize::from_wflag(flag(byte1, 0));
                let out = flag(byte1, 1);
                let variable = flag(byte1, 3);

                break if out {
                    if variable {
                        Out(OutPort(Port::Variable(data_size)))
                    } else {
                        Out(OutPort(Port::Fixed(data_size.with_unsigned_data(instructions.next_byte() as u16))))
                    }
                } else {
                    if variable {
                        In(InPort(Port::Variable(data_size)))
                    } else {
                        In(InPort(Port::Fixed(data_size.with_unsigned_data(instructions.next_byte() as u16))))
                    }
                }
            }
            else if opcode_cmp(byte1, 0b11111111, 0b11010111) {
                break Xlat;
            }

            // Match jump opcodes. Increment is relative to the next instruction, two bytes later
            let increment = instructions.next_byte_signed();
            break match byte1 {
                0b01110100 => Je(increment),
                0b01111100 => Jl(increment),
                0b01111110 => Jle(increment),
                0b01110010 => Jb(increment),
                0b01110110 => Jbe(increment),
                0b01111010 => Jp(increment),
                0b01110000 => Jo(increment),
                0b01111000 => Js(increment),
                0b01110101 => Jne(increment),
                0b01111101 => Jnl(increment),
                0b01111111 => Jg(increment),
                0b01110011 => Jnb(increment),
                0b01110111 => Ja(increment),
                0b01111011 => Jnp(increment),
                0b01110001 => Jno(increment),
                0b01111001 => Jns(increment),
                0b11100010 => Loop(increment),
                0b11100001 => Loopz(increment),
                0b11100000 => Loopnz(increment),
                0b11100011 => Jcxz(increment),
                _ => Unknown,
            };
        })
    }
}

