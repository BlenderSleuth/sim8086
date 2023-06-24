use std::fmt::{self, Formatter};
use std::vec::IntoIter;

use super::register_memory::{RegisterMemorySegment, SegmentRegister, Immediate, ImmediateRegisterMemory, ImmediateRegisterMemorySegment, Register, RegisterMemory};
use super::operations::{MoveOp, RegisterMemoryRegisterOp, RegisterMemoryImmediateOp, MathOp};

pub enum Instruction {
    // Move
    Mov(MoveOp),
    // Stack ops
    Push(RegisterMemorySegment),
    Pop(RegisterMemorySegment),
    // Exchange
    Xchg(RegisterMemoryRegisterOp),
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
            Add(op) => write!(f, "add {op}"),
            Or(op) => write!(f, "or {op}"),
            Adc(op) => write!(f, "adc {op}"),
            Sbb(op) => write!(f, "sbb {op}"),
            And(op) => write!(f, "and {op}"),
            Sub(op) => write!(f, "sub {op}"),
            Xor(op) => write!(f, "xor {op}"),
            Cmp(op) => write!(f, "cmp {op}"),
            Je(inc) => write!(f, "je ${inc:+}"),
            Jl(inc) => write!(f, "jl ${inc:+}"),
            Jle(inc) => write!(f, "jle ${inc:+}"),
            Jb(inc) => write!(f, "jb ${inc:+}"),
            Jbe(inc) => write!(f, "jbe ${inc:+}"),
            Jp(inc) => write!(f, "jp ${inc:+}"),
            Jo(inc) => write!(f, "jo ${inc:+}"),
            Js(inc) => write!(f, "js ${inc:+}"),
            Jne(inc) => write!(f, "jne ${inc:+}"),
            Jnl(inc) => write!(f, "jnl ${inc:+}"),
            Jg(inc) => write!(f, "jg ${inc:+}"),
            Jnb(inc) => write!(f, "jnb ${inc:+}"),
            Ja(inc) => write!(f, "ja ${inc:+}"),
            Jnp(inc) => write!(f, "jnp ${inc:+}"),
            Jno(inc) => write!(f, "jno ${inc:+}"),
            Jns(inc) => write!(f, "jns ${inc:+}"),
            Loop(inc) => write!(f, "loop ${inc:+}"),
            Loopz(inc) => write!(f, "loopz ${inc:+}"),
            Loopnz(inc) => write!(f, "loopnz ${inc:+}"),
            Jcxz(inc) => write!(f, "jcxz ${inc:+}"),
            Unknown => write!(f, "unknown"),
        }
    }
}

pub struct InstructionStream {
    byte_stream: IntoIter<u8>,
}

impl InstructionStream {
    pub fn next_byte(&mut self) -> u8 {
        self.byte_stream.next().unwrap()
    }

    pub fn maybe_next_byte(&mut self) -> Option<u8> {
        self.byte_stream.next()
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

    pub fn next_data(&mut self, wflag: bool) -> u16 {
        if wflag {
            self.next_word()
        } else {
            self.next_byte() as u16
        }
    }

    pub fn next_data_signed(&mut self, wflag: bool) -> i16 {
        if wflag {
            self.next_word_signed()
        } else {
            self.next_byte_signed() as i16
        }
    }
}

pub struct InstructionIterator {
    instructions: InstructionStream,
    segment_override: Option<SegmentRegister>,
}

impl InstructionIterator {
    pub fn new(instructions: Vec<u8>) -> Self {
        let instruction_bytes = InstructionStream {
            byte_stream: instructions.into_iter(),
        };
        Self {
            instructions: instruction_bytes,
            segment_override: None,
        }
    }
}


impl Iterator for InstructionIterator {
    type Item = Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        use Instruction::*;

        // Compares an opcode based on a subset of bits
        fn opcode_cmp(value: u8, mask: u8, format: u8) -> bool {
            value & mask == format & mask
        }

        // Returns a bool flag corresponding to the value of a specified bit
        fn flag(value: u8, bit: u8) -> bool {
            ((value >> bit) & 1) != 0
        }
        
        let instructions = &mut self.instructions;

        Some(loop {
            let byte1 = instructions.maybe_next_byte()?;

            // Mov (Register/memory to/from register)
            if opcode_cmp(byte1, 0b11111100, 0b10001000) {
                let dflag = flag(byte1, 1);
                let wflag = flag(byte1, 0);

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
                let wflag = flag(byte1, 0);

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
                let wflag = flag(byte1, 3);
                let reg_bits = byte1;
                let reg = Register::new(wflag, reg_bits);

                break Mov(MoveOp {
                    src: ImmediateRegisterMemorySegment::Immediate(Immediate::new(sflag, wflag, instructions)),
                    dest: RegisterMemorySegment::RM(RegisterMemory::Register(reg)),
                });
            }
            // Mov (Memory to accumulator / Accumulator to memory)
            else if opcode_cmp(byte1, 0b11111100, 0b10100000) {
                let dflag = flag(byte1, 1);
                let wflag = flag(byte1, 0);

                let addr = RegisterMemory::DirectAddress(instructions.next_data(wflag));
                let accumulator = RegisterMemory::new_accumulator(wflag);

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
                let wflag = flag(byte1, 0);

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
                let wflag = flag(byte1, 0);

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
                let wflag = flag(byte1, 0);

                let immediate = Immediate::new(sflag, wflag, instructions);
                let accumulator = RegisterMemory::new_accumulator(wflag);

                let rm_op = RegisterMemoryImmediateOp {
                    dest: accumulator,
                    src: ImmediateRegisterMemory::Immediate(immediate),
                };

                break Instruction::new_math_instruction(math_op, rm_op);
            }
            // Push (Register/memory)
            else if opcode_cmp(byte1, 0b11111111, 0b11111111) {
                let wflag = true;

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
                let wflag = true;
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
                let wflag = true;

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
                let wflag = true;
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
                let wflag = flag(byte1, 0);

                let (rm, reg) = RegisterMemoryRegisterOp::new_reg_mem_with_reg(
                    wflag,
                    self.segment_override.take(),
                    instructions,
                );

                break Xchg(RegisterMemoryRegisterOp { rm, reg });
            }
            // Xchg (Register with accumulator)
            else if opcode_cmp(byte1, 0b11111000, 0b10010000) {
                let wflag = true;
                let rm = RegisterMemory::new_register(wflag, byte1);
                break Xchg(RegisterMemoryRegisterOp { rm, reg: Register::AX });
            }

            // Match jump opcodes. Increment is relative to the next instruction, two bytes later
            let increment = instructions.next_byte_signed() + 2;
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
