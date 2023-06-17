#![allow(dead_code)]

use std::fmt::{self, Formatter};
use std::fs::File;
use std::io::prelude::*;
use std::num::{NonZeroI16, NonZeroI8};
use std::path::Path;
use std::vec::IntoIter;

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
    fn new(wflag: bool, reg: u8) -> Self {
        let reg = reg & 0b111;
        Self::new_checked(wflag, reg)
    }

    fn new_checked(wflag: bool, reg: u8) -> Self {
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
            Byte(_) | Word(_) => write!(f, "[{} {}]", self.rm, self.disp),
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

enum RegisterMemory {
    DirectAddress(u16),
    Memory(AddressCalculation),
    Register(Register),
}

impl RegisterMemory {
    // Gets the accumulator register
    fn new_accumulator(wflag: bool) -> Self {
        RegisterMemory::Register(if wflag { Register::AX } else { Register::AL })
    }

    // R/M is truncated to 3 bits, mode is truncated to 2 bits
    fn new(
        wflag: bool,
        rm: u8,
        mode: u8,
        disp: AddressDisplacement,
        segment_override: Option<SegmentRegister>,
    ) -> Self {
        let rm = rm & 0b111;
        let mode = mode & 0b111;
        Self::new_checked(wflag, rm, mode, disp, segment_override)
    }

    fn new_register(wflag: bool, reg: u8) -> Self {
        RegisterMemory::Register(Register::new(wflag, reg))
    }

    // R/M is truncated to 3 bits, mode is truncated to 2 bits
    fn new_mod_rm(
        wflag: bool,
        mode: u8,
        rm: u8,
        segment_override: Option<SegmentRegister>,
        instructions: &mut InstructionStream,
    ) -> Self {
        let rm = rm & 0b111;
        let mode = mode & 0b111;

        let disp = match (mode, rm) {
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

        Self::new_checked(wflag, rm, mode, disp, segment_override)
    }

    fn new_checked(
        wflag: bool,
        rm: u8,
        mode: u8,
        disp: AddressDisplacement,
        segment_override: Option<SegmentRegister>,
    ) -> Self {
        assert!(rm <= 0b111);
        assert!(mode <= 0b11);

        match (rm, mode) {
            (0b110, 0b00) => RegisterMemory::DirectAddress(disp.to_unsigned()),
            (_, 0b00 | 0b10 | 0b01) => RegisterMemory::Memory(AddressCalculation {
                rm: RegDisplacement::new_checked(rm),
                disp,
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
            DirectAddress(disp) => write!(f, "[{disp}]"),
            Memory(addr_calc) => match &addr_calc.segment_override {
                Some(segment) => write!(f, "{segment}:{addr_calc}"),
                None => write!(f, "{addr_calc}"),
            },
            Register(reg) => write!(f, "{}", reg.to_string()),
        }
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
    fn new(sflag: bool, wflag: bool, instructions: &mut InstructionStream) -> Self {
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

enum MathOp {
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
    fn new(math_op: u8) -> Self {
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

// An operation from reg/mem or immediate to reg/mem
struct RegisterMemoryOp {
    dest: RegisterMemory,
    src: OperatorSource,
}

impl RegisterMemoryOp {
    // Register/memory with register to either instruction (Mod | Reg | R/M style)
    fn new_reg_mem_with_reg(
        dflag: bool,
        wflag: bool,
        segment_override: Option<SegmentRegister>,
        instructions: &mut InstructionStream,
    ) -> Self {
        let byte = instructions.next_byte();

        let mode_bits = byte >> 6;
        let reg_bits = byte >> 3;
        let rm_bits = byte;

        let reg = RegisterMemory::new_register(wflag, reg_bits);
        let rm =
            RegisterMemory::new_mod_rm(wflag, mode_bits, rm_bits, segment_override, instructions);

        if dflag {
            Self {
                dest: reg,
                src: OperatorSource::RM(rm),
            }
        } else {
            Self {
                dest: rm,
                src: OperatorSource::RM(reg),
            }
        }
    }

    // From immediate to register/memory, returning the math op
    fn new_immediate(
        sflag: bool,
        wflag: bool,
        segment_override: Option<SegmentRegister>,
        instructions: &mut InstructionStream,
    ) -> (Self, MathOp) {
        let byte = instructions.next_byte();

        let mode_bits = byte >> 6;
        let op_bits = byte >> 3;
        let rm_bits = byte;

        let rm =
            RegisterMemory::new_mod_rm(wflag, mode_bits, rm_bits, segment_override, instructions);

        (
            Self {
                src: OperatorSource::Immediate(Immediate::new(sflag, wflag, instructions)),
                dest: rm,
            },
            MathOp::new(op_bits),
        )
    }
}

impl fmt::Display for RegisterMemoryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}, {}", self.dest, self.src)
    }
}

enum ImmediateRegisterMemorySegment {
    Immediate(Immediate),
    RM(RegisterMemory),
    SegmentRegister(SegmentRegister),
}

impl From<OperatorSource> for ImmediateRegisterMemorySegment {
    fn from(src: OperatorSource) -> Self {
        use ImmediateRegisterMemorySegment::*;
        match src {
            OperatorSource::Immediate(value) => Immediate(value),
            OperatorSource::RM(rm) => RM(rm),
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

enum RegisterMemorySegment {
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

struct MoveOp {
    dest: RegisterMemorySegment,
    src: ImmediateRegisterMemorySegment,
}

impl MoveOp {
    fn new_segment_register(
        dflag: bool,
        segment_override: Option<SegmentRegister>,
        instructions: &mut InstructionStream,
    ) -> Self {
        // Segment registers are always wide
        let wflag = true;

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
}

impl From<RegisterMemoryOp> for MoveOp {
    fn from(op: RegisterMemoryOp) -> Self {
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

enum Instruction {
    // Move
    Mov(MoveOp),
    // Stack ops
    Push(RegisterMemorySegment),
    Pop(RegisterMemorySegment),
    // Math ops
    Add(RegisterMemoryOp),
    Or(RegisterMemoryOp),
    Adc(RegisterMemoryOp),
    Sbb(RegisterMemoryOp),
    And(RegisterMemoryOp),
    Sub(RegisterMemoryOp),
    Xor(RegisterMemoryOp),
    Cmp(RegisterMemoryOp),
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
    fn new_math_instruction(math_op: MathOp, rm_op: RegisterMemoryOp) -> Self {
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

struct InstructionStream {
    byte_stream: IntoIter<u8>,
}

impl InstructionStream {
    fn next_byte(&mut self) -> u8 {
        self.byte_stream.next().unwrap()
    }

    fn maybe_next_byte(&mut self) -> Option<u8> {
        self.byte_stream.next()
    }

    fn next_byte_signed(&mut self) -> i8 {
        self.next_byte() as i8
    }

    fn next_word(&mut self) -> u16 {
        let lo_byte = self.next_byte() as u16;
        let hi_byte = self.next_byte() as u16;
        hi_byte << 8 | lo_byte
    }

    fn next_word_signed(&mut self) -> i16 {
        self.next_word() as i16
    }

    fn next_data(&mut self, wflag: bool) -> u16 {
        if wflag {
            self.next_word()
        } else {
            self.next_byte() as u16
        }
    }

    fn next_data_signed(&mut self, wflag: bool) -> i16 {
        if wflag {
            self.next_word_signed()
        } else {
            self.next_byte_signed() as i16
        }
    }
}

struct InstructionIterator {
    instructions: InstructionStream,
    segment_override: Option<SegmentRegister>,
}

impl InstructionIterator {
    fn new(instructions: Vec<u8>) -> Self {
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

        let instructions = &mut self.instructions;

        Some(loop {
            let byte1 = instructions.maybe_next_byte()?;

            // Mov (Register/memory to/from register)
            if opcode_cmp(byte1, 0b11111100, 0b10001000) {
                let dflag = flag(byte1, 1);
                let wflag = flag(byte1, 0);

                let rm_op = RegisterMemoryOp::new_reg_mem_with_reg(
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
                let (rm_op, _) = RegisterMemoryOp::new_immediate(
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

                let rm_op = RegisterMemoryOp::new_reg_mem_with_reg(
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

                let (rm_op, math_op) = RegisterMemoryOp::new_immediate(
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

                let rm_op = RegisterMemoryOp {
                    dest: accumulator,
                    src: OperatorSource::Immediate(immediate),
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

fn main() -> std::io::Result<()> {
    let part_one_tests = Path::new("../../computer_enhance/perfaware/part1");

    // For testing / comparision
    // nu: cargo run | save -f output.asm; nasm output.asm; fc output ..\..\computer_enhance\perfaware\part1\listing_0040_challenge_movs

    // listing_0037_single_register_mov
    // listing_0038_many_register_mov
    // listing_0039_more_movs
    // listing_0040_challenge_movs
    // listing_0041_add_sub_cmp_jnz
    // listing_0042_completionist_decode
    let listing_path = part_one_tests.join(Path::new("listing_0042_completionist_decode"));

    // output
    let test_path = Path::new("input");

    let mut file = match File::open(&test_path) {
        Err(why) => panic!("couldn't open {}: {}", listing_path.display(), why),
        Ok(file) => file,
    };

    let mut instructions = Vec::<u8>::new();
    file.read_to_end(&mut instructions)?;

    println!("bits 16");

    for instruction in InstructionIterator::new(instructions) {
        println!("{instruction}");
    }

    Ok(())
}
