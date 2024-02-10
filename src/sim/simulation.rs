use std::fmt::{self, Formatter};
use std::fs::File;
use std::io::Write;
use std::path::Path;

use super::operations::{InPort, JmpOp, MathOp, MoveOp, OutPort, Port, RegisterMemoryImmediateOp, RegisterMemoryRegisterOp};
use super::utils;
use super::instruction::{HALT_OPCODE, Instruction, InstructionStream};
use super::register_memory::*;

pub enum SimulateErr {
    UnknownInstruction,
    Halt,
}

pub enum InterruptErr {
    Halt,
    Unhandled,
}

pub trait SimulatorDelegate {
    fn simulate(&mut self, simulator: &mut Simulator) -> Result<(), SimulateErr>;
    fn interrupt_handler(&mut self, simulator: &mut Simulator, code: u8) -> Result<(), InterruptErr>;
}

pub struct DefaultDelegate;
impl SimulatorDelegate for DefaultDelegate {
    fn simulate(&mut self, _: &mut Simulator) -> Result<(), SimulateErr>  { Ok(()) }

    fn interrupt_handler(&mut self, _: &mut Simulator, _: u8) -> Result<(), InterruptErr>  { Ok(()) }
}

pub struct Simulator {
    pub register_storage: RegisterStorage,
    memory_storage: Box<[u8]>,
    segment_override: Option<SegmentRegister>,
    program_size: usize,
    ip: usize,
    zero_flag: bool,
    parity_flag: bool,
    sign_flag: bool,
}

impl Simulator {
    pub fn new(memory_size: usize, instruction_start: usize, instructions: &[u8]) -> Self {
        let mut simulator = Self {
            register_storage: RegisterStorage::new(),
            memory_storage: vec![0u8; memory_size].into_boxed_slice(),
            segment_override: None,
            program_size: instructions.len(),
            ip: instruction_start,
            zero_flag: false,
            parity_flag: false,
            sign_flag: false,
        };

        // Copies instructions into simulation memory
        simulator.memory_storage[..instructions.len()].clone_from_slice(instructions);
        // Append halt code for safety
        simulator.memory_storage[instructions.len()] = HALT_OPCODE;
        simulator
    }
    
    pub fn decode(&mut self) -> Option<Instruction> {
        use Instruction::*;

        // Compares an opcode based on a subset of bits
        fn opcode_cmp(value: u8, mask: u8, format: u8) -> bool {
            value & mask == format & mask
        }

        // Returns a bool flag corresponding to the value of a specified bit
        fn flag(value: u8, bit: u8) -> bool {
            ((value >> bit) & 1) != 0
        }

        let mut instruction_stream = InstructionStream::new(&self.memory_storage[..self.program_size], &mut self.ip);
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
            // Inc (register)
            else if opcode_cmp(byte1, 0b11111000, 0b01000000) {
                break Inc(RegisterMemory::Register(Register::new(DataSize::Word, byte1))) 
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
            
            // Match whole byte
            match byte1 {
                // Increment/Decrement byte (rm)
                0b11111110 => {
                    let wflag = DataSize::Byte;

                    let byte2 = instructions.next_byte();

                    let mode = byte2 >> 6;
                    let op = (byte2 >> 3) & 0b111;
                    let rm = byte2;
                    
                    match op {
                        0b000 => break Inc(RegisterMemory::new_mod_rm(
                            wflag,
                            mode,
                            rm,
                            self.segment_override.take(),
                            instructions,
                        )),
                        0b001 => break Dec(RegisterMemory::new_mod_rm(
                            wflag,
                            mode,
                            rm,
                            self.segment_override.take(),
                            instructions,
                        )),
                        _ => {}
                    }
                }
                0b11111111 => {
                    let wflag = DataSize::Word;
                    
                    let byte2 = instructions.next_byte();
                    
                    let mode = byte2 >> 6;
                    let op = (byte2 >> 3) & 0b111;
                    let rm = byte2;
                    
                    match op {
                        // Increment word (rm)
                        0b000 => {
                            break Inc(RegisterMemory::new_mod_rm(
                                wflag,
                                mode,
                                rm,
                                self.segment_override.take(),
                                instructions,
                            ));
                        }
                        0b001 => break Dec(RegisterMemory::new_mod_rm(
                            wflag,
                            mode,
                            rm,
                            self.segment_override.take(),
                            instructions,
                        )),
                        // Push (Register/memory)
                        0b110 => {
                            break Push(RegisterMemorySegment::RM(RegisterMemory::new_mod_rm(
                                wflag,
                                mode,
                                rm,
                                self.segment_override.take(),
                                instructions,
                            )));
                        }
                        // JMP Indirect (within segment)
                        0b100 => {
                            break Jmp(JmpOp::Indirect(RegisterMemory::new_mod_rm(wflag, mode, rm, None, instructions)))
                        }
                        // JMP Indirect (inter-segment)
                        0b101 => {
                            break Jmp(JmpOp::FarIndirect(RegisterMemory::new_mod_rm(wflag, mode, rm, None, instructions)))
                        }
                        _ => {}
                    }
                }
                // Xlat
                0b11010111 => break Xlat,
                // Interrupt
                0b11001101 => {
                    let interrupt = instructions.next_byte();
                    break Interrupt(interrupt)
                }
                // Interrupt type 3
                0b11001100 => {
                    break Interrupt(3)
                }
                // JMP (word)
                0b11101001 => {
                    // 16 bit increment
                    let increment = instructions.next_word_signed();
                    break Jmp(JmpOp::Word(increment))
                }
                // JMP (byte)
                0b11101011 => {
                    // 8-bit increment
                    let increment = instructions.next_byte_signed();
                    break Jmp(JmpOp::Byte(increment))
                }
                // Halt code
                HALT_OPCODE => break Halt,
                _ => {}
            }
            
            // Match jump opcodes. Increment is relative to the next instruction, two bytes later
            let increment = instructions.next_byte_signed();
            match byte1 {
                0b01110100 => break Je(increment),
                0b01111100 => break Jl(increment),
                0b01111110 => break Jle(increment),
                0b01110010 => break Jb(increment),
                0b01110110 => break Jbe(increment),
                0b01111010 => break Jp(increment),
                0b01110000 => break Jo(increment),
                0b01111000 => break Js(increment),
                0b01110101 => break Jne(increment),
                0b01111101 => break Jnl(increment),
                0b01111111 => break Jg(increment),
                0b01110011 => break Jnb(increment),
                0b01110111 => break Ja(increment),
                0b01111011 => break Jnp(increment),
                0b01110001 => break Jno(increment),
                0b01111001 => break Jns(increment),
                0b11100010 => break Loop(increment),
                0b11100001 => break Loopz(increment),
                0b11100000 => break Loopnz(increment),
                0b11100011 => break Jcxz(increment),
                _ => {},
            };
            
            break Unknown;
        })
    }
    
    pub fn simulate<F: FnMut(&mut Self, u8) -> Result<(), InterruptErr>>(&mut self, instruction: &Instruction, mut interrupt_handler: F) -> Result<String, SimulateErr> {
        use Instruction::*;
        
        Ok(match *instruction {
            Mov(move_op) => {
                let data_size = move_op.get_data_size();
                
                let new_value = match move_op.src {
                    ImmediateRegisterMemorySegment::Immediate(immediate) => immediate.get_data(),
                    ImmediateRegisterMemorySegment::RM(rm) => self.read_rm(rm, data_size),
                    ImmediateRegisterMemorySegment::SegmentRegister(segment) => 
                        self.register_storage.read_segment_register(segment),
                };
                
                match move_op.dest {
                    RegisterMemorySegment::RM(rm) => {
                        let old_value = self.write_rm(rm, new_value, data_size);
                        let new_value = self.read_rm(rm, DataSize::Word);
                        format!("{rm}:{:#0x}->{:#0x}", old_value, new_value)
                    },
                    RegisterMemorySegment::SegmentRegister(segment) => {
                        let old_value = self.register_storage.write_segment_register(segment, new_value);
                        format!("{segment}:{:#0x}->{:#0x}", old_value, new_value)
                    }
                }
            }
            Add(add_op) => {
                let (arg0, arg1, data_size) = self.get_args(add_op);
                let (result, _overflow) = arg0.overflowing_add(arg1);

                self.write_rm(add_op.dest, result, data_size);
                let new_value = self.read_rm(add_op.dest, DataSize::Word);

                format!("{}:{arg0:#0x}->{new_value:#0x} {}", add_op.dest, self.set_flags(result))
            }
            Sub(sub_op) => {
                let (arg0, arg1, data_size) = self.get_args(sub_op);
                let (result, _overflow) = arg0.overflowing_sub(arg1);
                
                self.write_rm(sub_op.dest, result, data_size);
                let new_value = self.read_rm(sub_op.dest, DataSize::Word);
                
                format!("{}:{arg0:#0x}->{new_value:#0x} {}", sub_op.dest, self.set_flags(result))
            }
            Cmp(cmp_op) => {
                let (arg0, arg1, _) = self.get_args(cmp_op);
                let (result, _overflow) = arg0.overflowing_sub(arg1);
                self.set_flags(result)
            }
            Jmp(op) => {
                let increment = match op {
                    JmpOp::Byte(data) => data as i16,
                    JmpOp::Word(data) => data,
                    JmpOp::Indirect(rm) | JmpOp::FarIndirect(rm) => self.read_rm(rm, DataSize::Word) as i16,
                };
                self.jump_word(increment);
                String::new()
            }
            Jne(increment) => {
                if !self.zero_flag {
                    self.jump(increment);
                };
                String::new()
            }
            Je(increment) => {
                if self.zero_flag {
                    self.jump(increment);
                };
                String::new()
            }
            Loop(increment) => {
                let count = self.register_storage.read_register(Register::CX) - 1;
                self.register_storage.write_register(Register::CX, count);
                
                if count != 0 {
                    self.jump(increment);
                };
                String::new()
            }
            Interrupt(code) => {
                // Interrupts are handled outside simulator
                if let Err(err) = interrupt_handler(self, code) {
                    match err {
                        InterruptErr::Halt => return Err(SimulateErr::Halt),
                        InterruptErr::Unhandled => panic!("Unhandled interrupt")
                    }
                } else {
                    String::new()
                }
            }
            Halt => return Err(SimulateErr::Halt),
            _ => return Err(SimulateErr::UnknownInstruction),
        })
    }
    
    pub fn get_memory(&self) -> &[u8] {
        return &self.memory_storage
    }

    pub fn get_memory_mut(&mut self) -> &mut [u8] {
        return &mut self.memory_storage
    }
    
    pub fn get_program_size(&self) -> usize { self.program_size }
    
    pub fn get_ip(&self) -> usize { self.ip }
    
    fn get_args(&self, op: RegisterMemoryImmediateOp) -> (u16, u16, DataSize) {
        let data_size = op.get_data_size();
        (self.read_rm(op.dest, data_size), self.read_irm(op.src, data_size), data_size)
    }
    
    fn address_calculation(&self, calc: AddressDisplacementCalculation) -> usize {
        let disp = calc.displacement.to_signed();
        
        match calc.rm {
            AddressRegisterCalculation::BXpSI =>
                self.register_storage.read_register(Register::BX) +
                    self.register_storage.read_register(Register::SI),
            AddressRegisterCalculation::BXpDI =>
                self.register_storage.read_register(Register::BX) +
                    self.register_storage.read_register(Register::DI),
            AddressRegisterCalculation::BPpSI =>
                self.register_storage.read_register(Register::BP) +
                    self.register_storage.read_register(Register::SI),
            AddressRegisterCalculation::BPpDI =>
                self.register_storage.read_register(Register::BP) +
                    self.register_storage.read_register(Register::DI),
            AddressRegisterCalculation::SI => self.register_storage.read_register(Register::SI),
            AddressRegisterCalculation::DI => self.register_storage.read_register(Register::DI),
            AddressRegisterCalculation::BP => self.register_storage.read_register(Register::BP),
            AddressRegisterCalculation::BX => self.register_storage.read_register(Register::BX),
        }.checked_add_signed(disp).unwrap() as usize
    }
    
    // Reads a word from memory (little endian)
    pub fn read_word_mem(&self, addr: usize) -> u16 {
        utils::combine_u8s_to_u16(self.memory_storage[addr+1], self.memory_storage[addr])
    }

    pub fn read_mem(&self, addr: usize, dest_reg_size: DataSize) -> u16 {
        match dest_reg_size {
            DataSize::Byte => self.memory_storage[addr] as u16,
            DataSize::Word => self.read_word_mem(addr)
        }
    }
    
    fn read_irm(&self, irm: ImmediateRegisterMemory, dest_reg_size: DataSize) -> u16 {
        match irm {
            ImmediateRegisterMemory::Immediate(imm) => {
                imm.get_data()
            }
            ImmediateRegisterMemory::RM(rm) => self.read_rm(rm, dest_reg_size),
        }
    }
    
    fn read_rm(&self, rm: RegisterMemory, dest_reg_size: DataSize) -> u16 {
        match rm {
            RegisterMemory::DirectAddress(addr) => {
                self.read_mem(addr as usize, dest_reg_size)
            }
            RegisterMemory::Memory(mem) => 
                self.read_mem(self.address_calculation(mem), dest_reg_size),
            RegisterMemory::Register(reg) => {
                let to_word = dest_reg_size == DataSize::Word;
                self.register_storage.read_register(if to_word { reg.to_whole() } else { reg })
            }
        }
    }

    // Writes a word to memory (little endian)
    pub fn write_word_mem(&mut self, addr: usize, value: u16) { 
        (self.memory_storage[addr+1], self.memory_storage[addr]) = utils::split_u16_to_u8s(value);
    }

    pub fn write_byte_mem(&mut self, addr: usize, value: u8) {
        self.memory_storage[addr] = value;
    }

    pub fn write_mem(&mut self, addr: usize, value: u16, data_size: DataSize) {
        match data_size {
            DataSize::Byte => self.write_byte_mem(addr, value as u8),
            DataSize::Word => self.write_word_mem(addr, value),
        }
    }
    
    fn write_rm(&mut self, rm: RegisterMemory, value: u16, data_size: DataSize) -> u16 {
        let old_value = self.read_rm(rm, data_size);
        
        match rm {
            RegisterMemory::DirectAddress(addr) => {
                self.write_mem(addr as usize, value, data_size);
            }
            RegisterMemory::Memory(mem) =>
                self.write_mem(self.address_calculation(mem), value, data_size),
            RegisterMemory::Register(reg) => {
                self.register_storage.write_register(reg, value);
            }
        }

        old_value
    }

    fn jump_word(&mut self, increment: i16) {
        self.ip = self.ip.checked_add_signed(increment.into()).expect("Jumped outside instruction stream.");
    }
    
    fn jump(&mut self, increment: i8) {
        self.jump_word(increment as i16);
    }
    
    fn print_flags(&self) -> String {
        format!("{}{}{}",
                if self.zero_flag   { "Z" } else { "" },
                if self.parity_flag { "P" } else { "" },
                if self.sign_flag   { "S" } else { "" },
        )
    }
    
    fn set_flags(&mut self, result: u16) -> String {
        let old_flags = self.print_flags();
        
        self.zero_flag = result == 0;
        self.parity_flag = ((result as u8).count_ones() & 1) == 0;
        self.sign_flag = (result & 0x8000u16) != 0; // check sign bit
        
        let new_flags = self.print_flags();
        
        if old_flags.len() > 0 || new_flags.len() > 0 {
            format!("flags:{old_flags}->{new_flags}")
        } else { 
            String::new() 
        }
    }
    
    pub fn dump(&self, filename: &str) -> std::io::Result<()> {
        let dump_path = Path::new(filename);
        let mut dumpfile = match File::create(dump_path) {
            Err(why) => panic!("couldn't open {}: {}", dump_path.display(), why),
            Ok(file) => file,
        };
        dumpfile.write_all(self.get_memory())
    }
}

impl<'a> fmt::Display for Simulator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let ax = self.register_storage.read_register(Register::AX);
        let bx = self.register_storage.read_register(Register::BX);
        let cx = self.register_storage.read_register(Register::CX);
        let dx = self.register_storage.read_register(Register::DX);
        
        let sp = self.register_storage.read_register(Register::SP);
        let bp = self.register_storage.read_register(Register::BP);
        let si = self.register_storage.read_register(Register::SI);
        let di = self.register_storage.read_register(Register::DI);

        let es = self.register_storage.read_segment_register(SegmentRegister::ES);
        let cs = self.register_storage.read_segment_register(SegmentRegister::CS);
        let ss = self.register_storage.read_segment_register(SegmentRegister::SS);
        let ds = self.register_storage.read_segment_register(SegmentRegister::DS);
        
        let ip = self.ip;
        
        let flags = self.print_flags();

        if ax > 0 { writeln!(f, "      ax: {ax:#06x} ({ax})")?; }
        if bx > 0 { writeln!(f, "      bx: {bx:#06x} ({bx})")?; }
        if cx > 0 { writeln!(f, "      cx: {cx:#06x} ({cx})")?; }
        if dx > 0 { writeln!(f, "      dx: {dx:#06x} ({dx})")?; }
        
        if sp > 0 { writeln!(f, "      sp: {sp:#06x} ({sp})")?; }
        if bp > 0 { writeln!(f, "      bp: {bp:#06x} ({bp})")?; }
        if si > 0 { writeln!(f, "      si: {si:#06x} ({si})")?; }
        if di > 0 { writeln!(f, "      di: {di:#06x} ({di})")?; }

        if es > 0 { writeln!(f, "      es: {es:#06x} ({es})")?; }
        if cs > 0 { writeln!(f, "      cs: {cs:#06x} ({cs})")?; }
        if ss > 0 { writeln!(f, "      ss: {ss:#06x} ({ss})")?; }
        if ds > 0 { writeln!(f, "      ds: {ds:#06x} ({ds})")?; }

        writeln!(f, "      ip: {ip:#06x} ({ip})")?;
        
        if flags.len() > 0 { writeln!(f, "   flags: {flags}")?; }
        
        Ok(())
    }
}