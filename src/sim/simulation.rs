use std::fmt::{self, Formatter};

use super::operations::RegisterMemoryImmediateOp;
use super::utils;
use super::instruction::Instruction;
use super::register_memory::*;

pub struct Simulator {
    register_storage: RegisterStorage,
    memory_storage: [u8; 65536],
    cached_ip: usize,
    zero_flag: bool,
    parity_flag: bool,
    sign_flag: bool,
}

impl Simulator {
    pub fn new() -> Self {
        Self {
            register_storage: RegisterStorage::new(),
            memory_storage: [0; 65536],
            cached_ip: 0,
            zero_flag: false,
            parity_flag: false,
            sign_flag: false,
        }
    }

    pub fn simulate(&mut self, instruction: Instruction, ip: &mut usize) -> String {
        self.cached_ip = *ip;
        
        match instruction {
            Instruction::Mov(move_op) => {
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
            Instruction::Add(add_op) => {
                let (arg0, arg1, data_size) = self.get_args(add_op);
                let result = arg0 + arg1;

                self.write_rm(add_op.dest, result, data_size);
                let new_value = self.read_rm(add_op.dest, DataSize::Word);

                format!("{}:{arg0:#0x}->{new_value:#0x} {}", add_op.dest, self.set_flags(result))
            }
            Instruction::Sub(sub_op) => {
                let (arg0, arg1, data_size) = self.get_args(sub_op);
                let (result, _overflow) = arg0.overflowing_sub(arg1);
                
                self.write_rm(sub_op.dest, result, data_size);
                let new_value = self.read_rm(sub_op.dest, DataSize::Word);
                
                format!("{}:{arg0:#0x}->{new_value:#0x} {}", sub_op.dest, self.set_flags(result))
            }
            Instruction::Cmp(cmp_op) => {
                let (arg0, arg1, _) = self.get_args(cmp_op);
                let (result, _overflow) = arg0.overflowing_sub(arg1);
                self.set_flags(result)
            }
            Instruction::Jne(increment) => {
                if !self.zero_flag {
                    Simulator::jump(ip, increment);
                };
                String::new()
            },
            _ => panic!("Cannot simulate instruction: {instruction}"),
        }
    }
    
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
    fn read_word_mem(&self, addr: usize) -> u16 {
        utils::combine_u8s_to_u16(self.memory_storage[addr+1], self.memory_storage[addr])
    }
    
    fn read_mem(&self, addr: usize, dest_reg_size: DataSize) -> u16 {
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
    fn write_word_mem(&mut self, addr: usize, value: u16) { 
        (self.memory_storage[addr+1], self.memory_storage[addr]) = utils::split_u16_to_u8s(value);
    }
    
    fn write_mem(&mut self, addr: usize, value: u16, data_size: DataSize) {
        match data_size {
            DataSize::Byte => self.memory_storage[addr] = value as u8,
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

    fn jump(ip: &mut usize, increment: i8) {
        *ip = ip.checked_add_signed(increment.into()).expect("Jumped outside instruction stream.");
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
        self.sign_flag = (result & 0x8000u16) != 0;
        
        let new_flags = self.print_flags();
        
        if old_flags.len() > 0 || new_flags.len() > 0 {
            format!("flags:{old_flags}->{new_flags}")
        } else { 
            String::new() 
        }
    }
}

impl fmt::Display for Simulator {
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
        let ss = self.register_storage.read_segment_register(SegmentRegister::SS);
        let ds = self.register_storage.read_segment_register(SegmentRegister::DS);
        
        let ip = self.cached_ip;
        
        let flags = self.print_flags();
        
        if bx > 0 { writeln!(f, "      bx: {bx:#06x} ({bx})")?; }
        if ax > 0 { writeln!(f, "      ax: {ax:#06x} ({ax})")?; }
        if cx > 0 { writeln!(f, "      cx: {cx:#06x} ({cx})")?; }
        if dx > 0 { writeln!(f, "      dx: {dx:#06x} ({dx})")?; }
        
        if sp > 0 { writeln!(f, "      sp: {sp:#06x} ({sp})")?; }
        if bp > 0 { writeln!(f, "      bp: {bp:#06x} ({bp})")?; }
        if si > 0 { writeln!(f, "      si: {si:#06x} ({si})")?; }
        if di > 0 { writeln!(f, "      di: {di:#06x} ({di})")?; }

        if es > 0 { writeln!(f, "      es: {es:#06x} ({es})")?; }
        if ss > 0 { writeln!(f, "      ss: {ss:#06x} ({ss})")?; }
        if ds > 0 { writeln!(f, "      ds: {ds:#06x} ({ds})")?; }

        writeln!(f, "      ip: {ip:#06x} ({ip})")?;
        
        if flags.len() > 0 { writeln!(f, "   flags: {flags}")?; }
        
        Ok(())
    }
}