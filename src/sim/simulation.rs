use std::fmt::{self, Formatter};
use crate::sim::operations::RegisterMemoryImmediateOp;

use super::instruction::Instruction;
use super::register_memory::*;
use super::register_memory::RegisterStorage;

pub struct Simulation {
    register_storage: RegisterStorage,
    zero_flag: bool,
    parity_flag: bool,
    sign_flag: bool,
}

impl Simulation {
    pub fn new() -> Self {
        Self {
            register_storage: RegisterStorage::new(),
            zero_flag: false,
            parity_flag: false,
            sign_flag: false,
        }
    }

    pub fn simulate(&mut self, instruction: Instruction) -> String {
        match instruction {
            Instruction::Mov(move_op) => {
                let new_value = match move_op.src {
                    ImmediateRegisterMemorySegment::Immediate(immediate) => immediate.get_data(),
                    ImmediateRegisterMemorySegment::RM(rm) => self.read_rm(rm, false),
                    ImmediateRegisterMemorySegment::SegmentRegister(segment) => 
                        self.register_storage.read_segment_register(segment),
                };
                
                match move_op.dest {
                    RegisterMemorySegment::RM(rm) => {
                        let old_value = self.write_rm(rm, new_value);
                        let new_value = self.read_rm(rm, true);
                        format!("{rm}:{:#0x}->{:#0x}", old_value, new_value)
                    },
                    RegisterMemorySegment::SegmentRegister(segment) => {
                        let old_value = self.register_storage.write_segment_register(segment, new_value);
                        format!("{segment}:{:#0x}->{:#0x}", old_value, new_value)
                    }
                }
            }
            Instruction::Add(add_op) => {
                let (arg0, arg1) = self.get_args(add_op);
                let result = arg0 + arg1;

                self.write_rm(add_op.dest, result);
                let new_value = self.read_rm(add_op.dest, true);

                format!("{}:{:#0x}->{:#0x} {}", add_op.dest, arg0, new_value, self.set_flags(result))
            }
            Instruction::Sub(sub_op) => {
                let (arg0, arg1) = self.get_args(sub_op);
                let result = arg0 - arg1;
                
                self.write_rm(sub_op.dest, result);
                let new_value = self.read_rm(sub_op.dest, true);
                
                format!("{}:{:#0x}->{:#0x} {}", sub_op.dest, arg0, new_value, self.set_flags(result))
            }
            Instruction::Cmp(cmp_op) => {
                let (arg0, arg1) = self.get_args(cmp_op);
                let result = arg0 - arg1;
                self.set_flags(result)
            }
            _ => String::new(),
        }
    }
    
    fn get_args(&self, op: RegisterMemoryImmediateOp) -> (u16, u16) {
        (self.read_rm(op.dest, false), self.read_irm(op.src))
    }
    
    fn read_irm(&self, irm: ImmediateRegisterMemory) -> u16 {
        match irm {
            ImmediateRegisterMemory::Immediate(imm) => {
                imm.get_data()
            }
            ImmediateRegisterMemory::RM(rm) => self.read_rm(rm, false),
        }
    }
    
    fn read_rm(&self, rm: RegisterMemory, to_whole: bool) -> u16 {
        match rm {
            RegisterMemory::DirectAddress(_addr) => {
                0
            }
            RegisterMemory::Memory(_mem) => {
                0
            }
            RegisterMemory::Register(reg) => {
                self.register_storage.read_register(if to_whole { reg.to_whole() } else { reg })
            }
        }
    }
    
    fn write_rm(&mut self, rm: RegisterMemory, value: u16) -> u16 {
        match rm {
            RegisterMemory::DirectAddress(_addr) => {
                0
            }
            RegisterMemory::Memory(_mem) => {
                0
            }
            RegisterMemory::Register(reg) => {
                let old_value = self.register_storage.write_register(reg, value);
                old_value
            }
        }
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
            format!("flags:{}->{}", old_flags, new_flags)
        } else { 
            String::new() 
        }
    }
}

impl fmt::Display for Simulation {
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
        
        let flags = self.print_flags();
        
        if ax > 0 { writeln!(f, "      ax: {:#06x} ({ax})", ax)?; }
        if bx > 0 { writeln!(f, "      bx: {:#06x} ({bx})", bx)?; }
        if cx > 0 { writeln!(f, "      cx: {:#06x} ({cx})", cx)?; }
        if dx > 0 { writeln!(f, "      dx: {:#06x} ({dx})", dx)?; }

        if sp > 0 { writeln!(f, "      sp: {:#06x} ({sp})", sp)?; }
        if bp > 0 { writeln!(f, "      bp: {:#06x} ({bp})", bp)?; }
        if si > 0 { writeln!(f, "      si: {:#06x} ({si})", si)?; }
        if di > 0 { writeln!(f, "      di: {:#06x} ({di})", di)?; }

        if es > 0 { writeln!(f, "      es: {:#06x} ({es})", es)?; }
        if ss > 0 { writeln!(f, "      ss: {:#06x} ({ss})", ss)?; }
        if ds > 0 { writeln!(f, "      ds: {:#06x} ({ds})", ds)?; }

        if flags.len() > 0 { writeln!(f, "   flags: {flags}")?; }
        
        Ok(())
    }
}