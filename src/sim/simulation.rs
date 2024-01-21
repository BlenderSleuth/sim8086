use std::fmt::{self, Formatter};

use super::instruction::Instruction;
use super::register_memory::*;
use super::register_memory::RegisterStorage;

pub struct Simulation {
    register_storage: RegisterStorage,
}

impl Simulation {
    pub fn new() -> Self {
        Self {
            register_storage: RegisterStorage::new(),
        }
    }

    pub fn simulate(&mut self, instruction: Instruction) -> String {
        match instruction {
            Instruction::Mov(move_op) => {
                let new_value = match move_op.src {
                    ImmediateRegisterMemorySegment::Immediate(immediate) => {
                        immediate.get_data()
                    }
                    ImmediateRegisterMemorySegment::RM(rm) => {
                        match rm {
                            RegisterMemory::DirectAddress(_addr) => {
                                0
                            }
                            RegisterMemory::Memory(_mem) => {
                                0
                            }
                            RegisterMemory::Register(reg) => {
                                self.register_storage.read_register(reg)
                            }
                        }
                    },
                    ImmediateRegisterMemorySegment::SegmentRegister(segment) => {
                        self.register_storage.read_segment_register(segment)
                    },
                };
                
                match move_op.dest {
                    RegisterMemorySegment::RM(rm) => match rm {
                        RegisterMemory::DirectAddress(_addr) => {
                            String::new()
                        }
                        RegisterMemory::Memory(_mem) => {
                            String::new()
                        }
                        RegisterMemory::Register(reg) => {
                            let old_value = self.register_storage.write_register(reg, new_value);
                            let new_value = self.register_storage.read_register(reg.to_whole());
                            format!("{reg}:{:#0x}->{:#0x}", old_value, new_value)
                        }
                    }
                    RegisterMemorySegment::SegmentRegister(segment) => {
                        let old_value = self.register_storage.write_segment_register(segment, new_value);
                        format!("{segment}:{:#0x}->{:#0x}", old_value, new_value)
                    }
                }
            }
            _ => String::new(),
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
        
        writeln!(f, "      ax: {:#06x} ({})", ax, ax)?;
        writeln!(f, "      bx: {:#06x} ({})", bx, bx)?;
        writeln!(f, "      cx: {:#06x} ({})", cx, cx)?;
        writeln!(f, "      dx: {:#06x} ({})", dx, dx)?;

        writeln!(f, "      sp: {:#06x} ({})", sp, sp)?;
        writeln!(f, "      bp: {:#06x} ({})", bp, bp)?;
        writeln!(f, "      si: {:#06x} ({})", si, si)?;
        writeln!(f, "      di: {:#06x} ({})", di, di)?;

        writeln!(f, "      es: {:#06x} ({})", es, es)?;
        writeln!(f, "      ss: {:#06x} ({})", ss, ss)?;
        writeln!(f, "      ds: {:#06x} ({})", ds, ds)
    }
}