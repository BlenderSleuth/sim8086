pub mod sim;

pub use sim::simulation::{Simulator, SimulatorDelegate, DefaultDelegate, SimulateErr, InterruptErr};

use std::fs::File;
use std::fmt::Write;
use std::path::Path;
use std::io::prelude::*;

pub fn simulate(binary_path: &Path, mut delegate: impl SimulatorDelegate, print: bool, exec: bool, clocks: bool, dump_file: Option<&str>) -> Result<String, std::io::Error> {
    let mut file = match File::open(binary_path) {
        Err(why) => panic!("Could not open {}: {why}", binary_path.to_str().unwrap()),
        Ok(file) => file,
    };

    let mut output = String::new();

    let mut instructions = Vec::<u8>::new();
    file.read_to_end(&mut instructions)?;

    let memory_size = 65536; // 2^16 = 65K
    let instructions_ptr = 0;
    let mut simulator = Simulator::new(memory_size, instructions_ptr, &instructions);

    if print && !exec {
        writeln!(output, "bits 16").unwrap();
    }

    let mut old_ip = simulator.get_ip();
    let mut total_cycles: u32 = 0;
    let mut cycles = if clocks { Some(&mut total_cycles) } else { None };
    while let Some(instruction) = simulator.decode() {
        if print {
            write!(output, "{instruction}").unwrap();
        }

        // simulate
        if exec {
            if let Err(err) = delegate.simulate(&mut simulator) {
                match err {
                    SimulateErr::UnknownInstruction => panic!("Cannot simulate instruction: {instruction}"),
                    SimulateErr::Halt => break,
                }
            }

            let sim_result = simulator.simulate(&instruction, cycles.as_deref_mut(), |simulator, code| {
                delegate.interrupt_handler(simulator, code)
            });

            match sim_result {
                Ok(result) => if print {
                    let ip = simulator.get_ip();
                    write!(output, " ; {result} ip:{old_ip:#0x}->{ip:#0x}").unwrap();
                    old_ip = ip;
                }
                Err(err) => match err {
                    SimulateErr::UnknownInstruction => panic!("Cannot simulate instruction: {instruction}"),
                    SimulateErr::Halt => break,
                }
            }
        }

        if print {
            writeln!(output).unwrap();
        }
    }

    if print && exec {
        writeln!(output).unwrap();
        writeln!(output, "Final registers:").unwrap();
        writeln!(output, "{simulator}").unwrap();
    }

    if let Some(dumpfile) = dump_file {
        simulator.dump(dumpfile)?;
    }

    Ok(output)
}

pub fn simple_simulate(binary_path: &Path) -> Result<String, std::io::Error> {
    simulate(&binary_path, DefaultDelegate {}, true, false, false, None)
}

#[cfg(test)]
mod tests {
    use std::fs::{File, remove_file};
    use std::io::{Read, Write};
    use std::path::{Path, PathBuf};
    use std::process::Command;
    use super::{DefaultDelegate, simulate, simple_simulate};

    fn get_listing_path(listing: &str) -> PathBuf {
        let perf_aware_home = Path::new("../computer_enhance/perfaware/part1/");
        perf_aware_home.join(Path::new(listing))
    }

    fn run_decode_listing(listing: &str) {
        let input_listing_path = get_listing_path(listing);
        let output = simple_simulate(&input_listing_path).expect("Simulator error.");

        let listing_asm_path = format!("{listing}.asm");

        let mut reference_buf = Vec::new();
        let mut output_buf = Vec::new();
        // Write and flush simulator output
        {
            let mut output_asm_file = File::create(&listing_asm_path).expect("Could not create output file.");
            write!(output_asm_file, "{output}").unwrap();
        }
        // Assemble output read binary
        {
            Command::new("nasm")
                .arg(&listing_asm_path)
                .spawn().expect("nasm command failed")
                .wait().expect("nasm command failed");

            let mut reference_binary_file = File::open(input_listing_path).expect("Listing input binary not found.");
            reference_binary_file.read_to_end(&mut reference_buf).expect("Listing input binary could not be read.");

            let mut output_binary_file = File::open(listing).expect("Simulator output binary not found.");
            output_binary_file.read_to_end(&mut output_buf).expect("Simulator output binary could not be read.");
        }

        // Clean up
        remove_file(listing_asm_path).unwrap();
        remove_file(listing).unwrap();

        // Final compare
        assert_eq!(reference_buf, output_buf, "Input output files were not the same");
    }

    fn run_simulate_listing(listing: &str) {
        let input_listing_path = get_listing_path(listing);
        let output = simulate(&input_listing_path, DefaultDelegate {}, true, true, false, None).expect("Simulator error.");

        let reference_path = format!("{}.txt", input_listing_path.to_str().unwrap());

        let mut reference_file = File::open(reference_path).expect("Reference file not found.");
        let mut reference_str = String::new();
        reference_file.read_to_string(&mut reference_str).expect("Reference output could not be read.");

        let first_line = reference_str.find("\n").unwrap();
        let reference_str_cmp = &reference_str[first_line..];

        assert_eq!(output, reference_str_cmp, "Simulator run output files were not the same");
    }

    #[test]
    fn listing_37() {
        run_decode_listing("listing_0037_single_register_mov");
    }

    #[test]
    fn listing_38() {
        run_decode_listing("listing_0038_many_register_mov");
    }

    #[test]
    fn listing_39() {
        run_decode_listing("listing_0039_more_movs");
    }

    #[test]
    fn listing_40() {
        run_decode_listing("listing_0040_challenge_movs");
    }

    #[test]
    fn listing_41() {
        run_decode_listing("listing_0041_add_sub_cmp_jnz");
    }

    /*
    #[test]
    fn listing_42() {
        // Not finished
        run_decode_listing("listing_0042_completionist_decode");
    }
    */

    /*#[test]
    fn listing_43() {
        run_simulate_listing("listing_0043_immediate_movs");
    }*/
}