#![allow(dead_code)]

mod sim;
use sim::simulation::Simulator;

use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::env;


fn main() -> std::io::Result<()> {
    // For decoder testing / comparison
    // nu: 
    // nasm input.asm | cargo run | save -f output.asm; nasm output.asm; fc input output

    // For simulation testing
    // nu:
    // nasm input.asm | cargo run -- --exec | save -f output.txt
    
    let args: Vec<String> = env::args().collect();
    let exec = args.contains(&"--exec".to_owned());
    
    let part_one_tests = Path::new("../../computer_enhance/perfaware/part1");
    // listing_0037_single_register_mov
    // listing_0038_many_register_mov
    // listing_0039_more_movs
    // listing_0040_challenge_movs
    // listing_0041_add_sub_cmp_jnz
    // listing_0042_completionist_decode
    let _listing_path = part_one_tests.join(Path::new("listing_0042_completionist_decode"));
    
    let _input_path = Path::new("input");

    let mut file = match File::open(&_input_path) {
        Err(why) => panic!("couldn't open {}: {}", _input_path.display(), why),
        Ok(file) => file,
    };

    let mut instructions = Vec::<u8>::new();
    file.read_to_end(&mut instructions)?;

    let memory_size = 65536; // 2^16 = 65K
    let instructions_ptr = 0;
    let mut simulation = Simulator::new(memory_size, instructions_ptr, &instructions);
    
    if !exec {
        println!("bits 16");
    }
    
    let mut old_ip = simulation.get_ip();
    while let Some(instruction) = simulation.decode() {
        print!("{instruction}");
        
        // simulate
        if exec {
            let sim_result = simulation.simulate(instruction);
            
            let ip = simulation.get_ip();
            print!(" ; ip:{old_ip:#0x}->{ip:#0x} {sim_result}");
            old_ip = ip;
        }
        
        println!();
    }
    
    if exec {
        println!();
        println!("Final registers:");
        println!("{simulation}");
    }

    Ok(())
}
