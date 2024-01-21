#![allow(dead_code)]

mod sim;
use sim::InstructionIterator;
use sim::simulation::Simulation;

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
    let exec = args.len() > 1 && args[1] == "--exec";
    
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
    
    
    let mut simulation = Simulation::new();
    
    if !exec {
        println!("bits 16");
    }

    for instruction in InstructionIterator::new(instructions) {
        print!("{instruction}");
        
        // simulate
        if exec {
            let sim_result = simulation.simulate(instruction);
            println!(" ; {sim_result}")
        }
    }
    
    if exec {
        println!();
        println!("Final registers:");
        println!("{simulation}");
    }

    Ok(())
}
