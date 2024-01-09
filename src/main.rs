#![allow(dead_code)]

mod sim;
use sim::InstructionIterator;

use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() -> std::io::Result<()> {
    // For testing / comparison
    // nu: nasm input.asm | cargo run | save -f output.asm; nasm output.asm; fc input output

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

    println!("bits 16");

    for instruction in InstructionIterator::new(instructions) {
        println!("{instruction}");
    }

    Ok(())
}
