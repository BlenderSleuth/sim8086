use std::env;
use std::path::Path;

use sim8086::{simulate, DefaultDelegate};

// For decoder testing / comparison
// nu: 
// nasm input.asm | cargo run -- input | save -f output.asm; nasm output.asm; fc input output

// For simulation testing
// nu:
// nasm input.asm | cargo run -- input --exec --print | save -f output.txt
fn main() -> Result<(), std::io::Error>{
    // Get cmdline args
    let mut args = env::args();
    // Ignore first argument (the executable name)
    args.next();
    let input = args.next().expect("First argument should be input executable.");
    let mut exec= false;
    let mut print= false; 
    let mut dump_file = None;
    while let Some(arg) = args.next() {
        if arg == "--exec" {
            exec = true;
        } else if arg == "--print" {
            print = true;
        } else if arg == "--dump" {
            dump_file = Some(if let Some(file) = args.next() {
                file
            } else {
                String::from("dump.data")
            });
        }
    }
    
    let output = simulate(Path::new(&input), DefaultDelegate {}, exec, dump_file.as_deref())?;
    if print || !exec {
        print!("{output}");
    }
    
    Ok(())
}