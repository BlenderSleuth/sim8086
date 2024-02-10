use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=sna86/-sna86.asm");

    Command::new("nasm").args(&["sna86/-sna86.asm sna86.asm"]).status().unwrap_or_else(|e| {
        println!("cargo:warning={e}");
        panic!("NASM build failed. Make sure you have nasm installed or disable the \"asm\" feature.\n\
        You can get NASM from https://nasm.us or your system's package manager.\n\nerror: {e}");
    });
}