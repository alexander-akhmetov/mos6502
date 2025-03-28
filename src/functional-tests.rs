use std::fs;
use std::path::Path;

extern crate mos6502;
use mos6502::cpu::CPU;

extern crate clap;
// Use clap's derive feature for argument parsing
use clap::Parser;

#[macro_use]
extern crate log;
extern crate env_logger;

/// MOS 6502 functional test runner
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[arg(value_name = "FILE", default_value = "6502_functional_test.bin")]
    binary: String,
}

fn main() {
    env_logger::init();

    // Parse arguments using the derived parser
    let cli = Cli::parse();

    let mut cpu = CPU::new();

    // Use the parsed argument value
    let filename = &cli.binary;

    if !Path::new(filename).exists() {
        panic!("File {} does not exist", filename);
    }

    cpu.load(&fs::read(filename).unwrap(), 0);
    cpu.pc = 0x400;

    let mut trapped = 0;
    let mut brk = false;
    println!("Test started");
    while !brk {
        brk = cpu.step();
        if cpu.memory.get(cpu.pc) == 0xD0 && cpu.memory.get(cpu.pc + 1) == 0xFE {
            trapped += 1;
        } else {
            trapped = 0;
        }
        if trapped == 3 {
            brk = true;
            info!("possible cycle detected");
        }
        if cpu.pc == 3399 {
            brk = true;
            println!("SUCCESS");
        }
    }
}
