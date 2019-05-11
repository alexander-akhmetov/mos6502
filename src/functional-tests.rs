use std::fs;
use std::path::Path;

extern crate mos6502;
use mos6502::cpu::CPU;

extern crate clap;
use clap::{App, Arg};

#[macro_use]
extern crate log;
extern crate env_logger;

fn main() {
    env_logger::init();

    let matches = App::new("MOS 6502 functional test runner")
        .arg(
            Arg::with_name("binary")
                .short("b")
                .help("Load binary file")
                .index(1),
        )
        .get_matches();

    let mut cpu = CPU::new();

    let filename = matches
        .value_of("binary")
        .unwrap_or("6502_functional_test.bin");

    if !Path::new(filename).exists() {
        panic!(format!("File {} does not exist", filename))
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
