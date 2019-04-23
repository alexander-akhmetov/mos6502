use std::fs;
use std::sync::mpsc;
use std::sync::mpsc::{Receiver, Sender};
use std::{thread, time};

extern crate mos6502;
use mos6502::asm::assemble_file;
use mos6502::cpu::CPU;

extern crate ncurses;

extern crate clap;
use clap::{App, Arg};

#[macro_use]
extern crate log;
extern crate env_logger;

fn main() {
    env_logger::init();

    let matches = App::new("mos6502-cli")
        .version("0.1.0")
        .author("Alexander Akhmetov")
        .arg(
            Arg::with_name("binary")
                .short("b")
                .help("Load binary file")
                .index(1),
        )
        .arg(
            Arg::with_name("start-addr")
                .short("a")
                .help("Start at addr")
                .index(2),
        )
        .get_matches();

    let start_at_addr = matches.value_of("start-addr").unwrap();
    let start_at_addr =
        u16::from_str_radix(start_at_addr, 16).expect("Can't parse HEX start address");

    let mut cpu = CPU::new();

    fn write_callback(addr: u16, value: u8) -> Option<(u16, u8)> {
        match (addr, value) {
            (0x000F, v) => {
                // info!("0x000f: 0x{:X}", v);
                None
            }
            _ => None,
        }
    };
    cpu.memory.register_write_callback(write_callback);

    let filename = matches.value_of("binary").unwrap();
    cpu.load(&fs::read(filename).unwrap(), 0);
    cpu.pc = start_at_addr;

    let mut trapped = 0;
    let mut brk = false;
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
    }

    info!("mem 000F: 0x{:X}", cpu.memory.get(0x000F));
}
