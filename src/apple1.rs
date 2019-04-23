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

const CR: u8 = 0x0D;
const KBD: u16 = 0xD010;
const KBDCR: u16 = 0xD011;
const DSP: u16 = 0xD012;
const BASIC_ADDR: u16 = 0xE000;
const WOZMON_ADDR: u16 = 0xFF00;

struct Apple1 {
    cpu: CPU,
    disable_screen: bool,
}

impl Apple1 {
    pub fn new(disable_screen: bool, wozmon: &str) -> Apple1 {
        let mut apple1 = Apple1 {
            cpu: CPU::new(),
            disable_screen,
        };

        let wozmon_bytes = fs::read(wozmon).expect("Can't read wozmon file");
        apple1.load(&wozmon_bytes, WOZMON_ADDR);

        fn read_callback(addr: u16) -> Option<(u16, u8)> {
            match addr {
                KBD => Some((KBDCR, 0)),
                _ => None,
            }
        };

        apple1.cpu.memory.register_read_callback(read_callback);

        fn write_callback(addr: u16, value: u8) -> Option<(u16, u8)> {
            match (addr, value) {
                (DSP, 0) => None,
                (KBD, _) => None,
                (KBDCR, _) => None,
                (DSP, v) => Some((DSP, v | 0b1000_0000)),
                _ => None,
            }
        };
        apple1.cpu.memory.register_write_callback(write_callback);

        apple1
    }

    pub fn run(&mut self) {
        let (tx, rx): (Sender<u8>, Receiver<u8>) = mpsc::channel();
        if !self.disable_screen {
            ncurses::initscr();
            ncurses::resize_term(60, 40);
            ncurses::scrollok(ncurses::stdscr(), true);
            ncurses::noecho();
            ncurses::raw();
            thread::spawn(move || Apple1::read_input(tx));
        }

        while !self.cpu.step() {
            self.print_output_to_display();

            if let Ok(c) = rx.try_recv() {
                match c {
                    0x03 => break, // ^c
                    0x05 => {
                        self.print_status();
                        continue;
                    } // ^e
                    _ => {}
                };

                self.write_kbd_input(c);
            }
            thread::sleep(time::Duration::from_micros(100));
        }
        ncurses::endwin();
    }

    fn char_to_apple1(&self, c: u8) -> u8 {
        let mut c = c.to_ascii_uppercase() as u8;
        if c == 0xA {
            c = CR; // CR instead of NL
        }
        c |= 0x80; // apple1 ascii + set bit 7
        c
    }

    fn write_kbd_input(&mut self, c: u8) {
        self.cpu.memory.set(KBD, self.char_to_apple1(c));
        self.cpu.memory.set(KBDCR, 0b1000_0000);
    }

    fn read_input(tx: Sender<u8>) {
        // reads user input from keyboard
        loop {
            tx.send(ncurses::getch() as u8).unwrap();
        }
    }

    fn print_status(&mut self) {
        let status = &format!("{}", &self.status_string());
        debug!("{}", status);
        if !self.disable_screen {
            ncurses::addstr(status);
            ncurses::addstr("\n");
        };
    }

    fn status_string(&mut self) -> String {
        format!(
            "[apple1] pc=0x{:X} a=0x{:X} x=0x{:X} y=0x{:X} p=0b{:08b} video=0b{:08b} kbd=0b{:08b}",
            self.cpu.pc,
            self.cpu.a,
            self.cpu.x,
            self.cpu.y,
            self.cpu.p,
            self.cpu.memory.get(DSP),
            self.cpu.memory.get(KBD),
        )
    }

    pub fn load(&mut self, program: &[u8], addr: u16) {
        self.cpu.load(program, addr);
    }

    fn print_output_to_display(&mut self) {
        let value = self.cpu.memory.get(DSP);

        if value & 0b1000_0000 != 0 {
            self.cpu.memory.set(DSP, 0);
            let value = value & 0b0111_1111; // remove 7 bit
            if value == CR {
                ncurses::addch('\n' as u64);
            } else {
                ncurses::addch(u64::from(value));
            }
            info!("[apple1 -> screen] 0x{:X}", value);
            ncurses::refresh();
        } else {
            debug!("no video output");
        }
    }
}

fn main() {
    env_logger::init();

    let matches = App::new("mos6502-cli")
        .version("0.0.1")
        .author("Alexander Akhmetov")
        .arg(Arg::with_name("binary").short("b").help("Load binary file"))
        .arg(
            Arg::with_name("disable-screen")
                .short("s")
                .help("Disable ncurses screen"),
        )
        .arg(
            Arg::with_name("start-addr")
                .short("a")
                .help("Start at addr")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("load-addr")
                .short("l")
                .help("Load program at addr")
                .takes_value(true),
        )
        .arg(
            Arg::with_name("program")
                .short("p")
                .help("Load additional program to EA60")
                .takes_value(true),
        )
        .get_matches();

    let start_at_addr = matches.value_of("start-addr").unwrap_or("FF00");
    let start_at_addr =
        u16::from_str_radix(start_at_addr, 16).expect("Can't parse HEX start address");
    let load_program_at = matches.value_of("load-addr").unwrap_or("E000");
    let load_program_at =
        u16::from_str_radix(load_program_at, 16).expect("Can't parse HEX start address");

    let mut apple1 = Apple1::new(matches.is_present("disable-screen"), "sys/wozmon.bin");

    if matches.is_present("program") {
        let original_pc = apple1.cpu.pc;
        let filename = matches.value_of("program").unwrap();
        if filename.ends_with("asm") {
            apple1.load(&assemble_file(filename), load_program_at);
        } else {
            apple1.load(&fs::read(filename).unwrap(), load_program_at);
        }
        apple1.cpu.pc = original_pc;
    }

    apple1.cpu.pc = start_at_addr;

    apple1.run();
}
