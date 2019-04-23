pub mod cpu;
mod operation;
mod utils;
use cpu::CPU;

fn main() {
    println!("Hello, world!");
    let cpu = CPU::new();
}
