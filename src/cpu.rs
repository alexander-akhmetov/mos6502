use crate::operation;
use crate::utils;

pub struct CPU {
    a: u8,
    x: u8,
    y: u8,
    s: u8,
    p: u8,
    pc: u16,
    memory: [u8; 64 * 1024],
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            a: 0,
            x: 0,
            y: 0,
            s: 0,
            p: 0,
            pc: 0,
            memory: [0; 64 * 1024],
        }
    }

    pub fn load(&mut self, buf: &[u8], addr: u16) {
        let program_end_addr = buf.len() + addr as usize;
        if program_end_addr > 65536 {
            panic!("Wrong address to load the program");
        }

        self.memory[(addr as usize)..program_end_addr].clone_from_slice(buf);
        self.pc = addr;
    }

    pub fn run(&mut self) {
        loop {
            let operation = operation::by_code(self.memory[self.pc as usize]);

            match operation.name {
                "BRK" => return,
                "LDA" => self.operation_lda(operation),
                "ADC" => self.operation_adc(operation),
                "STA" => self.operation_sta(operation),
                _ => panic!("unknown operation"),
            };

            self.pc += operation.length as u16;
        }
    }

    fn operation_lda(&mut self, operation: &operation::Operation) {
        self.a = self.get_operand(operation) as u8;
    }

    fn operation_adc(&mut self, operation: &operation::Operation) {
        self.a += self.get_operand(operation) as u8;
    }

    fn operation_sta(&mut self, operation: &operation::Operation) {
        let addr = self.next_two_byte_number();
        self.memory[addr as usize] = self.a;
    }

    fn get_operand(&self, operation: &operation::Operation) -> usize {
        match operation.addressing {
            operation::AddressingMode::Immediate => self.memory[self.pc as usize + 1] as usize,
            operation::AddressingMode::Absolute => {
                self.memory[self.next_two_byte_number() as usize] as usize
            }
            _ => panic!("unknown addressing mode"),
        }
    }

    fn next_two_byte_number(&self) -> u16 {
        // returns next two bytes (self.pc + 1, self.pc + 2) as one u16
        // (converts from little endian)
        utils::little_endian_to_u16(
            self.memory[self.pc as usize + 1],
            self.memory[self.pc as usize + 2],
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::cpu::CPU;
    use crate::operation;

    #[test]
    fn test_new() {
        let c = CPU::new();

        assert_eq!(c.a, 0);
        assert_eq!(c.x, 0);
        assert_eq!(c.y, 0);
        assert_eq!(c.s, 0);
        assert_eq!(c.p, 0);
        assert_eq!(c.pc, 0);
        assert_eq!(&c.memory[..], &[0; 64 * 1024][..]);
    }

    #[test]
    fn test_load() {
        // test when it loads a three bytes dump at a specific location
        let mut cpu = CPU::new();

        let addr = 32768;
        let bytes = [1; 3];

        assert_eq!(&cpu.memory[..], &[0; 64 * 1024][..]);
        cpu.load(&bytes, addr);

        let exp_memory = [0, 1, 1, 1, 0];
        assert_eq!(&cpu.memory[32767..32772], exp_memory);

        assert_eq!(cpu.pc, addr);
    }

    #[test]
    #[should_panic]
    fn test_load_data_too_big() {
        let mut cpu = CPU::new();
        cpu.load(&[1, 2, 3], 65535);
    }

    #[test]
    fn test_operation_lda() {
        let mut cpu = CPU::new();

        let program: [u8; 3] = [
            operation::by_name("LDA", operation::AddressingMode::Immediate).code,
            10,
            operation::by_name("BRK", operation::AddressingMode::Immediate).code,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.a, 0);

        cpu.run();

        assert_eq!(cpu.a, 10);
    }

    #[test]
    fn test_operation_adc_immediate() {
        let mut cpu = CPU::new();
        cpu.a = 10;

        let program: [u8; 3] = [
            operation::by_name("ADC", operation::AddressingMode::Immediate).code,
            15,
            operation::by_name("BRK", operation::AddressingMode::Immediate).code,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.a, 10);

        cpu.run();

        assert_eq!(cpu.a, 25);
    }

    #[test]
    fn test_operation_adc_absolute() {
        let mut cpu = CPU::new();
        cpu.a = 10;

        let program: [u8; 4] = [
            operation::by_name("ADC", operation::AddressingMode::Absolute).code,
            0xff,
            0xaa,
            operation::by_name("BRK", operation::AddressingMode::Immediate).code,
        ];
        cpu.load(&program, 0);
        cpu.memory[43775] = 15; // 0xaaff

        assert_eq!(cpu.a, 10);

        cpu.run();

        assert_eq!(cpu.a, 25);
    }

    #[test]
    fn test_operation_sta() {
        let mut cpu = CPU::new();
        cpu.a = 10;

        let program: [u8; 4] = [
            operation::by_name("STA", operation::AddressingMode::Absolute).code,
            50,
            0,
            operation::by_name("BRK", operation::AddressingMode::Immediate).code,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.memory[50], 0);

        cpu.run();

        assert_eq!(cpu.memory[50], cpu.a);
    }

    #[test]
    fn test_next_two_bytes() {
        let mut cpu = CPU::new();
        let bytes = [0, 0x0a, 0x0b];
        cpu.load(&bytes, 0);
        assert_eq!(cpu.next_two_byte_number(), 2826);
    }

    #[test]
    fn test_next_two_bytes_with_only_lower_byte() {
        let mut cpu = CPU::new();
        let bytes = [0, 10, 0];
        cpu.load(&bytes, 0);
        assert_eq!(cpu.next_two_byte_number(), 10);
    }

    #[test]
    fn test_simple_program() {
        // loads 100 from the memory
        // adds 7 to the a register
        // stores a register at address 0x000f
        let mut cpu = CPU::new();
        cpu.a = 10;

        let program: [u8; 8] = [
            operation::by_name("LDA", operation::AddressingMode::Immediate).code,
            100,
            operation::by_name("ADC", operation::AddressingMode::Immediate).code,
            7,
            operation::by_name("STA", operation::AddressingMode::Absolute).code,
            15,
            0,
            operation::by_name("BRK", operation::AddressingMode::Immediate).code,
        ];
        cpu.load(&program, 0);

        cpu.run();

        assert_eq!(cpu.a, 107);
        assert_eq!(cpu.memory[15], 107);
    }

}
