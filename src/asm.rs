use std::fs;

use crate::operation;

pub fn assemble_file(filename: &str) -> Vec<u8> {
    let asm_content = fs::read_to_string(filename).expect("Can't read the asm file");
    assemble(&asm_content)
}

pub fn assemble(program: &str) -> Vec<u8> {
    let lines = program.split('\n');
    let mut bytes = vec![];

    for line in lines {
        let t_line = line.trim();
        if !t_line.is_empty() {
            let (operation, mut args) = parse_line(t_line);
            bytes.push(operation);
            bytes.append(&mut args);
        }
    }

    bytes
}

fn parse_line(line: &str) -> (u8, Vec<u8>) {
    let split: Vec<&str> = line.splitn(2, ' ').collect();

    let mut args = vec![];
    let mut addressing = operation::AddressingMode::Implied;
    let operation_name = split[0];

    if split.len() != 1 {
        match split[1] {
            arg if arg.starts_with("#$") => {
                addressing = operation::AddressingMode::Immediate;
                let arg = arg.trim_start_matches("#$");
                args.push(u8::from_str_radix(arg, 16).unwrap());
            }
            arg if arg.starts_with('$') => {
                let arg = arg.trim_start_matches('$');

                // if arg.len() == 2 - we use zeropage addressing
                match arg.len() {
                    2 => {
                        addressing = operation::AddressingMode::ZeroPage;
                        args.push(u8::from_str_radix(arg, 16).unwrap());
                    }
                    4 => {
                        addressing = operation::AddressingMode::Absolute;
                        let arg = u16::from_str_radix(arg, 16).unwrap();
                        args.push(arg as u8);
                        args.push((arg >> 8) as u8);
                    }
                    _ => panic!("Can't parse argument"),
                }
            }
            _ => panic!("unknown operand"),
        };
    }

    // first it looks only by name, if there are many opcodes with such name,
    // it looks by name and addressing
    // todo: change
    let opcodes = operation::by_name(operation_name);
    let opcode = match opcodes.len() {
        0 => panic!("unknown operation name: {}", operation_name),
        1 => opcodes[0],
        _ => operation::by_name_and_addressing(operation_name, addressing),
    };

    (opcode.code, args)
}

pub fn disassemble(buf: &[u8]) -> String {
    let op = operation::by_code(buf[0]);

    let mut instruction = String::from(op.name);

    if op.length > 1 {
        match op.addressing {
            operation::AddressingMode::Immediate => instruction = format!("{} #$", instruction),
            _ => instruction = format!("{} #", instruction),
        }
        instruction = format!("{}{:X}", instruction, buf[1]);
    }
    if op.length > 2 {
        instruction = format!("{}{:X}", instruction, buf[2]);
    }

    instruction
}

#[cfg(test)]
mod tests {
    use crate::asm::assemble;

    #[test]
    fn test_assemble_one_line() {
        let program = "BRK";
        let bytes = assemble(&program);
        let expected: [u8; 1] = [0];
        assert_eq!(&bytes, &expected);
    }

    #[test]
    fn test_assemble_one_line_immediate_addressing() {
        let program = "LDA #$AF";
        let bytes = assemble(&program);
        let expected: [u8; 2] = [0xA9, 0xAF];
        assert_eq!(&bytes, &expected);
    }

    #[test]
    fn test_assemble() {
        let program = "LDA #$c0
                       TAX
                       INX
                       ADC #$c4
                       BRK";
        let bytes = assemble(&program);
        let expected: [u8; 7] = [0xa9, 0xc0, 0xaa, 0xe8, 0x69, 0xc4, 0x00];
        assert_eq!(&bytes, &expected);
    }

    #[test]
    fn test_assemble_2() {
        let program = "LDA #$80
                       STA $01
                       ADC $01";
        let bytes = assemble(&program);
        let expected: [u8; 6] = [0xa9, 0x80, 0x85, 0x01, 0x65, 0x01];
        assert_eq!(&bytes, &expected);
    }

    #[test]
    fn code_with_bne_instruction() {
        let program = "LDX #$5
                       DEX
                       CPX #$0
                       BNE #$FD
                       BRK";
        let bytes = assemble(&program);
        let expected: [u8; 8] = [0xa2, 0x05, 0xca, 0xe0, 0x00, 0xd0, 0xfd, 0x00];
        assert_eq!(&bytes, &expected);
    }
}
