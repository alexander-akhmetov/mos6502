#[derive(PartialEq, Eq)]
pub enum AddressingMode {
    Implied = 0,
    Immediate,
    Absolute,
    ZeroPage,
    IndirectAbsolute,
    AbsoluteIndexedX,
    AbsoluteIndexedY,
    ZeroPageIndexedX,
    ZeroPageIndexedY,
    IndirectIndexed,
    IndirectIndexedX,
    RelativeAddressing,
    AccumulatorAddressing,
}

pub struct Operation {
    pub name: &'static str,
    pub code: u8,
    pub length: u8,
    pub addressing: AddressingMode,
}

pub static OPERATIONS: &[Operation] = &[
    Operation {
        name: "BRK",
        code: 0x00,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "RTI",
        code: 0x40,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    // DEC
    Operation {
        name: "DEC",
        code: 0xC6,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "DEC",
        code: 0xD6,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "DEC",
        code: 0xCE,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "DEC",
        code: 0xDE,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedX,
    },
    // INC
    Operation {
        name: "INC",
        code: 0xEE,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "INC",
        code: 0xFE,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedX,
    },
    Operation {
        name: "INC",
        code: 0xE6,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "INC",
        code: 0xF6,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "INX",
        code: 0xE8,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "INY",
        code: 0xC8,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "TAX",
        code: 0xAA,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "TAY",
        code: 0xA8,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "DEX",
        code: 0xCA,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "DEY",
        code: 0x88,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "LDA",
        code: 0xa9,
        length: 2,
        addressing: AddressingMode::Immediate,
    },
    Operation {
        name: "LDA",
        code: 0xa5,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "LDA",
        code: 0xb5,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "LDA",
        code: 0xad,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "LDA",
        code: 0xB9,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedY,
    },
    Operation {
        name: "LDA",
        code: 0xBD,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedX,
    },
    Operation {
        name: "LDA",
        code: 0xA1,
        length: 2,
        addressing: AddressingMode::IndirectIndexedX,
    },
    Operation {
        name: "LDA",
        code: 0xB1,
        length: 2,
        addressing: AddressingMode::IndirectIndexed,
    },
    // LDX
    Operation {
        name: "LDX",
        code: 0xa2,
        length: 2,
        addressing: AddressingMode::Immediate,
    },
    Operation {
        name: "LDX",
        code: 0xa6,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "LDX",
        code: 0xB6,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedY,
    },
    Operation {
        name: "LDX",
        code: 0xae,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "LDX",
        code: 0xBE,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedY,
    },
    // LDY
    Operation {
        name: "LDY",
        code: 0xA0,
        length: 2,
        addressing: AddressingMode::Immediate,
    },
    Operation {
        name: "LDY",
        code: 0xA4,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "LDY",
        code: 0xB4,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "LDY",
        code: 0xAC,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "LDY",
        code: 0xBC,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedX,
    },
    Operation {
        name: "ADC",
        code: 0x69,
        length: 2,
        addressing: AddressingMode::Immediate,
    },
    Operation {
        name: "ADC",
        code: 0x6D,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "ADC",
        code: 0x65,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "ADC",
        code: 0x75,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "ADC",
        code: 0x7D,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedX,
    },
    Operation {
        name: "ADC",
        code: 0x79,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedY,
    },
    Operation {
        name: "ADC",
        code: 0x61,
        length: 2,
        addressing: AddressingMode::IndirectIndexedX,
    },
    Operation {
        name: "ADC",
        code: 0x71,
        length: 2,
        addressing: AddressingMode::IndirectIndexed,
    },
    // SBC
    Operation {
        name: "SBC",
        code: 0xE9,
        length: 2,
        addressing: AddressingMode::Immediate,
    },
    Operation {
        name: "SBC",
        code: 0xED,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "SBC",
        code: 0xFD,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedX,
    },
    Operation {
        name: "SBC",
        code: 0xF9,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedY,
    },
    Operation {
        name: "SBC",
        code: 0xE5,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "SBC",
        code: 0xF5,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "SBC",
        code: 0xF1,
        length: 2,
        addressing: AddressingMode::IndirectIndexed,
    },
    Operation {
        name: "SBC",
        code: 0xE1,
        length: 2,
        addressing: AddressingMode::IndirectIndexedX,
    },
    Operation {
        name: "STX",
        code: 0x8E,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "STX",
        code: 0x86,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "STX",
        code: 0x96,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedY,
    },
    Operation {
        name: "STY",
        code: 0x8C,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "STY",
        code: 0x84,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "STY",
        code: 0x94,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "STA",
        code: 0x8D,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "STA",
        code: 0x85,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "STA",
        code: 0x95,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "STA",
        code: 0x9D,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedX,
    },
    Operation {
        name: "STA",
        code: 0x99,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedY,
    },
    Operation {
        name: "STA",
        code: 0x81,
        length: 2,
        addressing: AddressingMode::IndirectIndexedX,
    },
    Operation {
        name: "STA",
        code: 0x91,
        length: 2,
        addressing: AddressingMode::IndirectIndexed,
    },
    Operation {
        name: "BEQ",
        code: 0xF0,
        length: 2,
        addressing: AddressingMode::RelativeAddressing,
    },
    Operation {
        name: "BNE",
        code: 0xD0,
        length: 2,
        addressing: AddressingMode::RelativeAddressing,
    },
    Operation {
        name: "BVC",
        code: 0x50,
        length: 2,
        addressing: AddressingMode::RelativeAddressing,
    },
    Operation {
        name: "BVS",
        code: 0x70,
        length: 2,
        addressing: AddressingMode::RelativeAddressing,
    },
    Operation {
        name: "BIT",
        code: 0x24,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "BIT",
        code: 0x2C,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "BCC",
        code: 0x90,
        length: 2,
        addressing: AddressingMode::RelativeAddressing,
    },
    Operation {
        name: "BMI",
        code: 0x30,
        length: 2,
        addressing: AddressingMode::RelativeAddressing,
    },
    Operation {
        name: "BPL",
        code: 0x10,
        length: 2,
        addressing: AddressingMode::RelativeAddressing,
    },
    Operation {
        name: "BCS",
        code: 0xB0,
        length: 2,
        addressing: AddressingMode::RelativeAddressing,
    },
    // CPX
    Operation {
        name: "CPX",
        code: 0xE0,
        length: 2,
        addressing: AddressingMode::Immediate,
    },
    Operation {
        name: "CPX",
        code: 0xE4,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "CPX",
        code: 0xEC,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    // EOR
    Operation {
        name: "EOR",
        code: 0x49,
        length: 2,
        addressing: AddressingMode::Immediate,
    },
    Operation {
        name: "EOR",
        code: 0x45,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "EOR",
        code: 0x55,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "EOR",
        code: 0x4D,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "EOR",
        code: 0x5D,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedX,
    },
    Operation {
        name: "EOR",
        code: 0x59,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedY,
    },
    Operation {
        name: "EOR",
        code: 0x41,
        length: 2,
        addressing: AddressingMode::IndirectIndexedX,
    },
    Operation {
        name: "EOR",
        code: 0x51,
        length: 2,
        addressing: AddressingMode::IndirectIndexed,
    },
    // CMP
    Operation {
        name: "CMP",
        code: 0xC9,
        length: 2,
        addressing: AddressingMode::Immediate,
    },
    Operation {
        name: "CMP",
        code: 0xC5,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "CMP",
        code: 0xD5,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "CMP",
        code: 0xCD,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "CMP",
        code: 0xDD,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedX,
    },
    Operation {
        name: "CMP",
        code: 0xD9,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedY,
    },
    Operation {
        name: "CMP",
        code: 0xC1,
        length: 2,
        addressing: AddressingMode::IndirectIndexedX,
    },
    Operation {
        name: "CMP",
        code: 0xD1,
        length: 2,
        addressing: AddressingMode::IndirectIndexed,
    },
    // CPY
    Operation {
        name: "CPY",
        code: 0xC0,
        length: 2,
        addressing: AddressingMode::Immediate,
    },
    Operation {
        name: "CPY",
        code: 0xC4,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "CPY",
        code: 0xCC,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "CLC",
        code: 0x18,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "CLD",
        code: 0xD8,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "CLI",
        code: 0x58,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "CLV",
        code: 0xB8,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "SEC",
        code: 0x38,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "SED",
        code: 0xF8,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "SEI",
        code: 0x78,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "NOP",
        code: 0xEA,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "JMP",
        code: 0x4C,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "JMP",
        code: 0x6C,
        length: 3,
        addressing: AddressingMode::IndirectAbsolute,
    },
    Operation {
        name: "PHA",
        code: 0x48,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "TXA",
        code: 0x8A,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "TYA",
        code: 0x98,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "TSX",
        code: 0xBA,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "PLA",
        code: 0x68,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "PLP",
        code: 0x28,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "PHP",
        code: 0x08,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    Operation {
        name: "JSR",
        code: 0x20,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "RTS",
        code: 0x60,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    // LSR
    Operation {
        name: "LSR",
        code: 0x46,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "LSR",
        code: 0x56,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "LSR",
        code: 0x4E,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "LSR",
        code: 0x5E,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedX,
    },
    Operation {
        name: "LSR",
        code: 0x4A,
        length: 1,
        addressing: AddressingMode::AccumulatorAddressing,
    },
    // ROL
    Operation {
        name: "ROL",
        code: 0x2A,
        length: 1,
        addressing: AddressingMode::AccumulatorAddressing,
    },
    Operation {
        name: "ROL",
        code: 0x26,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "ROL",
        code: 0x2E,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "ROL",
        code: 0x36,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "ROL",
        code: 0x3E,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedX,
    },
    Operation {
        name: "TXS",
        code: 0x9A,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    // ROR
    Operation {
        name: "ROR",
        code: 0x6A,
        length: 1,
        addressing: AddressingMode::AccumulatorAddressing,
    },
    Operation {
        name: "ROR",
        code: 0x66,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "ROR",
        code: 0x76,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "ROR",
        code: 0x6E,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "ROR",
        code: 0x7E,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedX,
    },
    Operation {
        name: "TXS",
        code: 0x9A,
        length: 1,
        addressing: AddressingMode::Implied,
    },
    // ASL
    Operation {
        name: "ASL",
        code: 0x0A,
        length: 1,
        addressing: AddressingMode::AccumulatorAddressing,
    },
    Operation {
        name: "ASL",
        code: 0x06,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "ASL",
        code: 0x16,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "ASL",
        code: 0x0E,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "ASL",
        code: 0x1E,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedX,
    },
    // AND
    Operation {
        name: "AND",
        code: 0x25,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "AND",
        code: 0x35,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "AND",
        code: 0x29,
        length: 2,
        addressing: AddressingMode::Immediate,
    },
    Operation {
        name: "AND",
        code: 0x2D,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "AND",
        code: 0x3D,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedX,
    },
    Operation {
        name: "AND",
        code: 0x39,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedY,
    },
    Operation {
        name: "AND",
        code: 0x21,
        length: 2,
        addressing: AddressingMode::IndirectIndexedX,
    },
    Operation {
        name: "AND",
        code: 0x31,
        length: 2,
        addressing: AddressingMode::IndirectIndexed,
    },
    //
    // ORA
    //
    Operation {
        name: "ORA",
        code: 0x09,
        length: 2,
        addressing: AddressingMode::Immediate,
    },
    Operation {
        name: "ORA",
        code: 0x05,
        length: 2,
        addressing: AddressingMode::ZeroPage,
    },
    Operation {
        name: "ORA",
        code: 0x15,
        length: 2,
        addressing: AddressingMode::ZeroPageIndexedX,
    },
    Operation {
        name: "ORA",
        code: 0x0D,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "ORA",
        code: 0x1D,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedX,
    },
    Operation {
        name: "ORA",
        code: 0x11,
        length: 2,
        addressing: AddressingMode::IndirectIndexed,
    },
    Operation {
        name: "ORA",
        code: 0x01,
        length: 2,
        addressing: AddressingMode::IndirectIndexedX,
    },
    Operation {
        name: "ORA",
        code: 0x19,
        length: 3,
        addressing: AddressingMode::AbsoluteIndexedY,
    },
];

pub fn by_code(code: u8) -> &'static Operation {
    debug!("searching for operation by code={:X}", code);
    let opcode = OPERATIONS.iter().find(|o| o.code == code);

    if let Some(opcode) = opcode {
        opcode
    } else {
        error!("Unknown operation code: 0x{:X}", code);
        panic!("unknown operation code");
    }
}

pub fn by_name(name: &str) -> Vec<&'static Operation> {
    debug!("searching for operation by name={}", name);
    OPERATIONS.iter().filter(|o| o.name == name).collect()
}

pub fn by_name_and_addressing(name: &str, addressing: AddressingMode) -> &'static Operation {
    debug!("searching for operation by name={}", name);
    OPERATIONS
        .iter()
        .find(|o| o.name == name && o.addressing == addressing)
        .expect("unknown operation name")
}

#[cfg(test)]
mod tests {
    use crate::operation::{AddressingMode, by_name_and_addressing};

    macro_rules! test_opcode {
        ($test_name:ident, $name:expr_2021, $mode:expr_2021, $expected:expr_2021) => {
            #[test]
            fn $test_name() {
                assert_eq!(by_name_and_addressing($name, $mode).code, $expected);
            }
        };
    }

    test_opcode!(brk, "BRK", AddressingMode::Implied, 0x00);
    test_opcode!(rti, "RTI", AddressingMode::Implied, 0x40);
    test_opcode!(tax, "TAX", AddressingMode::Implied, 0xAA);
    test_opcode!(tay, "TAY", AddressingMode::Implied, 0xA8);
    test_opcode!(txa, "TXA", AddressingMode::Implied, 0x8A);
    test_opcode!(tya, "TYA", AddressingMode::Implied, 0x98);
    test_opcode!(tsx, "TSX", AddressingMode::Implied, 0xBA);

    test_opcode!(php, "PHP", AddressingMode::Implied, 0x08);
    test_opcode!(plp, "PLP", AddressingMode::Implied, 0x28);

    test_opcode!(clc, "CLC", AddressingMode::Implied, 0x18);
    test_opcode!(cld, "CLD", AddressingMode::Implied, 0xD8);
    test_opcode!(cli, "CLI", AddressingMode::Implied, 0x58);
    test_opcode!(clv, "CLV", AddressingMode::Implied, 0xB8);

    test_opcode!(sec, "SEC", AddressingMode::Implied, 0x38);
    test_opcode!(sed, "SED", AddressingMode::Implied, 0xF8);
    test_opcode!(sei, "SEI", AddressingMode::Implied, 0x78);

    test_opcode!(nop, "NOP", AddressingMode::Implied, 0xEA);

    test_opcode!(inc_zero_page, "INC", AddressingMode::ZeroPage, 0xE6);
    test_opcode!(
        inc_zero_page_x,
        "INC",
        AddressingMode::ZeroPageIndexedX,
        0xF6
    );
    test_opcode!(inc_absolute, "INC", AddressingMode::Absolute, 0xEE);

    test_opcode!(dec_zero_page, "DEC", AddressingMode::ZeroPage, 0xC6);
    test_opcode!(
        dec_zero_page_x,
        "DEC",
        AddressingMode::ZeroPageIndexedX,
        0xD6
    );
    test_opcode!(dec_absolute, "DEC", AddressingMode::Absolute, 0xCE);
    test_opcode!(
        dec_absolute_x,
        "DEC",
        AddressingMode::AbsoluteIndexedX,
        0xDE
    );

    test_opcode!(inx, "INX", AddressingMode::Implied, 0xE8);
    test_opcode!(iny, "INY", AddressingMode::Implied, 0xC8);

    test_opcode!(dex, "DEX", AddressingMode::Implied, 0xCA);
    test_opcode!(dey, "DEY", AddressingMode::Implied, 0x88);

    test_opcode!(jmp_absolute, "JMP", AddressingMode::Absolute, 0x4C);
    test_opcode!(pha, "PHA", AddressingMode::Implied, 0x48);
    test_opcode!(pla, "PLA", AddressingMode::Implied, 0x68);

    test_opcode!(lsr_zero_page, "LSR", AddressingMode::ZeroPage, 0x46);
    test_opcode!(
        lsr_zero_page_x,
        "LSR",
        AddressingMode::ZeroPageIndexedX,
        0x56
    );
    test_opcode!(lsr_absolute, "LSR", AddressingMode::Absolute, 0x4E);
    test_opcode!(
        lsr_absolute_x,
        "LSR",
        AddressingMode::AbsoluteIndexedX,
        0x5E
    );
    test_opcode!(
        lsr_accumulator,
        "LSR",
        AddressingMode::AccumulatorAddressing,
        0x4A
    );

    test_opcode!(bit_zero_page, "BIT", AddressingMode::ZeroPage, 0x24);
    test_opcode!(bit_absolute, "BIT", AddressingMode::Absolute, 0x2C);

    test_opcode!(and_immediate, "AND", AddressingMode::Immediate, 0x29);

    test_opcode!(ora_immediate, "ORA", AddressingMode::Immediate, 0x09);
    test_opcode!(ora_zero_page, "ORA", AddressingMode::ZeroPage, 0x05);
    test_opcode!(
        ora_zero_page_x,
        "ORA",
        AddressingMode::ZeroPageIndexedX,
        0x15
    );
    test_opcode!(ora_absolute, "ORA", AddressingMode::Absolute, 0x0D);
    test_opcode!(
        ora_absolute_x,
        "ORA",
        AddressingMode::AbsoluteIndexedX,
        0x1D
    );
    test_opcode!(
        ora_absolute_y,
        "ORA",
        AddressingMode::AbsoluteIndexedY,
        0x19
    );

    test_opcode!(cmp_immediate, "CMP", AddressingMode::Immediate, 0xC9);
    test_opcode!(cmp_zero_page, "CMP", AddressingMode::ZeroPage, 0xC5);
    test_opcode!(cmp_absolute, "CMP", AddressingMode::Absolute, 0xCD);
    test_opcode!(
        cmp_indirect_x,
        "CMP",
        AddressingMode::IndirectIndexedX,
        0xC1
    );
    test_opcode!(cmp_indirect_y, "CMP", AddressingMode::IndirectIndexed, 0xD1);
    test_opcode!(
        cmp_zero_page_x,
        "CMP",
        AddressingMode::ZeroPageIndexedX,
        0xD5
    );
    test_opcode!(
        cmp_absolute_x,
        "CMP",
        AddressingMode::AbsoluteIndexedX,
        0xDD
    );
    test_opcode!(
        cmp_absolute_y,
        "CMP",
        AddressingMode::AbsoluteIndexedY,
        0xD9
    );

    test_opcode!(cpx_immediate, "CPX", AddressingMode::Immediate, 0xE0);
    test_opcode!(cpx_zero_page, "CPX", AddressingMode::ZeroPage, 0xE4);
    test_opcode!(cpx_absolute, "CPX", AddressingMode::Absolute, 0xEC);

    test_opcode!(cpy_immediate, "CPY", AddressingMode::Immediate, 0xC0);
    test_opcode!(cpy_zero_page, "CPY", AddressingMode::ZeroPage, 0xC4);
    test_opcode!(cpy_absolute, "CPY", AddressingMode::Absolute, 0xCC);

    test_opcode!(lda_immediate, "LDA", AddressingMode::Immediate, 0xa9);
    test_opcode!(
        lda_absolute_indexed_y,
        "LDA",
        AddressingMode::AbsoluteIndexedY,
        0xB9
    );
    test_opcode!(
        lda_absolute_indexed_x,
        "LDA",
        AddressingMode::AbsoluteIndexedX,
        0xBD
    );
    test_opcode!(lda_zero_page, "LDA", AddressingMode::ZeroPage, 0xa5);
    test_opcode!(
        lda_zero_page_x,
        "LDA",
        AddressingMode::ZeroPageIndexedX,
        0xB5
    );
    test_opcode!(lda_absolute, "LDA", AddressingMode::Absolute, 0xad);
    test_opcode!(
        lda_indirect_x,
        "LDA",
        AddressingMode::IndirectIndexedX,
        0xA1
    );
    test_opcode!(lda_indirect_y, "LDA", AddressingMode::IndirectIndexed, 0xB1);

    test_opcode!(ldx_immediate, "LDX", AddressingMode::Immediate, 0xa2);
    test_opcode!(ldx_zero_page, "LDX", AddressingMode::ZeroPage, 0xa6);
    test_opcode!(
        ldx_zero_page_y,
        "LDX",
        AddressingMode::ZeroPageIndexedY,
        0xB6
    );
    test_opcode!(ldx_absolute, "LDX", AddressingMode::Absolute, 0xae);
    test_opcode!(
        ldx_absolute_y,
        "LDX",
        AddressingMode::AbsoluteIndexedY,
        0xBE
    );

    test_opcode!(ldy_immediate, "LDY", AddressingMode::Immediate, 0xA0);
    test_opcode!(ldy_zero_page, "LDY", AddressingMode::ZeroPage, 0xA4);
    test_opcode!(
        ldy_zero_page_x,
        "LDY",
        AddressingMode::ZeroPageIndexedX,
        0xB4
    );
    test_opcode!(ldy_absolute, "LDY", AddressingMode::Absolute, 0xAC);
    test_opcode!(
        ldy_absolute_x,
        "LDY",
        AddressingMode::AbsoluteIndexedX,
        0xBC
    );

    test_opcode!(adc_immediate, "ADC", AddressingMode::Immediate, 0x69);
    test_opcode!(adc_absolute, "ADC", AddressingMode::Absolute, 0x6D);
    test_opcode!(adc_zero_page, "ADC", AddressingMode::ZeroPage, 0x65);
    test_opcode!(
        adc_zero_page_x,
        "ADC",
        AddressingMode::ZeroPageIndexedX,
        0x75
    );
    test_opcode!(
        adc_absolute_x,
        "ADC",
        AddressingMode::AbsoluteIndexedX,
        0x7D
    );
    test_opcode!(
        adc_absolute_y,
        "ADC",
        AddressingMode::AbsoluteIndexedY,
        0x79
    );

    test_opcode!(sbc_immediate, "SBC", AddressingMode::Immediate, 0xE9);
    test_opcode!(sbc_absolute, "SBC", AddressingMode::Absolute, 0xED);
    test_opcode!(sbc_zero_page, "SBC", AddressingMode::ZeroPage, 0xE5);
    test_opcode!(
        sbc_zero_page_x,
        "SBC",
        AddressingMode::ZeroPageIndexedX,
        0xF5
    );
    test_opcode!(sbc_indirect_y, "SBC", AddressingMode::IndirectIndexed, 0xF1);

    test_opcode!(stx_absolute, "STX", AddressingMode::Absolute, 0x8E);
    test_opcode!(stx_zero_page, "STX", AddressingMode::ZeroPage, 0x86);

    test_opcode!(txs, "TXS", AddressingMode::Implied, 0x9A);

    test_opcode!(eor_immediate, "EOR", AddressingMode::Immediate, 0x49);

    test_opcode!(
        asl_accumulator,
        "ASL",
        AddressingMode::AccumulatorAddressing,
        0x0A
    );
    test_opcode!(asl_zero_page, "ASL", AddressingMode::ZeroPage, 0x06);
    test_opcode!(
        asl_zero_page_x,
        "ASL",
        AddressingMode::ZeroPageIndexedX,
        0x16
    );
    test_opcode!(asl_absolute, "ASL", AddressingMode::Absolute, 0x0e);
    test_opcode!(
        asl_absolute_x,
        "ASL",
        AddressingMode::AbsoluteIndexedX,
        0x1e
    );

    test_opcode!(sty_absolute, "STY", AddressingMode::Absolute, 0x8C);
    test_opcode!(
        sty_zero_page_x,
        "STY",
        AddressingMode::ZeroPageIndexedX,
        0x94
    );
    test_opcode!(sty_zero_page, "STY", AddressingMode::ZeroPage, 0x84);

    test_opcode!(sta_absolute, "STA", AddressingMode::Absolute, 0x8D);
    test_opcode!(sta_zero_page, "STA", AddressingMode::ZeroPage, 0x85);
    test_opcode!(
        sta_zero_page_x,
        "STA",
        AddressingMode::ZeroPageIndexedX,
        0x95
    );
    test_opcode!(
        stx_zero_page_y,
        "STX",
        AddressingMode::ZeroPageIndexedY,
        0x96
    );
    test_opcode!(
        sta_absolute_x,
        "STA",
        AddressingMode::AbsoluteIndexedX,
        0x9D
    );
    test_opcode!(
        sta_indirect_x,
        "STA",
        AddressingMode::IndirectIndexedX,
        0x81
    );
    test_opcode!(sta_indirect_y, "STA", AddressingMode::IndirectIndexed, 0x91);
    test_opcode!(
        sta_absolute_y,
        "STA",
        AddressingMode::AbsoluteIndexedY,
        0x99
    );

    test_opcode!(bne, "BNE", AddressingMode::RelativeAddressing, 0xD0);
    test_opcode!(bvc, "BVC", AddressingMode::RelativeAddressing, 0x50);
    test_opcode!(bvs, "BVS", AddressingMode::RelativeAddressing, 0x70);
    test_opcode!(beq, "BEQ", AddressingMode::RelativeAddressing, 0xF0);
    test_opcode!(bmi, "BMI", AddressingMode::RelativeAddressing, 0x30);
    test_opcode!(bpl, "BPL", AddressingMode::RelativeAddressing, 0x10);
    test_opcode!(bcc, "BCC", AddressingMode::RelativeAddressing, 0x90);
    test_opcode!(bcs, "BCS", AddressingMode::RelativeAddressing, 0xB0);

    test_opcode!(jsr, "JSR", AddressingMode::Absolute, 0x20);
    test_opcode!(rts, "RTS", AddressingMode::Implied, 0x60);

    test_opcode!(ror_zero_page, "ROR", AddressingMode::ZeroPage, 0x66);
    test_opcode!(ror_absolute, "ROR", AddressingMode::Absolute, 0x6E);
    test_opcode!(
        ror_accumulator,
        "ROR",
        AddressingMode::AccumulatorAddressing,
        0x6A
    );

    test_opcode!(rol_zero_page, "ROL", AddressingMode::ZeroPage, 0x26);
    test_opcode!(
        rol_zero_page_x,
        "ROL",
        AddressingMode::ZeroPageIndexedX,
        0x36
    );
    test_opcode!(
        rol_absolute_x,
        "ROL",
        AddressingMode::AbsoluteIndexedX,
        0x3E
    );
    test_opcode!(rol_absolute, "ROL", AddressingMode::Absolute, 0x2E);
    test_opcode!(
        rol_accumulator,
        "ROL",
        AddressingMode::AccumulatorAddressing,
        0x2A
    );
}
