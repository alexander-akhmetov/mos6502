#[derive(PartialEq, Eq)]
pub enum AddressingMode {
    Immediate = 1,
    Absolute,
    ZeroPage,
    Implied,
    IndirectAbsolute,
    AbsoluteIndexed,
    ZeroPageIndexed,
    IndexedIndirect,
    IndirectIndexed,
    RelativeAddressing,
    AccumulatorAddressing,
}

pub struct Operation {
    pub name: &'static str,
    pub code: u8,
    pub length: u8,
    pub addressing: AddressingMode,
}

pub static OPERATIONS: &'static [Operation] = &[
    Operation {
        name: "BRK",
        code: 0x00,
        length: 1,
        addressing: AddressingMode::Immediate,
    },
    Operation {
        name: "LDA",
        code: 0xa9,
        length: 2,
        addressing: AddressingMode::Immediate,
    },
    Operation {
        name: "ADC",
        code: 0x69,
        length: 2,
        addressing: AddressingMode::Immediate,
    },
    Operation {
        name: "ADC",
        code: 0x60,
        length: 3,
        addressing: AddressingMode::Absolute,
    },
    Operation {
        name: "STA",
        code: 0x85,
        length: 2,
        addressing: AddressingMode::Absolute,
    },
];

pub fn by_code(code: u8) -> &'static Operation {
    let opcode = OPERATIONS.iter().find(|o| o.code == code);

    if let Some(opcode) = opcode {
        return opcode;
    } else {
        panic!("unknown operation code");
    }
}

pub fn by_name(name: &str, addressing: AddressingMode) -> &Operation {
    let opcode = OPERATIONS
        .iter()
        .find(|o| o.name == name && o.addressing == addressing);

    if let Some(opcode) = opcode {
        return opcode;
    } else {
        panic!("unknown operation name");
    }
}

#[cfg(test)]
mod tests {
    use crate::operation::{by_name, AddressingMode};

    macro_rules! test_opcode {
        ($test_name:ident, $name:expr, $mode:expr, $expected:expr) => {
            #[test]
            fn $test_name() {
                assert_eq!(by_name($name, $mode).code, $expected);
            }
        };
    }

    test_opcode!(brk_immediate, "BRK", AddressingMode::Immediate, 0x00);
    test_opcode!(lda_immediate, "LDA", AddressingMode::Immediate, 0xa9);
    test_opcode!(adc_immediate, "ADC", AddressingMode::Immediate, 0x69);
    test_opcode!(adc_absolute, "ADC", AddressingMode::Absolute, 0x60);
    test_opcode!(sta_absolute, "STA", AddressingMode::Absolute, 0x85);
}
