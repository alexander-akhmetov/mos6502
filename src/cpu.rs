use crate::mem::Memory;
use crate::operation;
use crate::utils;

/*
*/

const DEFAULT_FLAGS: u8 = 0b0011_0000;
const FLAG_NEGATIVE: u8 = 0b1000_0000;
const FLAG_OVERFLOW: u8 = 0b0100_0000;
const FLAG_B: u8 = 0b0001_0000;
const FLAG_DECIMAL: u8 = 0b0000_1000;
const FLAG_INTERRUPT: u8 = 0b0000_0100;
const FLAG_ZERO: u8 = 0b0000_0010;
const FLAG_CARRY: u8 = 0b0000_0001;

const STACK_BOTTOM: u16 = 0x0100;

#[derive(Default)]
pub struct CPU {
    // register A
    pub a: u8,
    // register X
    pub x: u8,
    // regiser Y
    pub y: u8,

    /*
    6502 status flags

    7 6 5 4 3 2 1 0
    ---------------
    N V s s D I Z C
    | | | | | | | |
    | | | | | | | +- 0 Carry
    | | | | | | +--- 1 Zero
    | | | | | +----- 2 Interrupt Disable
    | | | | +------- 3 Decimal
    | | + +--------- 4,5 Not used (B flag)
    | +------------- 6 Overflow
    +--------------- 7 Negative
    */
    pub p: u8,

    // stack pointer
    pub sp: u8,

    // program counter
    pub pc: u16,
    pub memory: Memory,
    pub cycle: usize,
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            a: 0,
            x: 0,
            y: 0,
            p: DEFAULT_FLAGS,
            sp: 0xff,
            pc: 0,
            memory: Memory::new(),
            cycle: 0,
        }
    }

    pub fn load(&mut self, buf: &[u8], addr: u16) {
        self.memory.load(addr, buf);
        self.pc = addr;
    }

    pub fn step(&mut self) -> bool {
        let operation = operation::by_code(self.memory.get(self.pc));
        info!(
            "[cpu] [PC:${:04X} SP:${:02X} STP:${:02X} A:${:02X} X:${:02X} Y:${:02X} P:{:08b}] (op:{} code:${:02X} length:{} data:{:X?}) cycle:{}",
            self.pc,
            self.sp,
            self.memory.get(STACK_BOTTOM + 0xff),
            self.a,
            self.x,
            self.y,
            self.p,
            operation.name,
            operation.code,
            operation.length,
            &self.memory.bytes()
                [(self.pc) as usize..(self.pc + u16::from(operation.length)) as usize],
            self.cycle
        );

        self.pc += u16::from(operation.length);

        // cycles don't work yet
        self.cycle += 1;

        match operation.name {
            "BRK" => self.operation_brk(),
            "RTI" => self.operation_rti(),
            "LDA" => self.operation_lda(operation),
            "SBC" => self.operation_sbc(operation),
            "LDX" => self.operation_ldx(operation),
            "EOR" => self.operation_eor(operation),
            "AND" => self.operation_and(operation),
            "ORA" => self.operation_ora(operation),
            "LDY" => self.operation_ldy(operation),
            "ADC" => self.operation_adc(operation),
            "STA" => self.operation_sta(operation),
            "STX" => self.operation_stx(operation),
            "STY" => self.operation_sty(operation),
            "INC" => self.operation_inc(operation),
            "DEC" => self.operation_dec(operation),
            "INX" => self.operation_inx(),
            "INY" => self.operation_iny(),
            "DEX" => self.operation_dex(),
            "DEY" => self.operation_dey(),
            "TAX" => self.operation_tax(),
            "BNE" => self.operation_bne(operation),
            "BEQ" => self.operation_beq(operation),
            "BCC" => self.operation_bcc(operation),
            "BCS" => self.operation_bcs(operation),
            "BVC" => self.operation_bvc(operation),
            "BVS" => self.operation_bvs(operation),
            "BPL" => self.operation_bpl(operation),
            "BMI" => self.operation_bmi(operation),
            "CMP" => self.operation_cmp(operation),
            "CPX" => self.operation_cpx(operation),
            "CPY" => self.operation_cpy(operation),
            "CLC" => self.operation_clc(),
            "CLD" => self.operation_cld(),
            "CLI" => self.operation_cli(),
            "CLV" => self.operation_clv(),
            "SEC" => self.operation_sec(),
            "SED" => self.operation_sed(),
            "SEI" => self.operation_sei(),
            "JMP" => self.operation_jmp(operation),
            "JSR" => self.operation_jsr(operation),
            "RTS" => self.operation_rts(),
            "LSR" => self.operation_lsr(operation),
            "PHA" => self.operation_pha(),
            "PLA" => self.operation_pla(),
            "TXA" => self.operation_txa(),
            "TXS" => self.operation_txs(),
            "TSX" => self.operation_tsx(),
            "PLP" => self.operation_plp(),
            "PHP" => self.operation_php(),
            "TYA" => self.operation_tya(),
            "TAY" => self.operation_tay(),
            "ROR" => self.operation_ror(operation),
            "ROL" => self.operation_rol(operation),
            "BIT" => self.operation_bit(operation),
            "ASL" => self.operation_asl(operation),
            "NOP" => {}

            _ => {
                error!("unknown operation: code=0x{:X}", operation.code);
                panic!("unknown operation");
            }
        };

        false
    }
    pub fn run(&mut self) {
        while !self.step() {}
    }

    fn operation_clc(&mut self) {
        self.clear_carry();
    }

    fn operation_cli(&mut self) {
        self.clear_interrupt();
    }

    fn operation_clv(&mut self) {
        self.clear_overflow();
    }

    fn operation_cld(&mut self) {
        self.clear_decimal();
    }

    fn operation_sec(&mut self) {
        self.set_carry();
    }

    fn operation_sed(&mut self) {
        self.set_decimal();
    }

    fn operation_sei(&mut self) {
        self.set_interrupt();
    }

    fn operation_tax(&mut self) {
        // Transfer Accumulator to Index X
        // A -> X
        self.x = self.a;

        self.set_zero_if_needed(self.x);
        self.set_negative_if_needed(self.x);
    }

    fn operation_dex(&mut self) {
        // Decrement X by one
        // X = X - 1
        let (result, _) = self.x.overflowing_sub(1);
        self.x = result;

        self.set_zero_if_needed(self.x);
        self.set_negative_if_needed(self.x);
    }

    fn operation_dey(&mut self) {
        // Decrement Y by one
        // Y = Y - 1
        let (result, _) = self.y.overflowing_sub(1);
        self.y = result;

        self.set_zero_if_needed(self.y);
        self.set_negative_if_needed(self.y);
    }

    fn operation_brk(&mut self) {
        // Break
        // set B flag (0b0001_0000)
        self.set_b();
        // push return address (it's already incremented by 1, so we increment it only by 1)
        self.stack_push_word(self.pc + 1);
        self.stack_push(self.p);

        // now set interrupt flag and go to ISR
        self.set_interrupt();
        let addr = utils::little_endian_to_u16(self.memory.get(0xFFFF), self.memory.get(0xFFFE));
        self.pc = addr;
    }

    fn operation_rti(&mut self) {
        // Return from interrupt
        self.p = self.stack_pop();
        self.pc = self.stack_pop_word();
    }

    fn set_b(&mut self) {
        self.p |= FLAG_B;
    }

    fn operation_inc(&mut self, operation: &operation::Operation) {
        // Increment memory by 1
        // M = M + 1
        let addr = self.get_operand_address(operation);
        let value = self.memory.get(addr);
        let (result, _) = value.overflowing_add(1);
        self.memory.set(addr, result);

        self.set_zero_if_needed(result);
        self.set_negative_if_needed(result);
    }

    fn operation_dec(&mut self, op: &operation::Operation) {
        // Decrement memory by 1
        // M = M - 1
        let addr = self.get_operand_address(op);
        let value = self.memory.get(addr);
        let (result, _) = value.overflowing_sub(1);
        self.memory.set(addr, result);

        self.set_zero_if_needed(result);
        self.set_negative_if_needed(result);
    }

    fn operation_inx(&mut self) {
        // Increment X by 1
        // X = X + 1
        let (result, _) = self.x.overflowing_add(1);
        self.x = result;

        self.set_negative_if_needed(self.x);
        self.set_zero_if_needed(self.x);
    }

    fn operation_iny(&mut self) {
        // Increment Y by 1
        // Y = Y + 1
        let (result, _) = self.y.overflowing_add(1);
        self.y = result;

        self.set_negative_if_needed(self.y);
        self.set_zero_if_needed(self.y);
    }

    fn operation_ora(&mut self, op: &operation::Operation) {
        // OR memory with accumulator
        // A = M OR A
        self.a |= self.get_operand(op);

        self.set_negative_if_needed(self.a);
        self.set_zero_if_needed(self.a);
    }

    fn operation_and(&mut self, op: &operation::Operation) {
        // AND memory with accumulator
        // A = M AND A
        self.a &= self.get_operand(op);

        self.set_negative_if_needed(self.a);
        self.set_zero_if_needed(self.a);
    }

    fn operation_eor(&mut self, op: &operation::Operation) {
        // Exclusive-OR memory with accumulator
        // A = A XOR M
        self.a ^= self.get_operand(op);

        self.set_negative_if_needed(self.a);
        self.set_zero_if_needed(self.a);
    }

    fn operation_ldx(&mut self, operation: &operation::Operation) {
        // Load X from memory
        // M -> X
        self.x = self.get_operand(operation);

        self.set_negative_if_needed(self.x);
        self.set_zero_if_needed(self.x);
    }

    fn operation_ldy(&mut self, operation: &operation::Operation) {
        // Load Y from memory
        // M -> Y
        self.y = self.get_operand(operation);

        self.set_negative_if_needed(self.y);
        self.set_zero_if_needed(self.y);
    }

    fn operation_lda(&mut self, op: &operation::Operation) {
        // Load A from memory
        // M -> A
        self.a = self.get_operand(op);

        self.set_negative_if_needed(self.a);
        self.set_zero_if_needed(self.a);
    }

    fn operation_sbc(&mut self, op: &operation::Operation) {
        // Subtract memory from accumulator with borrow
        // SBC uses the complement of the carry
        let operand = self.get_operand(op);
        let carry = i16::from(1 - self.get_carry());
        let original_a = self.a;
        let result = i16::from(original_a) - carry - i16::from(operand);

        self.a = result as u8;

        self.clear_overflow();
        if original_a as u8 >> 7 != self.a >> 7 {
            self.set_overflow();
        }

        // clear carry if overflow in the bit 7
        if (result as u16) < 256 {
            self.set_carry();
        } else {
            self.clear_carry();
        }

        // http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        self.clear_overflow();
        if ((255 - operand) ^ self.a) & (original_a ^ self.a) & 0x80 != 0 {
            self.set_overflow();
        };

        self.set_negative_if_needed(self.a);
        self.set_zero_if_needed(self.a);
    }

    fn operation_adc(&mut self, op: &operation::Operation) {
        // Add memory to accumulator with carry
        let operand = u16::from(self.get_operand(op));
        let original_a = u16::from(self.a);
        let result = original_a + operand + u16::from(self.get_carry());
        self.a = result as u8;

        // unsigned carry
        self.clear_carry();
        if result > 255 {
            self.set_carry();
        };

        // http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        self.clear_overflow();
        if (operand ^ result) & (original_a ^ result) & 0x80 != 0 {
            self.set_overflow();
        };

        self.set_negative_if_needed(self.a);
        self.set_zero_if_needed(self.a);
    }

    fn operation_beq(&mut self, operation: &operation::Operation) {
        // Branch on result zero (Z == 1)
        if self.get_zero() == FLAG_ZERO {
            self.branch(operation);
        }
    }

    fn operation_bne(&mut self, operation: &operation::Operation) {
        // Branch on result not zero (Z == 0)
        if self.get_zero() != FLAG_ZERO {
            self.branch(operation);
        }
    }

    fn branch(&mut self, operation: &operation::Operation) {
        let offset = self.get_operand(operation);
        if offset > 127 {
            debug!("branch jump, offset=-{}", (256 - offset as isize));
            self.pc -= 256 - u16::from(offset);
        } else {
            debug!("branch jump, offset={}", offset);
            self.pc += u16::from(offset);
        }
    }

    fn operation_bcc(&mut self, operation: &operation::Operation) {
        // Branch on carry clear (C == 0)
        if self.get_carry() == 0 {
            self.branch(operation);
        }
    }

    fn operation_bvs(&mut self, operation: &operation::Operation) {
        // Branch on overflow set (V == 1)
        if self.get_overflow() != 0 {
            self.branch(operation);
        }
    }

    fn operation_bvc(&mut self, operation: &operation::Operation) {
        // Branch on overflow clear (V == 0)
        if self.get_overflow() == 0 {
            self.branch(operation);
        }
    }

    fn operation_bcs(&mut self, operation: &operation::Operation) {
        // Branch on carry set (C == 1)
        if self.get_carry() != 0 {
            self.branch(operation);
        }
    }

    fn operation_bmi(&mut self, operation: &operation::Operation) {
        // Branch on minus (N == 1)
        if self.get_negative() == FLAG_NEGATIVE {
            self.branch(operation);
        }
    }

    fn operation_bpl(&mut self, operation: &operation::Operation) {
        // Branch on plus (N == 0)
        if self.get_negative() == 0 {
            self.branch(operation);
        }
    }

    fn operation_cpy(&mut self, operation: &operation::Operation) {
        // Compare memory and Y
        let operand = self.get_operand(operation);
        self.compare(self.y, operand);
    }

    fn operation_jmp(&mut self, op: &operation::Operation) {
        // jump to a new address
        match op.addressing {
            operation::AddressingMode::IndirectAbsolute => {
                let addr = self.get_operand_address(op);
                self.pc =
                    utils::little_endian_to_u16(self.memory.get(addr + 1), self.memory.get(addr));
            }
            _ => self.pc = self.get_operand_address(op),
        }
    }

    fn operation_rts(&mut self) {
        // Return from subroutine
        self.pc = self.stack_pop_word() + 1;
    }

    fn operation_jsr(&mut self, operation: &operation::Operation) {
        // Jump to subroutine
        self.stack_push_word(self.pc - 1);
        self.pc = self.get_operand_address(operation);
    }

    fn operation_asl(&mut self, op: &operation::Operation) {
        // Shift left one bit
        // C <- [76543210] <- 0
        let mut operand = self.get_operand(op);

        if operand >> 7 == 1 {
            self.set_carry(); // bit 7 to carry
        } else {
            self.clear_carry()
        }

        operand <<= 1;

        self.set_zero_if_needed(operand);
        self.set_negative_if_needed(operand);

        match op.addressing {
            operation::AddressingMode::AccumulatorAddressing => self.a = operand,
            _ => {
                let addr = self.get_operand_address(op);
                self.memory.set(addr, operand);
            }
        }
    }

    fn operation_bit(&mut self, op: &operation::Operation) {
        // Test bits in ьemory with accumulator
        // A AND M, M7 -> N, M6 -> V
        let operand = self.get_operand(op);

        self.set_zero_if_needed(self.a & operand);

        self.clear_overflow();
        if operand & 0b0100_0000 != 0 {
            self.set_overflow();
        }
        self.set_negative_if_needed(operand);
    }

    fn operation_rol(&mut self, op: &operation::Operation) {
        // rotate left, carry to 0 bit and 7 bit to carry
        // C <- [76543210] <- C
        let mut value = self.get_operand(op);

        let original_carry = matches!(self.get_carry(), FLAG_CARRY);
        if (value >> 7) != 0 {
            self.set_carry();
        } else {
            self.clear_carry();
        }

        value = (value << 1) as u8;

        if original_carry {
            value |= 0b0000_0001;
        };

        self.set_zero_if_needed(value);
        self.set_negative_if_needed(value);

        match op.addressing {
            operation::AddressingMode::AccumulatorAddressing => self.a = value,
            _ => {
                let addr = self.get_operand_address(op);
                self.memory.set(addr, value);
            }
        }
    }

    fn operation_ror(&mut self, op: &operation::Operation) {
        // rotate right, carry to 7 bit and 0 bit to carry
        // C -> [76543210] -> C
        let mut value = self.get_operand(op);

        let original_carry = matches!(self.get_carry(), FLAG_CARRY);
        if (value & 0x01) != 0 {
            self.set_carry();
        } else {
            self.clear_carry();
        }

        value = (value >> 1) as u8;

        if original_carry {
            value |= 0b1000_0000;
        };

        self.set_zero_if_needed(value);
        self.set_negative_if_needed(value);

        match op.addressing {
            operation::AddressingMode::AccumulatorAddressing => self.a = value,
            _ => {
                let addr = self.get_operand_address(op);
                self.memory.set(addr, value)
            }
        }
    }

    fn operation_lsr(&mut self, op: &operation::Operation) {
        // Shift one bit right
        // 0 -> [76543210] -> C
        let operand = self.get_operand(op);
        let zero_bit = (operand << 7) > 0;
        let value = operand >> 1;

        if zero_bit {
            self.set_carry();
        } else {
            self.clear_carry();
        }

        self.set_zero_if_needed(value);
        self.set_negative_if_needed(value);

        match op.addressing {
            operation::AddressingMode::AccumulatorAddressing => self.a = value,
            _ => {
                let addr = self.get_operand_address(op);
                self.memory.set(addr, value);
            }
        }
    }

    fn operation_txa(&mut self) {
        // X -> A
        self.a = self.x;
        self.set_negative_if_needed(self.a);
        self.set_zero_if_needed(self.a);
    }

    fn operation_tay(&mut self) {
        // A -> Y
        self.y = self.a;
        self.set_negative_if_needed(self.y);
        self.set_zero_if_needed(self.y);
    }

    fn operation_txs(&mut self) {
        self.sp = self.x;
    }

    fn operation_tsx(&mut self) {
        // SP -> X
        self.x = self.sp;
        self.set_negative_if_needed(self.x);
        self.set_zero_if_needed(self.x);
    }

    fn operation_php(&mut self) {
        // Push P to stack
        self.stack_push(self.p);
    }

    fn operation_plp(&mut self) {
        // Pull P from stack
        self.p = self.stack_pop() | 0b0011_0000;
    }

    fn operation_tya(&mut self) {
        // Y -> A
        self.a = self.y;
        self.set_negative_if_needed(self.y);
        self.set_zero_if_needed(self.y);
    }

    fn operation_pha(&mut self) {
        // Push A to stack
        self.stack_push(self.a);
    }

    fn stack_push_word(&mut self, value: u16) {
        let higher = ((value >> 8) & 0xFF) as u8;
        self.stack_push(higher);

        let lower = (value & 0xFF) as u8;
        self.stack_push(lower);
    }

    fn stack_pop_word(&mut self) -> u16 {
        let lower = self.stack_pop();
        let higher = self.stack_pop();

        (u16::from(higher) << 8) | u16::from(lower)
    }

    fn stack_push(&mut self, value: u8) {
        self.memory.set(STACK_BOTTOM + u16::from(self.sp), value);
        self.sp = ((u16::from(self.sp) - 1) & 0xFF) as u8;
    }

    fn operation_pla(&mut self) {
        // Pull A from stack
        self.a = self.stack_pop();
        self.set_negative_if_needed(self.a);
        self.set_zero_if_needed(self.a);
    }

    fn stack_pop(&mut self) -> u8 {
        self.sp = ((i16::from(self.sp) + 1) & 0xFF) as u8;
        self.memory.get(STACK_BOTTOM + u16::from(self.sp))
    }

    fn operation_cmp(&mut self, operation: &operation::Operation) {
        // Compare memory and A
        let operand = self.get_operand(operation);
        self.compare(self.a, operand);
    }

    fn operation_cpx(&mut self, operation: &operation::Operation) {
        // Compare memory and X
        let operand = self.get_operand(operation);
        self.compare(self.x, operand);
    }

    fn compare(&mut self, left_operand: u8, right_operand: u8) {
        self.clear_zero();
        self.clear_carry();
        self.clear_negative();

        if left_operand == right_operand {
            self.set_zero();
            self.set_carry();
        }

        if left_operand > right_operand {
            self.set_carry();
        }

        let result = (i16::from(left_operand) - i16::from(right_operand)) as u8;
        self.set_negative_if_needed(result);
    }

    fn get_overflow(&self) -> u8 {
        self.p & FLAG_OVERFLOW
    }

    fn set_overflow(&mut self) {
        self.p |= FLAG_OVERFLOW;
    }

    fn clear_overflow(&mut self) {
        self.p &= !FLAG_OVERFLOW;
    }

    fn set_decimal(&mut self) {
        self.p |= FLAG_DECIMAL;
    }

    fn set_interrupt(&mut self) {
        self.p |= FLAG_INTERRUPT;
    }

    fn set_carry(&mut self) {
        self.p |= FLAG_CARRY;
    }

    fn clear_carry(&mut self) {
        self.p &= !FLAG_CARRY;
    }

    fn get_negative(&self) -> u8 {
        self.p & FLAG_NEGATIVE
    }

    fn get_carry(&self) -> u8 {
        self.p & FLAG_CARRY
    }

    fn clear_decimal(&mut self) {
        self.p &= !FLAG_DECIMAL;
    }

    fn clear_interrupt(&mut self) {
        self.p &= !FLAG_INTERRUPT;
    }

    fn get_zero(&self) -> u8 {
        self.p & FLAG_ZERO
    }

    fn set_zero_if_needed(&mut self, result: u8) {
        self.clear_zero();
        if result == 0 {
            self.set_zero();
        }
    }

    fn set_zero(&mut self) {
        self.p |= FLAG_ZERO;
    }

    fn clear_zero(&mut self) {
        self.p &= !FLAG_ZERO;
    }

    fn set_negative_if_needed(&mut self, result: u8) {
        self.clear_negative();
        if result > 127 {
            self.set_negative();
        }
    }

    fn set_negative(&mut self) {
        self.p |= FLAG_NEGATIVE;
    }

    fn clear_negative(&mut self) {
        self.p &= !FLAG_NEGATIVE;
    }

    fn operation_sty(&mut self, operation: &operation::Operation) {
        let addr = self.get_operand_address(operation);
        self.memory.set(addr, self.y);
    }

    fn operation_stx(&mut self, operation: &operation::Operation) {
        let addr = self.get_operand_address(operation);
        self.memory.set(addr, self.x);
    }

    fn operation_sta(&mut self, op: &operation::Operation) {
        let addr = self.get_operand_address(op);
        self.memory.set(addr, self.a);
    }

    fn get_operand(&mut self, op: &operation::Operation) -> u8 {
        match op.addressing {
            operation::AddressingMode::AccumulatorAddressing => self.a,
            _ => {
                let addr = self.get_operand_address(op);
                self.memory.get(addr)
            }
        }
    }

    fn get_operand_address(&mut self, op: &operation::Operation) -> u16 {
        match op.addressing {
            operation::AddressingMode::Immediate => self.pc - 1,
            operation::AddressingMode::ZeroPage => u16::from(self.next_byte_number()) & 0xFF,
            operation::AddressingMode::ZeroPageIndexedX => {
                (u16::from(self.next_byte_number()) + u16::from(self.x)) & 0xFF
            }
            operation::AddressingMode::ZeroPageIndexedY => {
                (u16::from(self.next_byte_number()) + u16::from(self.y)) & 0xFF
            }
            operation::AddressingMode::Absolute => self.next_word_in_memory(),
            operation::AddressingMode::IndirectAbsolute => self.next_word_in_memory(),
            operation::AddressingMode::IndirectIndexedX => {
                let addr = (u16::from(self.next_byte_number()) + u16::from(self.x)) & 0xFF;
                utils::little_endian_to_u16(self.memory.get(addr + 1), self.memory.get(addr))
            }
            operation::AddressingMode::IndirectIndexed => {
                let addr = u16::from(self.next_byte_number());
                let value =
                    utils::little_endian_to_u16(self.memory.get(addr + 1), self.memory.get(addr));
                value + u16::from(self.y)
            }
            operation::AddressingMode::RelativeAddressing => self.pc - 1,
            operation::AddressingMode::AbsoluteIndexedY => {
                self.next_word_in_memory() + u16::from(self.y)
            }
            operation::AddressingMode::AbsoluteIndexedX => {
                self.next_word_in_memory() + u16::from(self.x)
            }
            _ => {
                error!("unknown addressing mode for operation code=0x{:X}", op.code);
                panic!("unknown addressing mode");
            }
        }
    }

    fn next_byte_number(&mut self) -> u8 {
        self.memory.get(self.pc - 1)
    }

    fn next_word_in_memory(&mut self) -> u16 {
        // returns next two bytes (self.pc - 1, self.pc - 2) as one u16
        // (converts from little endian)
        utils::little_endian_to_u16(self.memory.get(self.pc - 1), self.memory.get(self.pc - 2))
    }
}

#[cfg(test)]
mod tests {
    use crate::cpu::*;
    use crate::operation;

    const STACK_TOP: u16 = 0x01ff;

    #[test]
    fn new() {
        let c = CPU::new();

        assert_eq!(c.a, 0);
        assert_eq!(c.x, 0);
        assert_eq!(c.y, 0);
        assert_eq!(c.p, DEFAULT_FLAGS);
        assert_eq!(c.pc, 0);
        assert_eq!(c.sp, 0xff);
        assert_eq!(&c.memory.bytes()[..], &[0; 64 * 1024][..]);
    }

    #[test]
    fn load() {
        // test when it loads a three bytes dump at a specific location
        let mut cpu = CPU::new();

        let addr = 32768;
        let bytes = [1; 3];

        assert_eq!(&cpu.memory.bytes()[..], &[0; 64 * 1024][..]);
        cpu.load(&bytes, addr);

        let exp_memory = [0, 1, 1, 1, 0];
        assert_eq!(&cpu.memory.bytes()[32767..32772], exp_memory);

        assert_eq!(cpu.pc, addr);
    }

    #[test]
    #[should_panic]
    fn load_data_too_big() {
        let mut cpu = CPU::new();
        cpu.load(&[1, 2, 3], 65535);
    }

    #[test]
    fn clear_carry() {
        let mut cpu = CPU::new();

        cpu.set_carry();
        assert_eq!(cpu.p & 0b0000_0001, 1);

        cpu.clear_carry();
        assert_eq!(cpu.p & 0b0000_0001, 0);
    }

    #[test]
    fn operation_sed() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("SED", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.p & FLAG_DECIMAL, 0);

        cpu.step();

        assert_eq!(cpu.p & FLAG_DECIMAL, FLAG_DECIMAL);
    }

    #[test]
    fn operation_sei() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("SEI", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.p & FLAG_INTERRUPT, 0);

        cpu.step();

        assert_eq!(cpu.p & FLAG_INTERRUPT, FLAG_INTERRUPT);
    }

    #[test]
    fn operation_sec() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("SEC", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.p & FLAG_CARRY, 0);

        cpu.step();

        assert_eq!(cpu.p & FLAG_CARRY, FLAG_CARRY);
    }

    #[test]
    fn clear_overflow() {
        let mut cpu = CPU::new();

        cpu.set_overflow();
        assert_eq!(cpu.p & FLAG_OVERFLOW, FLAG_OVERFLOW);

        cpu.clear_overflow();
        assert_eq!(cpu.p & FLAG_OVERFLOW, 0);
    }

    #[test]
    fn clear_interrupt() {
        let mut cpu = CPU::new();

        cpu.p = FLAG_INTERRUPT;
        cpu.clear_interrupt();
        assert_eq!(cpu.p & FLAG_INTERRUPT, 0);
    }

    #[test]
    fn clear_decimal() {
        let mut cpu = CPU::new();

        cpu.p = FLAG_DECIMAL;
        cpu.clear_decimal();
        assert_eq!(cpu.p & FLAG_DECIMAL, 0);
    }

    // ############# LDA

    #[test]
    fn operation_lda() {
        let mut cpu = CPU::new();

        let program: [u8; 3] = [
            operation::by_name_and_addressing("LDA", operation::AddressingMode::Immediate).code,
            10,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.a, 0);

        cpu.step();

        assert_eq!(cpu.a, 10);
    }

    #[test]
    fn operation_lda_zero_page() {
        let mut cpu = CPU::new();

        let program: [u8; 3] = [
            operation::by_name_and_addressing("LDA", operation::AddressingMode::ZeroPage).code,
            10,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);
        cpu.memory.set(10, 55);

        assert_eq!(cpu.a, 0);

        cpu.step();

        assert_eq!(cpu.a, 55);
    }

    #[test]
    fn operation_lda_absolute() {
        let mut cpu = CPU::new();

        let program: [u8; 4] = [
            operation::by_name_and_addressing("LDA", operation::AddressingMode::Absolute).code,
            0x00,
            0xff,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);
        cpu.memory.set(0xff00, 99);

        assert_eq!(cpu.a, 0);

        cpu.step();

        assert_eq!(cpu.a, 99);
    }

    #[test]
    fn operation_lda_absolute_indexed_y() {
        let mut cpu = CPU::new();
        cpu.y = 1;

        let program: [u8; 4] = [
            operation::by_name_and_addressing("LDA", operation::AddressingMode::AbsoluteIndexedY)
                .code,
            0x01,
            0xff,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);
        cpu.memory.set(0xff02, 21);

        assert_eq!(cpu.a, 0);

        cpu.step();

        assert_eq!(cpu.a, 21);
    }

    #[test]
    fn operation_lda_absolute_indexed_x() {
        let mut cpu = CPU::new();
        cpu.x = 3;

        let program: [u8; 3] = [
            operation::by_name_and_addressing("LDA", operation::AddressingMode::AbsoluteIndexedX)
                .code,
            0x02,
            0xff,
        ];
        cpu.load(&program, 0);
        cpu.memory.set(0xff05, 11);

        assert_eq!(cpu.a, 0);

        cpu.step();

        assert_eq!(cpu.a, 11);
    }
    // ############# LDX

    #[test]
    fn operation_ldx() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("LDX", operation::AddressingMode::Immediate).code,
            10,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.x, 0);

        cpu.step();

        assert_eq!(cpu.x, 10);
    }

    #[test]
    fn operation_ldx_zero_page() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("LDX", operation::AddressingMode::ZeroPage).code,
            10,
        ];
        cpu.load(&program, 0);
        cpu.memory.set(10, 55);

        assert_eq!(cpu.x, 0);

        cpu.step();

        assert_eq!(cpu.x, 55);
    }

    #[test]
    fn operation_ldx_absolute() {
        let mut cpu = CPU::new();

        let program: [u8; 3] = [
            operation::by_name_and_addressing("LDX", operation::AddressingMode::Absolute).code,
            0x00,
            0xff,
        ];
        cpu.load(&program, 0);
        cpu.memory.set(0xff00, 99);

        assert_eq!(cpu.x, 0);

        cpu.step();

        assert_eq!(cpu.x, 99);
    }

    // ############# ADC

    #[test]
    fn operation_adc_immediate_with_overflow() {
        let mut cpu = CPU::new();
        cpu.a = 255;

        let program: [u8; 2] = [
            operation::by_name_and_addressing("ADC", operation::AddressingMode::Immediate).code,
            1,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.a, 255);

        cpu.step();

        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.p, 0b00110011); // overflow flag
    }

    #[test]
    fn test_adc_for_overflow_and_carry() {
        // http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
        let experiments = [
            (80, 16, false, false),
            (80, 80, false, true),
            (80, 144, false, false),
            (80, 208, true, false),
            (208, 16, false, false),
            (208, 80, true, false),
            (208, 144, true, true),
            (208, 208, true, false),
        ];

        for (a, operand, carry, overflow) in experiments.iter() {
            let mut cpu = CPU::new();
            cpu.a = *a;

            let program: [u8; 2] = [
                operation::by_name_and_addressing("ADC", operation::AddressingMode::Immediate).code,
                *operand,
            ];
            cpu.load(&program, 0);
            cpu.step();

            if *carry {
                assert_eq!(cpu.p & FLAG_CARRY, 0b0000_0001);
            } else {
                assert_eq!(cpu.p & FLAG_CARRY, 0b0000_0000);
            }
            if *overflow {
                assert_eq!(cpu.p & FLAG_OVERFLOW, 0b0100_0000);
            } else {
                assert_eq!(cpu.p & FLAG_OVERFLOW, 0b0000_0000);
            }
        }
    }

    #[test]
    fn operation_adc_add_two_positive_numbers_and_get_negative_result_must_set_overflow() {
        let mut cpu = CPU::new();
        cpu.a = 80;

        let program: [u8; 2] = [
            operation::by_name_and_addressing("ADC", operation::AddressingMode::Immediate).code,
            80,
        ];
        cpu.load(&program, 0);

        cpu.step();

        assert_eq!(cpu.a, 160);
        assert_eq!(cpu.p, 0b1111_0000);
    }

    #[test]
    fn operation_adc_immediate() {
        let mut cpu = CPU::new();
        cpu.a = 10;

        let program: [u8; 2] = [
            operation::by_name_and_addressing("ADC", operation::AddressingMode::Immediate).code,
            15,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.a, 10);

        cpu.step();

        assert_eq!(cpu.a, 25);
        assert_eq!(cpu.p, 0b00110000); // no overflow
    }

    #[test]
    fn operation_adc_absolute() {
        let mut cpu = CPU::new();
        cpu.a = 10;

        let program: [u8; 3] = [
            operation::by_name_and_addressing("ADC", operation::AddressingMode::Absolute).code,
            0xff,
            0xaa,
        ];
        cpu.load(&program, 0);
        cpu.memory.set(43775, 15); // 0xaaff

        assert_eq!(cpu.a, 10);

        cpu.step();

        assert_eq!(cpu.a, 25);
    }

    #[test]
    fn operation_stx_zeropage() {
        let mut cpu = CPU::new();
        cpu.x = 5;

        let program: [u8; 3] = [
            operation::by_name_and_addressing("STX", operation::AddressingMode::ZeroPage).code,
            22,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.memory.get(22), 0);

        cpu.step();

        assert_eq!(cpu.memory.get(22), 5);
    }

    #[test]
    fn operation_stx() {
        let mut cpu = CPU::new();
        cpu.x = 5;

        let program: [u8; 4] = [
            operation::by_name_and_addressing("STX", operation::AddressingMode::Absolute).code,
            12,
            0,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.memory.get(12), 0);

        cpu.step();

        assert_eq!(cpu.memory.get(12), 5);
    }

    #[test]
    fn operation_sta() {
        let mut cpu = CPU::new();
        cpu.a = 10;

        let program: [u8; 4] = [
            operation::by_name_and_addressing("STA", operation::AddressingMode::Absolute).code,
            0x30,
            0x01,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.memory.get(0x0130), 0);

        cpu.step();

        assert_eq!(cpu.memory.get(0x0130), cpu.a);
    }

    #[test]
    fn operation_sta_indirect_indexed_x() {
        let mut cpu = CPU::new();
        cpu.a = 10;
        cpu.x = 5;

        // STA ($0010,X)
        let program: [u8; 2] = [
            operation::by_name_and_addressing("STA", operation::AddressingMode::IndirectIndexedX)
                .code,
            0x2,
        ];
        cpu.memory.set(0x7, 0xCC);
        cpu.memory.set(0x8, 0xDD);
        cpu.load(&program, 0);

        assert_eq!(cpu.memory.get(0xDDCC), 0);

        cpu.step();

        assert_eq!(cpu.memory.get(0xDDCC), 10);
    }

    #[test]
    fn operation_sta_zero_page_x_with_overflow() {
        let mut cpu = CPU::new();
        cpu.a = 0xEE;
        cpu.x = 0xCC;

        let program: [u8; 2] = [
            operation::by_name_and_addressing("STA", operation::AddressingMode::ZeroPageIndexedX)
                .code,
            0xFF,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.memory.get(0xCB), 0);

        cpu.step();

        assert_eq!(cpu.memory.get(0xCB), 0xEE);
    }

    #[test]
    fn operation_sta_zero_page_x() {
        let mut cpu = CPU::new();
        cpu.a = 0xEE;
        cpu.x = 2;

        let program: [u8; 2] = [
            operation::by_name_and_addressing("STA", operation::AddressingMode::ZeroPageIndexedX)
                .code,
            0x5,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.memory.get(0x7), 0);

        cpu.step();

        assert_eq!(cpu.memory.get(0x7), 0xEE);
    }

    #[test]
    fn operation_sta_indexed_indirect_y() {
        let mut cpu = CPU::new();
        cpu.a = 0xFF;
        cpu.y = 0x03;

        // STA ($10E0,)Y
        // memory at location $00E0 holds a number 0xAA01
        // a must be stored at the address (0xAA01 + cpu.y)
        let program: [u8; 2] = [
            operation::by_name_and_addressing("STA", operation::AddressingMode::IndirectIndexed)
                .code,
            0x10,
        ];
        cpu.memory.set(0x10, 0x01);
        cpu.memory.set(0x11, 0xAA);
        cpu.load(&program, 0);

        assert_eq!(cpu.memory.get(0xAA04), 0);

        cpu.step();

        assert_eq!(cpu.memory.get(0xAA04), 0xFF);
        assert_eq!(cpu.get_negative(), 0);
    }

    #[test]
    fn operation_sta_absolute_y() {
        let mut cpu = CPU::new();
        cpu.a = 0xEE;
        cpu.y = 2;

        let program: [u8; 3] = [
            operation::by_name_and_addressing("STA", operation::AddressingMode::AbsoluteIndexedY)
                .code,
            0x2,
            0xE,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.memory.get(0x0E04), 0);

        cpu.step();

        assert_eq!(cpu.memory.get(0x0E04), 0xEE);
    }

    #[test]
    fn operation_sta_absolute() {
        let mut cpu = CPU::new();
        cpu.a = 0xEE;

        let program: [u8; 3] = [
            operation::by_name_and_addressing("STA", operation::AddressingMode::Absolute).code,
            0x2,
            0xE,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.memory.get(0x0E02), 0);

        cpu.step();

        assert_eq!(cpu.memory.get(0x0E02), 0xEE);
    }

    #[test]
    fn test_next_two_bytes() {
        let mut cpu = CPU::new();
        let bytes = [0, 0x0A, 0x0B];
        cpu.load(&bytes, 0);
        cpu.pc = 3;
        assert_eq!(cpu.next_word_in_memory(), 0x0B0A);
    }

    #[test]
    fn test_next_two_bytes_with_only_lower_byte() {
        let mut cpu = CPU::new();
        let bytes = [0, 10, 0];
        cpu.load(&bytes, 0);
        cpu.pc = 3;
        assert_eq!(cpu.next_word_in_memory(), 10);
    }

    #[test]
    fn test_simple_program() {
        // loads 100 from the memory
        // adds 7 to the a register
        // stores a register at address 0x000f
        let mut cpu = CPU::new();
        cpu.a = 10;

        let program: [u8; 7] = [
            operation::by_name_and_addressing("LDA", operation::AddressingMode::Immediate).code,
            100,
            operation::by_name_and_addressing("ADC", operation::AddressingMode::Immediate).code,
            7,
            operation::by_name_and_addressing("STA", operation::AddressingMode::Absolute).code,
            15,
            0,
        ];
        cpu.load(&program, 0);

        for _ in 0..3 {
            cpu.step();
        }

        assert_eq!(cpu.a, 107);
        assert_eq!(cpu.memory.get(15), 107);
    }

    #[test]
    fn test_dex_below_zero_sets_negative_flag() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("DEX", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);
        cpu.x = 0;

        cpu.step();
        assert_eq!(cpu.x, 0xFF);
        assert_eq!(cpu.p, 0b1011_0000);
    }

    #[test]
    fn test_dex_to_zero_sets_zero_flag() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("DEX", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);
        cpu.x = 1;

        cpu.step();
        assert_eq!(cpu.x, 0);
        assert_eq!(cpu.p, 0b0011_0010);
    }

    #[test]
    fn test_dex() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("DEX", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);
        cpu.x = 10;

        cpu.step();
        assert_eq!(cpu.x, 9);

        cpu.pc = 0;
        cpu.step();
        assert_eq!(cpu.x, 8);
    }

    #[test]
    fn operation_dey() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("DEY", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);
        cpu.y = 5;

        cpu.step();
        assert_eq!(cpu.y, 4);

        cpu.pc = 0;
        cpu.step();
        assert_eq!(cpu.y, 3);
    }

    #[test]
    fn operation_dey_below_zero_sets_negative_flag() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("DEY", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);
        cpu.y = 0;

        cpu.step();
        assert_eq!(cpu.y, 0xFF);
        assert_eq!(cpu.p, 0b1011_0000);
    }

    #[test]
    fn dey_to_zero_sets_zero_flag() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("DEY", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);
        cpu.y = 1;

        cpu.step();
        assert_eq!(cpu.y, 0);
        assert_eq!(cpu.p, 0b0011_0010);
    }

    #[test]
    fn operation_inc() {
        let mut cpu = CPU::new();

        let program: [u8; 3] = [
            operation::by_name_and_addressing("INC", operation::AddressingMode::ZeroPage).code,
            100,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);
        cpu.memory.set(100, 10);

        cpu.step();
        assert_eq!(cpu.memory.get(100), 11);

        cpu.pc = 0;
        cpu.step();
        assert_eq!(cpu.memory.get(100), 12);
    }

    #[test]
    fn operation_inx() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("INX", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.x, 0);
        cpu.step();
        assert_eq!(cpu.x, 1);

        cpu.pc = 0;
        cpu.step();
        assert_eq!(cpu.x, 2);
    }

    #[test]
    fn test_iny() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("INY", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0);

        assert_eq!(cpu.y, 0);
        cpu.step();
        assert_eq!(cpu.y, 1);

        cpu.pc = 0;
        cpu.step();
        assert_eq!(cpu.y, 2);
    }

    #[test]
    fn test_tax_sets_zero() {
        let mut cpu = CPU::new();

        let program: [u8; 1] =
            [operation::by_name_and_addressing("TAX", operation::AddressingMode::Implied).code];
        cpu.load(&program, 0);
        cpu.a = 0;
        cpu.x = 0xAA;

        cpu.step();

        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.x, 0);
        assert_eq!(cpu.p, DEFAULT_FLAGS | FLAG_ZERO);
    }

    #[test]
    fn test_tax() {
        let mut cpu = CPU::new();

        let program: [u8; 1] =
            [operation::by_name_and_addressing("TAX", operation::AddressingMode::Implied).code];
        cpu.load(&program, 0);
        cpu.a = 0xA;
        cpu.x = 0;

        cpu.step();

        assert_eq!(cpu.a, 0xA);
        assert_eq!(cpu.x, 0xA);
        assert_eq!(cpu.p, DEFAULT_FLAGS); // no negative flag
    }

    #[test]
    fn cmp_equals_immediate() {
        // L == R (L > 0; R > 0)
        let mut cpu = CPU::new();

        let program: [u8; 4] = [
            operation::by_name_and_addressing("LDA", operation::AddressingMode::Immediate).code,
            100,
            operation::by_name_and_addressing("CMP", operation::AddressingMode::Immediate).code,
            100,
        ];
        cpu.load(&program, 0);

        for _ in 0..2 {
            cpu.step();
        }

        assert_eq!(cpu.get_zero(), FLAG_ZERO);
    }

    #[test]
    fn cmp_equals_immediate_both_negative() {
        // L == R (L < 0; R < 0)
        let mut cpu = CPU::new();

        let program: [u8; 4] = [
            operation::by_name_and_addressing("LDA", operation::AddressingMode::Immediate).code,
            0xFE, // -1
            operation::by_name_and_addressing("CMP", operation::AddressingMode::Immediate).code,
            0xFE,
        ];
        cpu.load(&program, 0);

        cpu.step();
        cpu.step();

        assert_eq!(cpu.get_zero(), FLAG_ZERO);
        assert_eq!(cpu.get_negative(), 0);
        assert_eq!(cpu.get_carry(), FLAG_CARRY);
    }

    #[test]
    fn cmp_left_less_right_both_negative_immediate() {
        // L < R and (L < 0 && R < 0)
        let mut cpu = CPU::new();

        let program: [u8; 4] = [
            operation::by_name_and_addressing("LDA", operation::AddressingMode::Immediate).code,
            0xFE, // -2
            operation::by_name_and_addressing("CMP", operation::AddressingMode::Immediate).code,
            0xFF, // -1
        ];
        cpu.load(&program, 0);

        cpu.step();
        cpu.step();

        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_negative(), FLAG_NEGATIVE);
        assert_eq!(cpu.get_carry(), 0);
    }

    #[test]
    fn cmp_left_less_right_left_negative_immediate() {
        // L < R and (L < 0)
        let mut cpu = CPU::new();

        let program: [u8; 4] = [
            operation::by_name_and_addressing("LDA", operation::AddressingMode::Immediate).code,
            0xFE, // -2
            operation::by_name_and_addressing("CMP", operation::AddressingMode::Immediate).code,
            10,
        ];
        cpu.load(&program, 0);

        cpu.step();
        cpu.step();

        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_negative(), FLAG_NEGATIVE);
        assert_eq!(cpu.get_carry(), FLAG_CARRY); // L > R (unsigned)
    }

    #[test]
    fn cmp_left_greater_right_right_negative_immediate() {
        // L < R and (R < 0)
        let mut cpu = CPU::new();

        let program: [u8; 4] = [
            operation::by_name_and_addressing("LDA", operation::AddressingMode::Immediate).code,
            0xF,
            operation::by_name_and_addressing("CMP", operation::AddressingMode::Immediate).code,
            0xFE, // -2
        ];
        cpu.load(&program, 0);

        cpu.step();
        cpu.step();

        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_negative(), 0);
        assert_eq!(cpu.get_carry(), 0);
    }

    #[test]
    fn cmp_not_equals_immediate() {
        let mut cpu = CPU::new();

        let program: [u8; 4] = [
            operation::by_name_and_addressing("LDA", operation::AddressingMode::Immediate).code,
            55,
            operation::by_name_and_addressing("CMP", operation::AddressingMode::Immediate).code,
            99,
        ];
        cpu.load(&program, 0);

        cpu.step();
        cpu.step();

        assert_eq!(cpu.get_zero(), 0);
    }

    #[test]
    fn test_cpy_equals_immediate() {
        let mut cpu = CPU::new();

        let program: [u8; 4] = [
            operation::by_name_and_addressing("LDY", operation::AddressingMode::Immediate).code,
            1,
            operation::by_name_and_addressing("CPY", operation::AddressingMode::Immediate).code,
            1,
        ];
        cpu.load(&program, 0);

        cpu.step();
        cpu.step();

        assert_eq!(cpu.get_zero(), FLAG_ZERO);
    }

    #[test]
    fn test_cpy_not_equals_immediate() {
        let mut cpu = CPU::new();

        let program: [u8; 4] = [
            operation::by_name_and_addressing("LDY", operation::AddressingMode::Immediate).code,
            1,
            operation::by_name_and_addressing("CPY", operation::AddressingMode::Immediate).code,
            5,
        ];
        cpu.load(&program, 0);

        cpu.step();
        cpu.step();

        assert_eq!(cpu.get_zero(), 0);
    }

    #[test]
    fn test_cpx_equals_immediate() {
        let mut cpu = CPU::new();

        let program: [u8; 4] = [
            operation::by_name_and_addressing("LDX", operation::AddressingMode::Immediate).code,
            1,
            operation::by_name_and_addressing("CPX", operation::AddressingMode::Immediate).code,
            1,
        ];
        cpu.load(&program, 0);

        cpu.step();
        cpu.step();

        assert_eq!(cpu.get_zero(), FLAG_ZERO);
    }

    #[test]
    fn test_cpx_not_equals_immediate() {
        let mut cpu = CPU::new();

        let program: [u8; 4] = [
            operation::by_name_and_addressing("LDX", operation::AddressingMode::Immediate).code,
            1,
            operation::by_name_and_addressing("CPX", operation::AddressingMode::Immediate).code,
            5,
        ];
        cpu.load(&program, 0);

        cpu.step();
        cpu.step();

        assert_eq!(cpu.get_zero(), 0);
    }

    #[test]
    fn test_bne_positive_offset() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("BNE", operation::AddressingMode::RelativeAddressing)
                .code,
            0xA,
        ];
        cpu.load(&program, 0);
        cpu.step();

        assert_eq!(cpu.pc, 0xA + 2);
    }

    #[test]
    fn test_bne_negative_offset() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("BNE", operation::AddressingMode::RelativeAddressing)
                .code,
            250,
        ];
        cpu.load(&program, 10);
        cpu.step();

        assert_eq!(cpu.pc, 4 + 2); // 10 - (256 - 250)
    }

    #[test]
    fn test_bne_branching() {
        let mut cpu = CPU::new();

        let program: [u8; 7] = [
            operation::by_name_and_addressing("LDX", operation::AddressingMode::Immediate).code,
            8,
            operation::by_name_and_addressing("DEX", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("CPX", operation::AddressingMode::Immediate).code,
            3,
            operation::by_name_and_addressing("BNE", operation::AddressingMode::RelativeAddressing)
                .code,
            251,
        ];
        cpu.load(&program, 0);

        // 16: 1 - ldx + 15 - cycle
        for _ in 0..16 {
            cpu.step();
        }

        assert_eq!(cpu.x, 3);
        assert_eq!(cpu.pc, 7);
        assert_eq!(cpu.p, 0b0011_0011);
    }

    #[test]
    fn pha() {
        let mut cpu = CPU::new();
        let program: [u8; 4] = [
            operation::by_name_and_addressing("LDA", operation::AddressingMode::Immediate).code,
            5,
            operation::by_name_and_addressing("PHA", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("PHA", operation::AddressingMode::Implied).code,
        ];

        cpu.load(&program, 0x800);

        for _ in 0..3 {
            cpu.step();
        }

        assert_eq!(cpu.a, 5);
        assert_eq!(cpu.sp, 0xfd);
        assert_eq!(cpu.memory.get(STACK_TOP), 5);
        assert_eq!(cpu.memory.get(STACK_TOP - 1), 5);

        let zero: [u8; 253] = [0; 253];
        assert_eq!(
            &cpu.memory.bytes()[STACK_BOTTOM as usize..STACK_TOP as usize - 2],
            &zero[..]
        );
    }

    #[test]
    fn stack_pop_word() {
        let mut cpu = CPU::new();

        cpu.memory.set(STACK_TOP, 0x80);
        cpu.memory.set(STACK_TOP - 1, 0x01);
        cpu.sp = 0xfd;

        assert_eq!(cpu.stack_pop_word(), 0x8001);
    }

    #[test]
    fn stack_push_word() {
        let mut cpu = CPU::new();
        let word: u16 = 0x8001;

        cpu.stack_push_word(word);

        assert_eq!(cpu.sp, 0xfd);
        assert_eq!(cpu.memory.get(STACK_TOP), 0x80);
        assert_eq!(cpu.memory.get(STACK_TOP - 1), 0x01);
    }

    #[test]
    fn pla() {
        let mut cpu = CPU::new();
        cpu.memory.set(0x0000, 0x68);
        cpu.memory.set(STACK_TOP, 0xAB);
        cpu.sp = 0xFE;

        cpu.step();

        assert_eq!(cpu.pc, 0x0001);
        assert_eq!(cpu.a, 0xAB);
        assert_eq!(cpu.sp, 0xFF);
    }

    #[test]
    fn jsr_must_push_return_address() {
        let mut cpu = CPU::new();
        let program: [u8; 3] = [
            operation::by_name_and_addressing("JSR", operation::AddressingMode::Absolute).code,
            0xD2,
            0xFF,
        ];

        cpu.load(&program, 0x800);
        cpu.step();

        assert_eq!(cpu.pc, 0xFFD2);
        assert_eq!(cpu.sp, 0xFD);
        assert_eq!(cpu.stack_pop_word(), 0x802);
    }

    #[test]
    fn lsr_zero_page() {
        let mut cpu = CPU::new();
        let program: [u8; 2] = [
            operation::by_name_and_addressing("LSR", operation::AddressingMode::ZeroPage).code,
            10,
        ];
        cpu.memory.set(10, 0xFF);

        cpu.load(&program, 0x800);
        cpu.step();

        assert_eq!(cpu.memory.get(10), 0x7F); // 0xFF >> 1
        assert_eq!(cpu.get_carry(), 1);
    }

    #[test]
    fn operation_lsr_accumulator() {
        let mut cpu = CPU::new();
        let program: [u8; 1] = [operation::by_name_and_addressing(
            "LSR",
            operation::AddressingMode::AccumulatorAddressing,
        )
        .code];
        cpu.a = 0x41;

        cpu.load(&program, 0x800);
        cpu.step();

        assert_eq!(cpu.a, 0x20);
        assert_eq!(cpu.p, 0b0011_0001);
    }

    #[test]
    fn sbc_simple_test() {
        let mut cpu = CPU::new();
        cpu.a = 10;
        cpu.set_carry();
        let program: [u8; 2] = [
            operation::by_name_and_addressing("SBC", operation::AddressingMode::Immediate).code,
            10,
        ];

        cpu.load(&program, 0x800);
        cpu.step();

        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.get_zero(), FLAG_ZERO);
    }

    #[test]
    fn sbc_immediate_zeros() {
        let mut cpu = CPU::new();
        cpu.a = 0;
        cpu.set_carry();
        let program: [u8; 2] = [
            operation::by_name_and_addressing("SBC", operation::AddressingMode::Immediate).code,
            0,
        ];

        cpu.load(&program, 0x800);
        cpu.step();

        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.get_zero(), FLAG_ZERO);
        assert_eq!(cpu.get_carry(), FLAG_CARRY);
        assert_eq!(cpu.get_negative(), 0);
    }

    #[test]
    fn sbc_immediate_one_minus_one() {
        let mut cpu = CPU::new();
        cpu.a = 1;
        cpu.set_carry();
        let program: [u8; 2] = [
            operation::by_name_and_addressing("SBC", operation::AddressingMode::Immediate).code,
            1,
        ];

        cpu.load(&program, 0x800);
        cpu.step();

        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.get_zero(), FLAG_ZERO);
        assert_eq!(cpu.get_carry(), FLAG_CARRY);
        assert_eq!(cpu.get_negative(), 0);
    }

    #[test]
    fn sbc_immediate_10_minus_5_minus_carry() {
        let mut cpu = CPU::new();
        cpu.a = 10;
        cpu.clear_carry();
        let program: [u8; 2] = [
            operation::by_name_and_addressing("SBC", operation::AddressingMode::Immediate).code,
            5,
        ];

        cpu.load(&program, 0x800);
        cpu.step();

        assert_eq!(cpu.a, 4);
        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_carry(), FLAG_CARRY);
        assert_eq!(cpu.get_negative(), 0);
    }

    #[test]
    fn sbc_immediate_with_zero() {
        let mut cpu = CPU::new();
        cpu.a = 1;
        cpu.set_carry();
        let program: [u8; 2] = [
            operation::by_name_and_addressing("SBC", operation::AddressingMode::Immediate).code,
            5,
        ];

        cpu.load(&program, 0x800);
        cpu.step();

        assert_eq!(cpu.a, 252);
        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_carry(), 0);
        assert_eq!(cpu.get_negative(), FLAG_NEGATIVE);
    }

    #[test]
    fn sbc_immediate_carry_but_no_overflow() {
        let mut cpu = CPU::new();
        cpu.a = 0x50;
        cpu.set_carry();
        let program: [u8; 2] = [
            operation::by_name_and_addressing("SBC", operation::AddressingMode::Immediate).code,
            0xf0,
        ];

        cpu.load(&program, 0x800);
        cpu.step();

        assert_eq!(cpu.a, 0x60);
        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_carry(), 0);
        assert_eq!(cpu.get_negative(), 0);
        assert_eq!(cpu.get_overflow(), 0);
    }

    #[test]
    fn sbc_immediate_carry_and_overflow() {
        let mut cpu = CPU::new();
        cpu.a = 0x50;
        cpu.set_carry();
        let program: [u8; 2] = [
            operation::by_name_and_addressing("SBC", operation::AddressingMode::Immediate).code,
            0xb0,
        ];

        cpu.load(&program, 0x800);
        cpu.step();

        assert_eq!(cpu.a, 0xa0);
        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_carry(), 0);
        assert_eq!(cpu.get_negative(), FLAG_NEGATIVE);
        assert_eq!(cpu.get_overflow(), FLAG_OVERFLOW);
    }

    #[test]
    fn sbc_immediate_no_carry_and_overflow() {
        let mut cpu = CPU::new();
        cpu.a = 0xd0;
        cpu.set_carry();
        let program: [u8; 2] = [
            operation::by_name_and_addressing("SBC", operation::AddressingMode::Immediate).code,
            0x70,
        ];

        cpu.load(&program, 0x800);
        cpu.step();

        assert_eq!(cpu.a, 0x60);
        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_carry(), FLAG_CARRY);
        assert_eq!(cpu.get_negative(), 0);
        assert_eq!(cpu.get_overflow(), FLAG_OVERFLOW);
    }

    #[test]
    fn sbc_immediate_no_carry_and_no_overflow() {
        let mut cpu = CPU::new();
        cpu.a = 0xd0;
        cpu.set_carry();
        let program: [u8; 2] = [
            operation::by_name_and_addressing("SBC", operation::AddressingMode::Immediate).code,
            0x30,
        ];

        cpu.load(&program, 0x800);
        cpu.step();

        assert_eq!(cpu.a, 0xa0);
        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_carry(), FLAG_CARRY);
        assert_eq!(cpu.get_negative(), FLAG_NEGATIVE);
        assert_eq!(cpu.get_overflow(), 0);
    }

    #[test]
    fn sbc_immediate_with_overflow() {
        let mut cpu = CPU::new();
        cpu.a = 1;
        cpu.set_carry();
        let program: [u8; 2] = [
            operation::by_name_and_addressing("SBC", operation::AddressingMode::Immediate).code,
            5,
        ];

        cpu.load(&program, 0x800);
        cpu.step();

        assert_eq!(cpu.a, 252);
        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_carry(), 0);
        assert_eq!(cpu.get_negative(), FLAG_NEGATIVE);
    }

    #[test]
    fn ror_absolute_zero_and_carry_zero() {
        let mut cpu = CPU::new();
        let program: [u8; 3] = [
            operation::by_name_and_addressing("ROR", operation::AddressingMode::Absolute).code,
            0x00,
            0x80,
        ];

        cpu.load(&program, 0x800);
        cpu.step();

        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.get_zero(), FLAG_ZERO);
        assert_eq!(cpu.get_carry(), 0);
        assert_eq!(cpu.get_negative(), 0);
        assert_eq!(cpu.memory.get(0x8000), 0);
    }

    #[test]
    fn ror_absolute_zero_and_carry_one() {
        let mut cpu = CPU::new();
        cpu.set_carry();
        let program: [u8; 3] = [
            operation::by_name_and_addressing("ROR", operation::AddressingMode::Absolute).code,
            0x00,
            0x80,
        ];

        cpu.load(&program, 0x800);
        cpu.step();

        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_carry(), 0);
        assert_eq!(cpu.get_negative(), FLAG_NEGATIVE);
        assert_eq!(cpu.memory.get(0x8000), 0x80);
    }

    #[test]
    fn bmi() {
        let mut cpu = CPU::new();
        cpu.set_negative();
        let program: [u8; 2] = [
            operation::by_name_and_addressing("BMI", operation::AddressingMode::RelativeAddressing)
                .code,
            0x06,
        ];

        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.pc, 0x0002 + 0x06);
    }

    #[test]
    fn bit_absolite_zero_flag() {
        let mut cpu = CPU::new();

        let program: [u8; 3] = [
            operation::by_name_and_addressing("BIT", operation::AddressingMode::Absolute).code,
            0x10,
            0x11,
        ];
        cpu.memory.set(0x1110, 0b0001_0000);
        cpu.a = 0b1100_0000;

        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.get_zero(), FLAG_ZERO);
        assert_eq!(cpu.get_negative(), 0);
        assert_eq!(cpu.get_overflow(), 0);
    }

    #[test]
    fn bit_absolute_saves_6_and_7_bits() {
        let mut cpu = CPU::new();

        let program: [u8; 3] = [
            operation::by_name_and_addressing("BIT", operation::AddressingMode::Absolute).code,
            0x10,
            0x11,
        ];
        cpu.memory.set(0x1110, 0b1100_0000);
        cpu.a = 0b1100_0000;

        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_negative(), FLAG_NEGATIVE);
        assert_eq!(cpu.get_overflow(), FLAG_OVERFLOW);
    }

    #[test]
    fn bit_absolute_saves_6_and_7_bits_when_they_are_0() {
        let mut cpu = CPU::new();
        cpu.set_negative();
        cpu.set_overflow();
        cpu.set_zero();

        let program: [u8; 3] = [
            operation::by_name_and_addressing("BIT", operation::AddressingMode::Absolute).code,
            0x10,
            0x11,
        ];
        cpu.memory.set(0x1110, 1);
        cpu.a = 1;

        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_negative(), 0);
        assert_eq!(cpu.get_overflow(), 0);
    }

    #[test]
    fn bit_zero_page_zero_flag() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("BIT", operation::AddressingMode::ZeroPage).code,
            0x10,
        ];
        cpu.memory.set(0x10, 0b0001_0000);
        cpu.a = 0b1100_0000;

        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.get_zero(), FLAG_ZERO);
        assert_eq!(cpu.get_negative(), 0);
        assert_eq!(cpu.get_overflow(), 0);
    }

    #[test]
    fn bit_zero_page_saves_6_and_7_bits() {
        let mut cpu = CPU::new();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("BIT", operation::AddressingMode::ZeroPage).code,
            0x10,
        ];
        cpu.memory.set(0x10, 0b1100_0000);
        cpu.a = 0b1100_0000;

        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_negative(), FLAG_NEGATIVE);
        assert_eq!(cpu.get_overflow(), FLAG_OVERFLOW);
    }

    #[test]
    fn bit_zero_page_saves_6_and_7_bits_when_they_are_0() {
        let mut cpu = CPU::new();
        cpu.set_negative();
        cpu.set_overflow();
        cpu.set_zero();

        let program: [u8; 2] = [
            operation::by_name_and_addressing("BIT", operation::AddressingMode::ZeroPage).code,
            0x10,
        ];
        cpu.memory.set(0x10, 1);
        cpu.a = 1;

        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_negative(), 0);
        assert_eq!(cpu.get_overflow(), 0);
    }

    #[test]
    fn rts() {
        let mut cpu = CPU::new();

        let program: [u8; 5] = [
            operation::by_name_and_addressing("LDA", operation::AddressingMode::Immediate).code,
            0x50,
            operation::by_name_and_addressing("PHA", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("PHA", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("RTS", operation::AddressingMode::Implied).code,
        ];

        cpu.load(&program, 0x0);

        for _ in 0..4 {
            cpu.step();
        }

        assert_eq!(cpu.a, 0x50);
        assert_eq!(cpu.pc, 0x5051);
        assert_eq!(cpu.p, 0b0011_0000);
    }

    #[test]
    fn jmp_indirect() {
        let mut cpu = CPU::new();

        let program: [u8; 3] = [
            operation::by_name_and_addressing("JMP", operation::AddressingMode::IndirectAbsolute)
                .code,
            0x00,
            0x02,
        ];

        cpu.memory.set(0x0200, 0xCD);
        cpu.memory.set(0x0201, 0xAB);

        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.pc, 0xABCD);
    }

    #[test]
    fn asl_accumulator_zero() {
        let mut cpu = CPU::new();

        let program: [u8; 1] = [operation::by_name_and_addressing(
            "ASL",
            operation::AddressingMode::AccumulatorAddressing,
        )
        .code];
        cpu.a = 0;
        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.p, 0b00110010);
    }

    #[test]
    fn asl_accumulator_zero_with_1_from_status() {
        let mut cpu = CPU::new();

        let program: [u8; 1] = [operation::by_name_and_addressing(
            "ASL",
            operation::AddressingMode::AccumulatorAddressing,
        )
        .code];
        cpu.a = 0;
        cpu.p = 0b1111_1111;
        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.p, 0b01111110);
    }

    #[test]
    fn asl_accumulator_80_to_zero() {
        let mut cpu = CPU::new();

        let program: [u8; 1] = [operation::by_name_and_addressing(
            "ASL",
            operation::AddressingMode::AccumulatorAddressing,
        )
        .code];
        cpu.a = 0x80;
        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.get_zero(), FLAG_ZERO);
        assert_eq!(cpu.get_carry(), FLAG_CARRY);
    }

    #[test]
    fn asl_accumulator_shift() {
        let mut cpu = CPU::new();

        let program: [u8; 1] = [operation::by_name_and_addressing(
            "ASL",
            operation::AddressingMode::AccumulatorAddressing,
        )
        .code];
        cpu.a = 1;
        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.a, 0b10);
        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_negative(), 0);
        assert_eq!(cpu.get_overflow(), 0);
    }

    #[test]
    fn rol_accumulator_zero() {
        let mut cpu = CPU::new();
        cpu.clear_zero();
        cpu.clear_carry();
        cpu.clear_negative();

        let program: [u8; 1] = [operation::by_name_and_addressing(
            "ROL",
            operation::AddressingMode::AccumulatorAddressing,
        )
        .code];

        cpu.a = 0;
        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.get_zero(), FLAG_ZERO);
        assert_eq!(cpu.get_negative(), 0);
        assert_eq!(cpu.get_carry(), 0);
    }

    #[test]
    fn rol_accumulator_to_zero() {
        let mut cpu = CPU::new();
        cpu.clear_zero();
        cpu.clear_carry();
        cpu.clear_negative();

        let program: [u8; 1] = [operation::by_name_and_addressing(
            "ROL",
            operation::AddressingMode::AccumulatorAddressing,
        )
        .code];

        cpu.a = 0x80;
        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.a, 0);
        assert_eq!(cpu.get_zero(), FLAG_ZERO);
        assert_eq!(cpu.get_negative(), 0);
        assert_eq!(cpu.get_carry(), FLAG_CARRY);
    }

    #[test]
    fn rol_accumulator_with_initial_carry() {
        let mut cpu = CPU::new();
        cpu.clear_zero();
        cpu.set_carry();
        cpu.clear_negative();

        let program: [u8; 1] = [operation::by_name_and_addressing(
            "ROL",
            operation::AddressingMode::AccumulatorAddressing,
        )
        .code];

        cpu.a = 0x40;
        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.a, 0x81);
        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_negative(), FLAG_NEGATIVE);
        assert_eq!(cpu.get_carry(), 0);
    }

    #[test]
    fn rol_accumulator() {
        let mut cpu = CPU::new();
        cpu.clear_zero();
        cpu.clear_carry();
        cpu.clear_negative();

        let program: [u8; 1] = [operation::by_name_and_addressing(
            "ROL",
            operation::AddressingMode::AccumulatorAddressing,
        )
        .code];

        cpu.a = 0x1;
        cpu.load(&program, 0x0);
        cpu.step();

        assert_eq!(cpu.a, 0x2);
        assert_eq!(cpu.get_zero(), 0);
        assert_eq!(cpu.get_negative(), 0);
        assert_eq!(cpu.get_carry(), 0);
    }

    #[test]
    fn php_and_then_plp() {
        let mut cpu = CPU::new();

        cpu.p = 0b1100_0101;

        let program: [u8; 2] = [
            operation::by_name_and_addressing("PHP", operation::AddressingMode::Implied).code,
            operation::by_name_and_addressing("PLP", operation::AddressingMode::Implied).code,
        ];
        cpu.load(&program, 0x800);

        cpu.step();

        assert_eq!(cpu.sp, 0xfe);
        assert_eq!(
            cpu.memory.get(STACK_BOTTOM + u16::from(cpu.sp) + 1),
            0b1100_0101
        );

        cpu.p = 0;

        cpu.step();

        assert_eq!(cpu.sp, 0xff);
        assert_eq!(cpu.p, 0b1111_0101);
    }

    #[test]
    fn brk() {
        let mut cpu = CPU::new();

        let program =
            [operation::by_name_and_addressing("BRK", operation::AddressingMode::Implied).code];
        cpu.load(&program, 0x800);
        cpu.memory.set(0xfffe, 0x11);
        cpu.memory.set(0xffff, 0x22);

        cpu.step();

        assert_eq!(cpu.pc, 0x2211);
        assert_eq!(cpu.sp, 0xfc);
        assert_eq!(cpu.memory.get(STACK_BOTTOM + 0xff), 0x08);
        assert_eq!(cpu.memory.get(STACK_BOTTOM + 0xfe), 0x02);
        assert_eq!(cpu.memory.get(STACK_BOTTOM + 0xfd), 0b0011_0000);
    }
}
