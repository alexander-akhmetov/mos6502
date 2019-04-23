extern crate mos6502;

#[test]
fn test_simple_program() {
    let mut cpu = mos6502::cpu::CPU::new();

    let program = "LDA #$c0
                   TAX
                   INX
                   ADC #$c4";

    let bytes = mos6502::asm::assemble(&program);
    cpu.load(&bytes, 0x600);

    for _ in 0..4 {
        cpu.step();
    }

    assert_eq!(cpu.a, 0x84);
    assert_eq!(cpu.x, 0xc1);
    assert_eq!(cpu.y, 0x00);
    assert_eq!(cpu.sp, 0xff);
    assert_eq!(cpu.pc, 0x0606);
    assert_eq!(cpu.p, 0b1011_0001);
}

#[test]
fn test_simple_program_2() {
    let mut cpu = mos6502::cpu::CPU::new();

    let program = "LDA #$80
                   STA $01
                   ADC $01";

    let bytes = mos6502::asm::assemble(&program);
    cpu.load(&bytes, 0x600);

    for _ in 0..3 {
        cpu.step();
    }

    assert_eq!(cpu.a, 0x00);
    assert_eq!(cpu.x, 0x00);
    assert_eq!(cpu.y, 0x00);
    assert_eq!(cpu.sp, 0xff);
    assert_eq!(cpu.pc, 0x0606);
    assert_eq!(cpu.p, 0b0111_0011);
}

#[test]
fn sta_with_absolute_addressing() {
    let mut cpu = mos6502::cpu::CPU::new();

    let program = "LDA #$80
                   STA $FDE8";

    let bytes = mos6502::asm::assemble(&program);
    cpu.load(&bytes, 0x600);

    for _ in 0..2 {
        cpu.step();
    }

    assert_eq!(cpu.a, 0x80);
    assert_eq!(cpu.x, 0x00);
    assert_eq!(cpu.y, 0x00);
    assert_eq!(cpu.sp, 0xff);
    assert_eq!(cpu.pc, 0x0605);
    assert_eq!(cpu.p, 0b1011_0000);
}

#[test]
fn program_cycle_with_branching() {
    let mut cpu = mos6502::cpu::CPU::new();

    /*
    x = 5
    y = 0
    while x != 0 {
        x -= 1
        y += 1
    }
    */
    let program = "LDX #$5
                   LDY #$0
                   DEX
                   INY
                   CPX #$0
                   BNE #$FA";

    let bytes = mos6502::asm::assemble(&program);
    cpu.load(&bytes, 0x800);

    for _ in 0..22 {
        cpu.step();
    }

    assert_eq!(cpu.a, 0x00);
    assert_eq!(cpu.x, 0x00);
    assert_eq!(cpu.y, 0x05);
    assert_eq!(cpu.sp, 0xff);
    assert_eq!(cpu.pc, 0x80A);
    assert_eq!(cpu.p, 0b0011_0011);
}

#[test]
fn program_with_stack() {
    let mut cpu = mos6502::cpu::CPU::new();

    let program = "LDX #$00
                   LDY #$00
                   TXA
                   STA $0200
                   PHA
                   INX
                   INY
                   CPY #$10
                   BNE #$F5
                   PLA
                   STA $0200
                   INY
                   CPY #$20
                   BNE #$F7";

    cpu.load(&mos6502::asm::assemble(&program), 0x800);

    for _ in 0..194 {
        cpu.step();
    }

    assert_eq!(cpu.a, 0x00);
    assert_eq!(cpu.x, 0x10);
    assert_eq!(cpu.y, 0x20);
    assert_eq!(cpu.sp, 0xff);
    assert_eq!(cpu.pc, 0x818);
    assert_eq!(cpu.p, 0b0011_0011);
}
