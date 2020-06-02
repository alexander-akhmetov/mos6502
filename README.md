# MOS 6502 CPU emulator

Simple emulator of the [MOS 6502](https://en.wikipedia.org/wiki/MOS_Technology_6502) CPU.
I used it to build an [Apple-1](https://github.com/alexander-akhmetov/apple1) emulator.

## Usage

```rust
let mut cpu = mos6502::cpu::CPU::new();

let program = "LDA #$c0
TAX
INX
ADC #$c4";

// you can skip this step,
// if you load an assembled machine code
let bytes = mos6502::asm::assemble(&program);

cpu.load(&bytes, 0x800);

cpu.run();
```

## Tests

Run tests:

```shell
cargo test
```

### Functional tests

You can download functional tests from this [repository](https://github.com/Klaus2m5/6502_65C02_functional_tests).
Put the `6502_functional_test.bin` file to the root of the repository and then run:

```shell
wget "https://github.com/Klaus2m5/6502_65C02_functional_tests/blob/master/bin_files/6502_functional_test.bin?raw=true" -O 6502_functional_test.bin

make functional-tests
```

## Resources

* [6502 instruction set](https://www.masswerk.at/6502/6502_instruction_set.html#BIT)
* [6502 memory test](http://www.willegal.net/appleii/6502mem.htm)
* [6502 instructions description with undocumented commands](http://www.zimmers.net/anonftp/pub/cbm/documents/chipdata/64doc)
