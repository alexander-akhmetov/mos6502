# Apple1 emulator

## How to use

Run binary:

```
cargo run --features build-binary --bin apple1
```

The command above starts Apple1 with WozMonitor at the address `0xFF00`. You should see the screen and the command line prompt:

```
\
<cursor>
```


With optional flag `-p` you can load an additional program to the memory:

```
cargo run --features build-binary --bin apple1 -- -p asm/apple1hello.asm
```
It will be loaded to the memory with starting address `0xE000`. To run it using Woz Monitor type `E000R` and press enter.
To see the hex content of the program: `E000..<END ADDR>`, for example: `E000.E0FF`.

## Debug

You can disable the screen and enable debug logging:

```
RUST_LOG=debug cargo run --features build-binary --bin apple1 -- -p asm/apple1hello.asm
```

## Tests

Run tests:

```
cargo test
```

Run tests with logging:

```
RUST_LOG=mos6502=debug cargo test -- --nocapture
```

## Apple 1 Basic

* [BASIC source code listing](https://github.com/jefftranter/6502/blob/master/asm/a1basic/a1basic.s)
* [Disassembled BASIC](http://www.brouhaha.com/~eric/retrocomputing/apple/apple1/basic/)


There are two different ROMs, one of them is from Replica1 and it works. The original one, `roms/apple1basic.bin` does not work yet. Seems like it tries to read DSP at `0xD0F2` instead of `0xD012`. You can inspect this if you load it and then print hex data at `E3D5.E3DF` with WozMonitor.

	note: http://www.brielcomputers.com/phpBB3/viewtopic.php?f=10&t=404
	discussion about the same problem

```
E3D5.E3DF

E3D5: 2C F2 D0
E3D8: 30 FB 8D F2 D0 60 A0 06
```

Replica1 basic content:

```
E3D5.E3DF

E3D5: 2C 12 D0
E3D8: 30 FB 8D 12 D0 60 A0 06
```

### Start basic

```
RUST_LOG=error cargo run --features build-binary --bin apple1 -- -p roms/replica1.bin -b
```

and then type `E000R`.


## Resources

* [6502 instruction set](https://www.masswerk.at/6502/6502_instruction_set.html#BIT)
* [Apple1 BASIC manual](https://archive.org/stream/apple1_basic_manual/apple1_basic_manual_djvu.txt)
* [www.applefritter.com](https://www.applefritter.com)
* [6502 memory test](http://www.willegal.net/appleii/6502mem.htm)
* [apple1 programs](http://hoop-la.ca/apple2/2008/retrochallenge.net.html)
* [apple1 programs 2](http://www.willegal.net/appleii/apple1-software.htm)
* [Woz Monitor description](https://www.sbprojects.net/projects/apple1/wozmon.php)
* [6502 instructions description with undocumented commands](http://www.zimmers.net/anonftp/pub/cbm/documents/chipdata/64doc)
