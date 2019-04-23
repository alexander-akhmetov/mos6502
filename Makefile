replica1:
	RUST_LOG=warn cargo run --features build-binary --bin apple1 -- -p roms/replica1.bin -l e000

apple30:
	RUST_LOG=warn cargo run --features build-binary --bin apple1 -- -p roms/apple30.bin -l 280

minichess:
	RUST_LOG=error cargo run --features build-binary --bin apple1 -- -p roms/ASMmchess.bin -l 300

functional-tests:
	RUST_LOG=info cargo run --features build-binary --bin mos6502-cli roms/6502_functional_test.bin 400
