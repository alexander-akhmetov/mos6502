load at E000
enter at E000

run:

```shell
RUST_LOG=debug cargo run --features build-binary --bin mos6502-cli roms/apple1basic.bin -b -a E000
```
