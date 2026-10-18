# b4 in Rust

A port of the Pascal implementation (`imp/pas`): the VM (`src/vm.rs`), the b4a assembler (`src/asm.rs`) and the b4i interactive interpreter (`src/b4i.rs`), as a library crate plus two binaries.

- `b4i` is the interactive assembler/debugger. It takes the same `-a FILE`, `-i FILE`, `-l FILE`, `-o`, `-p` and `-q` arguments as `imp/pas/b4i`.
- `b4h` is the same interpreter with program output kept in the output buffer (drain it with `/ox?`), for `b4h-tests.org`.

The layout matches Pascal's, so programs that read the VM's internals see the same values:
- 64 KB of RAM;
- registers in the first 0x100 bytes, including the private ones at 0x80 and up (ip, stack heights, state, debug flag);
- the data and control stacks in RAM, 256 cells each.

Where it deliberately differs from Pascal:
- **RAM** is the whole 65,536 bytes; Pascal's array stops 3 bytes short.
- **The stacks** start at cells `mindata` and `minctrl`. Pascal used those cell indices as byte offsets, so data-stack entry 64 landed on control-stack entry 0.
- **`/C`** also drops pending forward references.

It also has the same float ops (`fa`..`fi`), the `tm` stub, and the `io` device: files, processes, args, env, stdin and sleep.

```sh
cargo build --release          # target/release/b4i, target/release/b4h
./test rs                      # from the repo root: b4 b4i bios pre b4s b4h
cargo test                     # the regression tests in tests/
```

Use it as a library with `b4::B4::new(Box::new(std::io::stdout()))`, then `b4.line("2 3 ad ?d")`. The `vm` field holds the machine and `dict` the assembler's labels.

A host program can add its own ops, as the JS version's `addOp` does. `b4.vm.add_op(0xA6, "dd", |vm| { let x = vm.dpop()?; vm.dput(x * 2) })` makes byte A6 run the closure. The assembler, `b4i` and the memory dump then know it as `dd`. A name must be two lower-case ascii letters that aren't a built-in mnemonic. Each name and byte can be taken once, and only bytes with no built-in op can be taken.
