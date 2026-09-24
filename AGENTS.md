B4 is a small forth-like virtual machine that
runs atop many different software stacks.

We use 'tanco' to test. Implementations live in `imp/`.
Run `./test` for every language, `./test pas` for Pascal,
`./test pas b4 b4i` for selected suites, or `./test -- b4` for the
basic suite on every implementation.

Current focus is implementing the machine in bend.

Git workflow:

- Do not commit to `main`. Commit on a branch and open a pull request against `main` for review.

When using Bend:

- run `bend guide` to learn it
- use `imp/bend/LAWS.bend` to keep important rules
- run `bend imp/bend/PROOF.bend` before committing
- parallelize the code whenever possible

Project reference:

- `b4a` is the VM's assembly language. `b4i` is the interactive
  assembler/debugger used for testing and direct interaction.
- The VM has a data stack (`ds`) for parameters/results and a control
  stack (`cs`) for return addresses and loop counters. Stack diagrams
  use `( before -- after )`, with the top of the stack on the right.
- Memory addresses `0x0000..0x007F` hold 32 named, 4-byte registers
  (`@` through `_`); `0x0080..0x00FF` hold system variables. Consult
  `b4a/bios.b4a` for the BIOS memory layout rather than assuming a fixed
  application start address.
- Register operations include `@R` (fetch), `!R` (store), and `^R`
  (invoke code at the address held in R). By convention, `@_` is the
  assembly/heap write pointer (`here`), and `@^` is the dictionary head
  (`last`). See `doc/registers.org` for details and exceptions.
- Opcodes use two-letter mnemonics, such as `ad` (add) and `sw` (swap).
  See `doc/opcodes.org` and `b4-tests.org` for instruction semantics and
  examples rather than maintaining a separate opcode summary here.

Interactive assembler:

- See `doc/b4i.org` for syntax and the feature support matrix across
  implementations, and `b4i-tests.org` for interpreter tests.
- In calculator mode, hexadecimal numbers such as `AA` and `1234`
  push values; character literals use a leading quote, such as `'a`.
  Opcodes execute immediately. Assembly mode writes bytes instead.
- `:labelname` defines a label; `:100 AA BB` writes bytes at address
  `0x100`.
- Useful commands: `?d` (data stack), `?c` (control stack), `?i`
  (instruction pointer), `?R` (register R), `?100` (16-byte memory dump),
  `/s` (single step), and `/q` (quit).

BIOS and Forth layer:

- `b4a/bios.b4a` supplies BIOS services; `b4a/b4f.b4a` supplies the
  Forth-like dictionary and REPL. See `b4a/b4f.b4a.org` for the literate
  source and explanation.
- Dictionary entries contain a 4-byte link to the previous entry, a
  counted name string, then code/data. The `.^` assembler macro builds
  these linked entries.
- The Forth layer provides `find`, `exec`, `words`, `repl`, and `try-num`.
  Its `^X` utility uses `dc rt` to execute an address from the data stack.

Pascal implementation workflow:

- Build the interactive interpreter with `make -C imp/pas b4i`, then run
  `./imp/pas/b4i` (or use `make -C imp/pas runi`). The Makefile also has targets
  for the other Pascal executables; its default target prints usage.
- Run `./test pas b4` for VM tests and `./test pas b4i` for interpreter
  tests. Both build `b4i` and invoke `tanco` with the corresponding
  top-level test document.
- Key sources: `imp/pas/ub4.pas` (VM), `imp/pas/ub4i.pas` (interactive
  interpreter), and `imp/pas/ub4asm.pas` (assembler).
