# Project Notes for AI Assistants: b4 Virtual Machine

This document contains my notes on the b4 virtual machine project. I will use this information to better assist in our sessions.

## 1. Project Overview

- **b4** is a virtual machine with a custom assembly language, **b4a**.
- There are multiple implementations of the VM (Pascal, JavaScript, etc.).
- The primary interface for testing and debugging is an interactive assembler/debugger (`b4i`).
- A BIOS (`bios.b4a`) provides a Forth-like environment on top of the raw VM, including a dictionary of "words" and a REPL.
- The project has a long-term vision that includes a Lisp-like interface (`b4s`), memory-mapped I/O, and more extensive documentation.

## 2. Core VM Concepts

### 2.1. Architecture

- **Stacks**: The VM is stack-based.
  - `ds`: Data Stack for parameters and results.
  - `cs`: Control Stack (or return stack) for call/return addresses and loop counters.
- **Memory Layout**:
  - `0000..007F`: 32 named registers (`^@` to `^_`).
  - `0080..00FF`: Internal/system registers.
  - `0100..0FFF`: Reserved for the BIOS.
  - `1000..`: Usable RAM for applications.
- **Registers**: 32 general-purpose registers, accessible via `^` notation (e.g., `^A`, `^X`). The `^_` register is the `here` pointer for assembly.

### 2.2. b4 Assembly Language (b4a)

- **Opcodes**: 2-letter mnemonics (e.g., `ad` for add, `sw` for swap).
- **Literals**:
  - Hex numbers are pushed directly (e.g., `AA`, `1234`).
  - ASCII characters are prefixed with `'` (e.g., `'a'`).
- **Labels & Addresses**:
  - Define a label: `:labelname`
  - Get a label's address: `labelname`
  - Get a control register's address: `` `A ``
- **Stack Diagram Notation**: `( before -- after )`. The top of the stack is on the right. Example: `( x y -- y x )` for the `sw` (swap) op.

### 2.3. Key Opcodes (Summary)

| Category      | Opcodes                                       | Description                                      |
|---------------|-----------------------------------------------|--------------------------------------------------|
| **Stack**     | `du` `sw` `ov` `zp` `dc` `cd`                 | Duplicate, Swap, Over, Zap, Data->Ctrl, Ctrl->Data |
| **Math**      | `ad` `sb` `ml` `dv` `md` `sh`                 | Add, Subtract, Multiply, Divide, Modulo, Shift   |
| **Logic**     | `an` `or` `xr` `nt`                           | AND, OR, XOR, NOT                                |
| **Compare**   | `eq` `lt`                                     | Equal, Less Than (result is `0` or `-1`)         |
| **Memory**    | `rb` `wb` `ri` `wi`                           | Read/Write Byte, Read/Write Integer (32-bit)     |
| **Literals**  | `lb` `li`                                     | Load Byte, Load Integer (from instruction stream)|
| **Control**   | `jm` `hp` `h0` `cl` `rt` `nx` `hl` `db`       | Jump, Hop, Hop-if-0, Call, Return, Next, Halt, Debug |

## 3. Interactive Assembler (`b4i`)

This is the primary tool for testing and direct interaction.

- **Commands**:
  - `%q`: Quit the session.
  - `?d`: Print the data stack.
  - `?c`: Print the control stack.
  - `?i`: Print the instruction pointer (`ip`).
  - `?X`: Query register `X`.
  - `!X`: Store top of stack into register `X`.
  - `@X`: Fetch value from register `X` to stack.
  - `%s`: Step and execute one instruction.
  - `?100`: Show 16 bytes of memory starting at address `0x100`.
  - `:100 AA BB`: Write bytes `AA BB` to memory at address `0x100`.

## 4. BIOS and Forth-like System

The BIOS builds a higher-level environment on the VM.

### 4.1. The Dictionary

- A linked list of "words" (functions/data).
- The `^^` register points to the most recently defined word ("last").
- **Word Structure**:
  1.  (4 bytes) Pointer to the previous word.
  2.  (n bytes) Counted string for the word's name.
  3.  (m bytes) The executable code or data for the word.
- **`.^` Macro**: An assembler macro used in `bios.b4a.org` to simplify creating these linked list entries.

### 4.2. Key BIOS Words

- `^X`: `dc rt` - A core utility to execute a word whose address is on the stack.
- `find`: `( s -- 0|e )` - Searches the dictionary for a string `s` and returns its entry address `e` or `0`.
- `exec`: `( e -- )` - Takes a dictionary entry `e`, finds its code address, and executes it.
- `words`: Lists all defined words in the dictionary.
- `repl`: The main Read-Eval-Print-Loop.
- `try-num`: Attempts to parse a string as a number.

## 5. Project Goals & Direction (from `plan.org`)

- **Current Sprint**:
  - Implement memory-mapped video buffer and terminal commands (`tbuf`, `cscr`, etc.).
  - Separate the BIOS from the Forth interpreter (`b4f`).
- **Backlog**:
  - Improve documentation (`b4-opcodes.org`, assembler syntax).
  - Add more `b4i` commands (save/load memory, list labels, run-to-cursor).
  - Implement a colon compiler (`: ... ;`) for the Forth system.
- **Future Ideas**:
  - A Lisp-like interface (`b4s`).
  - A memory allocator.
  - I/O devices for files, sound, and graphics.
  - Porting games and demos.

## 6. Development Workflow

This section outlines the primary commands and files for working on the Pascal implementation of the b4 virtual machine.

### 6.1. Building and Testing

- **Build:** The canonical way to build the Pascal executables is by using the Makefile in the `pas/` directory.
  #+begin_src shell
  make -C pas
  #+end_src
- **Test:** The test suite for the Pascal implementation can be run with:
  #+begin_src shell
  ./test-b4-pas
  #+end_src

### 6.2. Key Files

- `pas/ub4i.pas`: The core implementation of the interactive assembler/interpreter.
- `pas/ub4asm.pas`: The b4a assembler implementation.
- `doc/b4i.org`: The primary documentation for the b4i interpreter language.

### 6.3. Running the Interpreter

The interactive interpreter can be run with the following command:

#+begin_src shell
cd pas && ./b4i
#+end_src

For details on the commands available in the interpreter, see [[file:doc/b4i.org][The b4i Interactive Interpreter]].
