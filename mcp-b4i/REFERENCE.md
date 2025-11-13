# B4i MCP Server Quick Reference

## MCP Tools Summary

| Tool | Purpose | Required Args | Optional Args |
|------|---------|---------------|---------------|
| `b4i_execute` | Run any B4i command | `command` | - |
| `b4i_load_image` | Load .b4a/.b4i file | `path` | - |
| `b4i_query_stack` | Show data & control stacks | - | - |
| `b4i_query_memory` | Dump 16 bytes | `address` | - |
| `b4i_assemble` | Assemble bytecode | `address`, `bytes` | - |
| `b4i_step` | Execute one instruction | - | - |
| `b4i_reset` | Reset VM (keep memory) | - | - |
| `b4i_clear` | Clear VM completely | - | - |
| `b4i_register` | Read/write register | `register` | `value` |

## B4i Command Reference

### VM State Queries

```
?d              Show data stack
?c              Show control stack
?i              Show instruction pointer
?R              Show register R (e.g., ?X, ?A, ?_)
?100            Dump 16 bytes at hex address 0x100
```

### VM Control

```
%s              Step: execute one instruction
%q              Quit (not needed in MCP context)
%C              Clear: reset everything
%R              Reset: clear stacks, keep memory
\g              Go: run until halt/breakpoint
```

### Assembly

```
:100 AA BB CC   Assemble bytes at address
:label rt       Define label
:R              Point register R to current address
```

### Calculator Mode

```
FF 42           Push hex numbers to stack
'a 'X           Push ASCII values
ad sw du        Execute opcodes immediately
^R              Call code at register R
`R              Push address of register R
@label          Read 4 bytes at label
!label          Write 4 bytes to label
```

### Files & Dictionary

```
\p              Print dictionary (all labels)
\a file.b4a     Assemble file
\i file.b4i     Interpret script
\d [path]       Change directory
\f              Show forward references
```

## B4 Opcodes Quick Reference

### Constants (0x00-0x0F)

```
c0 c1 c2 c4     Push 0, 1, 2, 4
n1              Push -1 (0xFFFFFFFF)
```

### Stack Operations (0x10-0x1F)

```
du              Duplicate top
zp              Drop (zero pop)
sw              Swap top two
ov              Over: copy second to top
cd dc           Control stack: to/from data stack
```

### Arithmetic (0x20-0x2F)

```
ad              Add
sb              Subtract
ml              Multiply
dv              Divide
md              Modulo
```

### Bitwise (0x30-0x3F)

```
an              AND
or              OR
xr              XOR
nt              NOT (bitwise complement)
sh              Shift (positive = left, negative = right)
```

### Comparison (0x40-0x4F)

```
eq              Equal (returns -1 or 0)
lt              Less than
```

### Memory Access (0x50-0x5F)

```
ri              Read int32 from address
wi              Write int32 to address
rb              Read byte from address
wb              Write byte to address
rs              Read signed byte from address
```

### Control Flow (0x60-0x6F)

```
rt              Return (pop address from control stack)
cl              Call (push return address, jump)
jm              Jump to 4-byte address
hp              Hop (relative jump, signed byte)
h0              Hop if zero
nx              Next (decrement counter, loop if not zero)
```

### Sequential Ops (0x70-0x7F)

```
li              Load int32 (next 4 bytes)
lb              Load byte (next byte)
ls              Load signed byte (next byte)
```

### Register Ops (0x80-0x9F)

```
@R              Read register R
!R              Write to register R
+R              Add to register, return old value
```

### Control Characters (0x00-0x1F as `X notation)

```
`@              Address 0x0000
`A through `Z   Addresses 0x0004 through 0x0068
`[ `\ `] `^ `_  Addresses 0x006C through 0x007C
^A through ^Z   Call word at control character address
```

### Special Ops

```
io              I/O operation (TOS = command, NOS = data)
  'e io         Emit character
  'r io         Read character
  'f io         Flush output
hl              Halt VM
db              Debug break (pause for inspection)
```

## Common Patterns

### Define and Call Function

```javascript
// Define
await b4i_assemble({
  address: "myfunc",
  bytes: "c0 c1 ad rt"
})

// Call via register
await b4i_execute({ command: "^myfunc ?d" })

// Call via cl opcode
await b4i_execute({ command: "`myfunc cl ?d" })
```

### Loop N Times

```javascript
await b4i_assemble({
  address: "loop",
  bytes: "lb 05 dc"  // 5 iterations
})
await b4i_assemble({
  address: "body",
  bytes: "... nx 00 rt"
})
```

### Conditional Branch

```javascript
await b4i_assemble({
  address: "test",
  bytes: `
    ... eq          # compare values
    h0 05           # if equal (zero), skip
    ... then-code   # executed if not equal
  `
})
```

### String Definition and Use

```javascript
// Counted string
await b4i_execute({ command: ':msg ."Hello"' })

// Raw string
await b4i_execute({ command: ':raw "Hello"' })

// Access
await b4i_execute({ command: "`msg ri ?d" })  // Get address
```

### Stack Manipulation

```javascript
// ROT3: Rotate top 3 items (a b c -> b c a)
await b4i_execute({ command: "dc sw cd sw" })

// NIP: Remove second item (a b -> b)
await b4i_execute({ command: "sw zp" })

// TUCK: Copy top below second (a b -> b a b)
await b4i_execute({ command: "sw ov" })

// 2DUP: Duplicate top two (a b -> a b a b)
await b4i_execute({ command: "ov ov" })
```

### Memory Block Copy

```javascript
// Copy 4 bytes from 0x100 to 0x200
await b4i_execute({
  command: "0100 ri 0200 wi"
})
```

### Using Registers as Pointers

```javascript
// Initialize pointer
await b4i_execute({ command: "1000 !X" })

// Read and advance
await b4i_execute({ command: "01 +X rb ?d" })  // Read, X += 1

// Write and advance
await b4i_execute({ command: "42 01 +X wb" })  // X += 1, write 42
```

## Memory Map Convention

```
0x0000-0x00FF   Control character dictionary (32 × 4 bytes)
0x0100-0x0FFF   Code region (assembly default)
0x1000-0x1FFF   Data/constants/strings
0x2000-0x2FFF   Heap/dynamic allocation
0x3000+         Stack space
```

## Register Usage Convention

```
A-Z             General purpose (26 registers)
_               "Here" pointer (assembly address)
I               Instruction pointer (read-only via ?i)
```

## Hex/ASCII Quick Reference

```
Hex → Dec       ASCII
20     32       (space)
30-39  48-57    0-9
41-5A  65-90    A-Z
61-7A  97-122   a-z
0A     10       \n (newline)
0D     13       \r (carriage return)
00     0        \0 (null)
```

## Example: Complete Program

```javascript
// 1. Clear VM
await b4i_clear()

// 2. Define emit helper
await b4i_assemble({
  address: "E",
  bytes: "lb 'e io rt"
})

// 3. Define newline helper
await b4i_assemble({
  address: "N",
  bytes: "lb 0A ^E rt"
})

// 4. Define greeting
await b4i_execute({ command: ':msg ."Hi!"' })

// 5. Define print string function
await b4i_assemble({
  address: "P",
  bytes: `
    du rb dc c1 ad !X    # len → cs, addr+1 → X
  `
})
await b4i_assemble({
  address: "P2",
  bytes: `
    c1 +X rb ^E nx 00 rt  # print loop
  `
})

// 6. Run it
await b4i_execute({ command: "`msg ^P ^N" })
// Output: Hi!

// 7. Verify final state
await b4i_query_stack()
```

## Debugging Tips

1. **Use `db` opcode** to set breakpoints
2. **Check stacks after each operation**: `?d ?c`
3. **Inspect memory before/after**: `?100`
4. **Use `%s` to step through code**
5. **Print dictionary to see labels**: `\p`
6. **Use registers to save important addresses**
7. **Reset with `%R` to clear stacks without losing code**

## Performance Tips

1. **Batch commands**: `"c0 c1 ad c2 ad ?d"` not 4 separate calls
2. **Use opcodes**: `ad` not `cl add_addr rt`
3. **Keep code tight**: fewer hops = faster execution
4. **Use registers**: avoid re-calculating addresses

## Common Errors

| Error | Cause | Solution |
|-------|-------|----------|
| Stack underflow | Not enough items for operation | Check stack with `?d` |
| Infinite loop | `hp 00` or bad `nx` | Use `db` to inspect |
| Wrong memory | Address calculation error | Print addresses with `?d` |
| Register not set | Using uninitialized register | Check with `?R` |
| Label not found | Typo or not yet defined | Use `\p` to list labels |

## File Format Notes

### .b4a (Assembly)

```
:label op1 op2
  op3 op4

:label2
  .i cond .t code .e other .t
```

### .b4i (Script)

```
# Comments
?d              # Commands to execute
:100 AA BB      # Assembly
^A              # Invocation
%q              # Quit (optional)
```

### .b4x (Binary)

Raw 16KB memory dump, typically loaded with custom loader code.

## Resources

- Full Documentation: `../doc/b4i.org`
- Test Suite: `../b4-tests.org`, `../b4i-tests.org`
- Examples: `example.md`
- Installation: `INSTALL.md`
- Main README: `README.md`
