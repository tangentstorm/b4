# B4i MCP Server Usage Examples

This document shows examples of using the B4i MCP server to interact with the B4 virtual machine.

## Basic VM Operations

### Check VM State

```javascript
// Query the data stack
await b4i_execute({ command: "?d" })
// Output: ds: []

// Query the control stack
await b4i_execute({ command: "?c" })
// Output: cs: []

// Query instruction pointer
await b4i_execute({ command: "?i" })
// Output: ip: 100
```

### Calculator Mode

The B4i interpreter can be used as a hex calculator:

```javascript
// Push two numbers and add them
await b4i_execute({ command: "01 02 ad ?d" })
// Output: ds: [3]

// More complex calculation: (10 + 5) * 2
await b4i_execute({ command: "0A 05 ad 02 ml ?d" })
// Output: ds: [1E]  // 30 in hex
```

## Assembly and Memory

### Write Bytecode to Memory

```javascript
// Assemble a simple program at address 0x100
await b4i_assemble({
  address: "100",
  bytes: "lb 42 rt"
})
// Output: lb !B rt .. .. .. .. .. .. .. .. .. .. .. .. ..

// Verify by reading memory
await b4i_query_memory({ address: "100" })
// Output: lb !B rt .. .. .. .. .. .. .. .. .. .. .. .. ..
```

### Define and Use Labels

```javascript
// Define a label called "emit" that outputs a character
await b4i_assemble({
  address: "emit",
  bytes: "lb 'e io rt"
})

// Now we can call it by register
await b4i_execute({ command: "'h ^E 'i ^E" })
// Output: hi
```

## Registers

The B4 VM has 26 registers (A-Z) that can store 32-bit values:

```javascript
// Store a value in register X
await b4i_register({
  register: "X",
  value: "12345678"
})
// Output: Register X set\n00012345678

// Read register X
await b4i_register({ register: "X" })
// Output: 00012345678

// Use register as a pointer
await b4i_execute({ command: "100 !X" })  // Store address 0x100 in X
await b4i_execute({ command: "^X" })       // Call code at address in X
```

## Debugging

### Step Through Execution

```javascript
// Assemble a program
await b4i_assemble({
  address: "100",
  bytes: "c0 c1 c2 ad ad rt"
})

// Reset IP to 0x100
await b4i_execute({ command: "0100 !I" })

// Step through one instruction at a time
await b4i_step()
// Output: ip: 101\nds: [0]

await b4i_step()
// Output: ip: 102\nds: [0 1]

await b4i_step()
// Output: ip: 103\nds: [0 1 2]

await b4i_step()
// Output: ip: 104\nds: [0 3]

await b4i_step()
// Output: ip: 105\nds: [3]
```

### Using Breakpoints

The `db` (debug break) opcode pauses execution:

```javascript
// Assemble code with a breakpoint
await b4i_assemble({
  address: "100",
  bytes: "c0 c1 db c2 c4 rt"
})

// Execute until breakpoint
await b4i_execute({ command: "^@ \\g" })
// Execution stops at the db instruction
// Output: ds: [0 1]

// Continue execution
await b4i_execute({ command: "\\g ?d" })
// Output: ds: [0 1 2 4]
```

## Working with Files

### Load and Assemble B4A Files

```javascript
// Load a B4 assembly file
await b4i_load_image({ path: "bios/b4i.b4a" })

// The assembler processes the file and creates labels
await b4i_execute({ command: "\\p" })  // Print dictionary
// Shows all defined labels
```

### Run B4i Scripts

```javascript
// Execute a B4i script file
await b4i_load_image({ path: "init.b4i" })
// Scripts can contain any b4i commands
```

## Advanced Examples

### Fibonacci Sequence

Calculate the first 10 Fibonacci numbers:

```javascript
// Define fibonacci function
await b4i_assemble({
  address: "fib",
  bytes: `
    c0 c1                # push 0, 1 (first two fibonacci numbers)
    lb 0A                # push 10 (count)
    dc                   # move count to control stack
  `
})

await b4i_assemble({
  address: "loop",
  bytes: `
    du                   # duplicate top (next fib number)
    ov                   # over (get previous fib number)
    ad                   # add them
    nx 00                # loop back if count > 0
    rt                   # return
  `
})

// Execute
await b4i_execute({ command: "^fib ?d" })
```

### Memory Scanner

Scan memory for a specific byte pattern:

```javascript
// Write pattern to memory
await b4i_execute({ command: ":100 AA BB CC DD" })
await b4i_execute({ command: ":200 AA BB CC DD" })

// Scan for pattern
await b4i_execute({ command: "100 !X" })  // Start address

for (let i = 0; i < 16; i++) {
  const result = await b4i_execute({
    command: `@X rb ?d zp @X c1 ad !X`
  })
  // Read byte, check if it matches, increment address
}
```

### String Processing

```javascript
// Define a counted string
await b4i_execute({ command: ':msg .\"Hello, B4!\"' })

// Print the string
await b4i_assemble({
  address: "print",
  bytes: `
    @msg                 # Get string address
    du rb                # Duplicate and read length byte
    dc                   # Move count to control stack
    c1 ad !X             # Point X to first char
  `
})

await b4i_assemble({
  address: "print_loop",
  bytes: `
    c1 +X rb             # Read character, increment pointer
    lb 'e io             # Emit character
    nx 00                # Loop
    rt
  `
})

await b4i_execute({ command: "^print" })
// Output: Hello, B4!
```

## Stack Operations Reference

Common stack manipulation patterns:

```javascript
// DUP: Duplicate top of stack
await b4i_execute({ command: "42 du ?d" })
// Output: ds: [42 42]

// SWAP: Swap top two items
await b4i_execute({ command: "11 22 sw ?d" })
// Output: ds: [22 11]

// OVER: Copy second item to top
await b4i_execute({ command: "11 22 ov ?d" })
// Output: ds: [11 22 11]

// DROP: Remove top item
await b4i_execute({ command: "11 22 33 zp ?d" })
// Output: ds: [11 22]

// ROT: Rotate top three items
await b4i_execute({ command: "11 22 33 sw ov ?d" })
// Output: ds: [22 33 11]
```

## Control Flow

### Conditional Execution

```javascript
// h0: Hop if zero
await b4i_assemble({
  address: "test",
  bytes: `
    c1                   # push 1 (true)
    h0 05                # if zero, skip next 5 bytes
    lb 'T lb 'e io       # print 'T' if not zero
    rt
  `
})

await b4i_execute({ command: "^test" })
// Output: T
```

### Loops

```javascript
// nx: Count down loop
await b4i_assemble({
  address: "countdown",
  bytes: `
    lb 05 dc             # Count from 5
  `
})

await b4i_assemble({
  address: "loop",
  bytes: `
    lb 'X lb 'e io       # Print 'X'
    nx 00                # Loop back
    rt
  `
})

await b4i_execute({ command: "^countdown" })
// Output: XXXXX
```

## Resetting the VM

```javascript
// Reset: Clear stacks, set IP to 0x100, keep memory
await b4i_reset()

// Clear: Complete VM reset including memory
await b4i_clear()
```

## Tips and Tricks

### 1. Use Labels for Reusable Code

Define utility functions as labels:

```javascript
await b4i_assemble({ address: "E", bytes: "lb 'e io rt" })  // Emit
await b4i_assemble({ address: "N", bytes: "lb 0A ^E rt" })  // Newline
await b4i_assemble({ address: "S", bytes: "lb 20 ^E rt" })  // Space
```

### 2. Save Memory State

Query and save memory for later restoration:

```javascript
const snapshot = [];
for (let addr = 0x100; addr < 0x200; addr += 0x10) {
  const mem = await b4i_query_memory({
    address: addr.toString(16)
  })
  snapshot.push(mem);
}
```

### 3. Batch Commands

Use `b4i_execute` with multiple commands:

```javascript
await b4i_execute({
  command: "c0 c1 ad c2 ad c4 ad ?d"
})
// More efficient than separate calls
```

### 4. Register Dictionary Pattern

Use registers A-Z as a 26-entry dictionary of addresses:

```javascript
await b4i_assemble({ address: "A", bytes: "lb 'a lb 'e io rt" })
await b4i_assemble({ address: "B", bytes: "lb 'b lb 'e io rt" })
await b4i_assemble({ address: "C", bytes: "lb 'c lb 'e io rt" })
// Now you can call them with ^A, ^B, ^C
```

### 5. Memory as Data Structure

Use memory regions for different purposes:

- `0x0000-0x00FF`: Control character dictionary (32 entries × 4 bytes)
- `0x0100-0x0FFF`: Code region (default assembly target)
- `0x1000-0x1FFF`: Data/strings
- `0x2000+`: Stack/heap structures

## Error Handling

Check for common errors:

```javascript
// Division by zero
await b4i_execute({ command: "0A 00 dv ?d" })
// Depends on VM implementation

// Stack underflow
await b4i_execute({ command: "%C ad ?d" })
// Tries to add with empty stack

// Invalid memory access
await b4i_query_memory({ address: "FFFFFF" })
// May clamp to valid range
```

## Performance Considerations

1. **Batch operations**: Send multiple commands in one `b4i_execute` call
2. **Persistent session**: The MCP server maintains one B4i process
3. **No compilation overhead**: Bytecode executes directly
4. **Memory efficient**: VM uses only what you allocate

## Next Steps

- Read the [B4 specification](../doc/b4i.org)
- Study the [test suite](../b4-tests.org) for more examples
- Examine the [BIOS implementation](../bios/b4i.b4a)
- Build your own B4 programs!
