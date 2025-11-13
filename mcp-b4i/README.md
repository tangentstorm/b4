# B4i MCP Server

An MCP (Model Context Protocol) server that wraps the B4 virtual machine interactive interpreter (`b4i`). This allows AI assistants to interact with the B4 VM for debugging, assembling, and executing B4 bytecode.

## Features

- **Interactive B4i Commands**: Execute any B4i command through MCP tools
- **VM State Inspection**: Query data stack, control stack, registers, and memory
- **Assembly**: Write bytecode to memory addresses with label support
- **Debugging**: Step through execution, set breakpoints (via `db` opcode)
- **Image Loading**: Load `.b4a` assembly files and `.b4i` script files
- **Persistent Session**: Maintains a single B4i process across multiple tool calls

## Installation

1. Make sure you have Node.js installed (v18 or later recommended)
2. Install dependencies:

```bash
cd mcp-b4i
npm install
```

3. Make the server executable (Unix/Linux/Mac):

```bash
chmod +x index.js
```

## Usage

### With Claude Desktop

Add this to your Claude Desktop configuration (`~/Library/Application Support/Claude/claude_desktop_config.json` on macOS):

```json
{
  "mcpServers": {
    "b4i": {
      "command": "node",
      "args": ["/absolute/path/to/b4/mcp-b4i/index.js"]
    }
  }
}
```

Or if you've installed it globally:

```json
{
  "mcpServers": {
    "b4i": {
      "command": "mcp-b4i"
    }
  }
}
```

### Manual Testing

You can test the server manually using the MCP inspector:

```bash
npx @modelcontextprotocol/inspector node index.js
```

## Available Tools

### `b4i_execute`
Execute arbitrary B4i commands. Most flexible tool for direct interaction.

**Examples:**
- `?d` - Show data stack
- `01 02 ad ?d` - Push 1, 2, add them, show result
- `:100 AA BB CC` - Assemble bytes at address 0x100
- `%s` - Step one instruction
- `?100` - Dump 16 bytes at address 0x100

### `b4i_load_image`
Load a B4 assembly or script file.

**Parameters:**
- `path`: Path to `.b4a` (assembly) or `.b4i` (script) file

### `b4i_query_stack`
Get both data and control stack contents.

### `b4i_query_memory`
Dump 16 bytes of memory.

**Parameters:**
- `address`: Hex address (e.g., "100", "1000")

### `b4i_assemble`
Assemble bytecode at an address.

**Parameters:**
- `address`: Hex address or label name
- `bytes`: Space-separated hex bytes or mnemonics

**Examples:**
- Address "100", bytes "lb 42 rt" - Load byte 42 and return
- Address "mylabel", bytes "c0 c1 ad rt" - Create label, push 0, 1, add, return

### `b4i_step`
Execute one instruction and show new state.

### `b4i_reset`
Reset VM (clears stacks, sets IP to 0x100, keeps memory).

### `b4i_clear`
Clear VM completely (stacks, memory, IP).

### `b4i_register`
Read or write VM registers (A-Z).

**Parameters:**
- `register`: Single letter (A-Z)
- `value`: Optional hex value to write

**Examples:**
- Register "X", no value - Read register X
- Register "X", value "12345678" - Write 0x12345678 to register X

## B4i Command Reference

See [doc/b4i.org](../doc/b4i.org) for complete B4i documentation.

### Common Commands

**VM State:**
- `?d` - Data stack
- `?c` - Control stack
- `?i` - Instruction pointer
- `?R` - Register R (e.g., `?X`, `?A`)
- `?100` - Memory dump at address

**VM Control:**
- `%s` - Step one instruction
- `%q` - Quit (not needed in MCP context)
- `%C` - Clear VM
- `%R` - Reset VM
- `\g` - Go (run until halt/breakpoint)

**Assembly:**
- `:100 AA BB CC` - Assemble at address
- `:label rt` - Define label
- `:R` - Point register R to current address

**Calculator:**
- `FF 42` - Push hex numbers
- `'a 'X` - Push ASCII values
- `ad sw du` - Execute opcodes immediately
- `^R` - Call code at register R

**Dictionary/Files:**
- `\p` - Print dictionary
- `\a file.b4a` - Assemble file
- `\i file.b4i` - Interpret script
- `\d [path]` - Change directory

## Example Session

```javascript
// Load a B4 program
await b4i_load_image({ path: "bios/b4i.b4a" })

// Assemble some code
await b4i_assemble({
  address: "mylabel",
  bytes: "lb 42 lb 'e io rt"
})

// Check what was assembled
await b4i_query_memory({ address: "100" })

// Execute it
await b4i_execute({ command: "^mylabel" })

// Step through execution
await b4i_step()
await b4i_step()

// Check the stacks
await b4i_query_stack()
```

## Architecture

The server maintains a persistent child process running `pas/b4i`. Commands are sent via stdin and responses collected from stdout. This allows for:

- Continuous VM state across tool calls
- Label definitions that persist
- Debugging sessions that span multiple interactions
- Efficient execution without VM restart overhead

## Troubleshooting

**Server won't start:**
- Ensure `pas/b4i` is compiled (run `make` in the `pas` directory)
- Check that Node.js version is 18 or later

**Commands timeout or hang:**
- The server uses a simple 100ms timeout for responses
- Complex operations might need adjustment in the code

**Process exits unexpectedly:**
- Check stderr output for b4i errors
- Verify file paths are correct for `\a` and `\i` commands

## License

MIT
