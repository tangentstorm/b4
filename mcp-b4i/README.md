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

### Multi-Instance Architecture

The MCP server v2.0 supports running **multiple B4 VM instances simultaneously**, each with different implementations (Pascal or JavaScript). This allows you to:

- Compare behavior between Pascal and JavaScript implementations side-by-side
- Run multiple independent VM sessions for different tests
- Switch between implementations on a per-call basis

Each instance has a name (e.g., `pas0`, `js0`, `mytest`) and can be specified when calling tools.

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

Or with the `claude mcp add` command:
```bash
claude mcp add b4i node /absolute/path/to/b4/mcp-b4i/index.js
```

### Manual Testing

You can test the server manually using the MCP inspector:

```bash
npx @modelcontextprotocol/inspector node index.js
```

## Available Tools

All tools now accept optional `instance` and `impl` parameters:

- **`instance`**: Name of the VM instance (e.g., `"pas0"`, `"js0"`, `"mytest"`)
  - If omitted, uses the default instance `"pas0"`
  - Will be created if it doesn't exist and `impl` is provided
- **`impl`**: Implementation to use (`"pas"` or `"js"`)
  - Only needed when creating a new instance
  - If provided without `instance`, auto-generates a name like `"pas0"`, `"js1"`, etc.

### `b4i_list_instances`
List all active B4i VM instances and their implementations.

**Example:**
```javascript
// See all running instances
await b4i_list_instances()
// Output: Active instances:
//         pas0 (pas)
//         js0 (js)
//         mytest (js)
```

### `b4i_execute`
Execute arbitrary B4i commands on a specific instance.

**Parameters:**
- `command`: B4i command(s) to execute
- `instance`: (optional) Instance name
- `impl`: (optional) Implementation for new instance

**Examples:**
```javascript
// Use default instance (pas0)
await b4i_execute({ command: "?d" })

// Use JavaScript implementation (creates js0)
await b4i_execute({ command: "?d", impl: "js" })

// Use named instance
await b4i_execute({ command: "?d", instance: "mytest", impl: "js" })

// Use existing instance
await b4i_execute({ command: "01 02 ad ?d", instance: "mytest" })
```

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

### Basic Usage (Default Instance)

```javascript
// All commands use the default 'pas0' instance
await b4i_execute({ command: ":100 c0 c1 ad rt" })
await b4i_step()
await b4i_query_stack()
```

### Comparing Implementations

```javascript
// Test the same code on both implementations
const testCode = ":100 c0 c1 ad rt";

// Run on Pascal
await b4i_execute({ command: testCode, instance: "pas0", impl: "pas" })
await b4i_step({ instance: "pas0" })
const pasResult = await b4i_query_stack({ instance: "pas0" })

// Run on JavaScript
await b4i_execute({ command: testCode, instance: "js0", impl: "js" })
await b4i_step({ instance: "js0" })
const jsResult = await b4i_query_stack({ instance: "js0" })

// Compare results
console.log("Pascal:", pasResult)
console.log("JavaScript:", jsResult)
```

### Multiple Test Sessions

```javascript
// Create named instances for different tests
await b4i_execute({
  command: ":test1 c0 c1 ad rt",
  instance: "test1",
  impl: "js"
})

await b4i_execute({
  command: ":test2 FF FF ad rt",
  instance: "test2",
  impl: "js"
})

// See all instances
await b4i_list_instances()

// Work with specific instances
await b4i_step({ instance: "test1" })
await b4i_step({ instance: "test2" })
```

## Architecture

The server maintains **multiple persistent child processes**, one for each VM instance. Each instance:

- Runs independently with its own memory, stacks, and registers
- Maintains state across tool calls (labels, assembled code persist)
- Can use either Pascal (`pas/b4i`) or JavaScript (`js/b4i.mjs`) implementation
- Auto-restarts if it crashes

Commands are sent via stdin and responses collected from stdout, allowing for efficient execution without restart overhead.

## Troubleshooting

**Server won't start:**
- Check that Node.js version is 18 or later
- For Pascal: Ensure `pas/b4i` is compiled (run `make` in the `pas` directory)
- For JavaScript: Ensure `js/b4i.mjs` exists

**Commands timeout or hang:**
- The server uses a simple 100ms timeout for responses
- Complex operations might need adjustment in the code

**Instance doesn't exist:**
- Use `b4i_list_instances` to see active instances
- Provide `impl` parameter when creating new instances
- Default instance `pas0` is created automatically on first use

**Process exits unexpectedly:**
- Check stderr output for b4i errors (tagged with instance name like `b4i[js0]`)
- Verify file paths are correct for `\a` and `\i` commands
- Instances auto-restart on crash

## License

MIT
