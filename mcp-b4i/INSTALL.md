# Installation Guide for B4i MCP Server

## Quick Start

### 1. Install Dependencies

```bash
cd mcp-b4i
npm install
```

### 2. Make Executable (Unix/Linux/Mac only)

```bash
chmod +x index.js
```

### 3. Configure Claude Desktop

#### macOS

Edit `~/Library/Application Support/Claude/claude_desktop_config.json`:

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

#### Windows

Edit `%APPDATA%\Claude\claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "b4i": {
      "command": "node",
      "args": ["D:\\absolute\\path\\to\\b4\\mcp-b4i\\index.js"]
    }
  }
}
```

**Note:** On Windows, make sure you have Git Bash installed, as the server uses `bash` to run the `pas/b4i` executable.

#### Linux

Edit `~/.config/Claude/claude_desktop_config.json`:

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

### 4. Restart Claude Desktop

After editing the configuration, completely quit and restart Claude Desktop for the changes to take effect.

### 5. Verify Installation

In Claude Desktop, start a new conversation and ask:

> Can you list the available MCP tools?

You should see 9 B4i-related tools:
- `b4i_execute`
- `b4i_load_image`
- `b4i_query_stack`
- `b4i_query_memory`
- `b4i_assemble`
- `b4i_step`
- `b4i_reset`
- `b4i_clear`
- `b4i_register`

## Testing Without Claude Desktop

You can test the server directly using the included test script:

```bash
cd mcp-b4i
node test.js
```

You should see output like:
```
Starting B4i MCP server test...
Server: B4i MCP server running on stdio
Test 1: Initialize
✓ Initialized: mcp-b4i
Test 2: List tools
✓ Found 9 tools:
...
All tests passed! ✓
```

## Using the MCP Inspector

The MCP SDK includes an inspector tool for interactive testing:

```bash
npx @modelcontextprotocol/inspector node index.js
```

This will open a web interface where you can:
- View all available tools
- Test tool calls interactively
- See real-time communication between client and server
- Debug issues

## Troubleshooting

### Server Not Appearing in Claude Desktop

1. **Check the config file path**
   - Make sure you edited the correct `claude_desktop_config.json` file
   - File locations vary by OS (see above)

2. **Check the JSON syntax**
   - Use a JSON validator to ensure no syntax errors
   - Common mistakes: missing commas, wrong quotes, unescaped backslashes on Windows

3. **Check the absolute path**
   - Must be an absolute path, not relative
   - On Windows, use double backslashes (`\\`) or forward slashes (`/`)

4. **Check Node.js installation**
   - Run `node --version` to verify Node.js is installed and in PATH
   - Minimum version: Node.js 18

5. **Restart Claude Desktop completely**
   - On Mac: Quit the app (Cmd+Q), don't just close the window
   - On Windows: Exit from system tray
   - On Linux: Kill all Claude processes

### "bash: command not found" (Windows)

The server requires Git Bash on Windows. Install it from:
https://git-scm.com/download/win

Make sure `bash` is in your system PATH.

### "pas/b4i not found"

Make sure the B4i interpreter is compiled:

```bash
cd pas
make b4i
```

Or compile manually:
```bash
cd pas
fpc b4i.pas
```

### Commands Timeout

If commands seem to hang or timeout:

1. Test the `pas/b4i` executable directly:
   ```bash
   echo "?d %q" | pas/b4i
   ```

2. Increase the timeout in `index.js` (currently 100ms):
   ```javascript
   await new Promise((resolve) => setTimeout(resolve, 100));
   ```
   Change to 200ms or more if needed.

### Server Crashes

Check the Claude Desktop logs:
- **macOS**: `~/Library/Logs/Claude/`
- **Windows**: `%APPDATA%\Claude\logs\`
- **Linux**: `~/.config/Claude/logs/`

Look for error messages related to the MCP server.

## Example Usage in Claude Desktop

Once configured, you can ask Claude to:

### Run B4 Code

> Can you assemble and run this B4 code:
> ```
> :mylabel
>   lb 42
>   du
>   ad
>   rt
> ```

### Debug a Program

> Load the file `bios/b4i.b4a` and step through the first 5 instructions

### Inspect VM State

> What's currently on the data stack? And show me memory at address 0x100

### Calculate

> Use the B4 VM as a calculator: what's 0x42 + 0x38 in hex?

## Uninstalling

1. Remove the `b4i` entry from `claude_desktop_config.json`
2. Restart Claude Desktop
3. Delete the `mcp-b4i` directory

## Getting Help

- See [README.md](./README.md) for usage examples
- Check the [B4i documentation](../doc/b4i.org) for command reference
- Review the [test files](../b4-tests.org) for B4 VM examples
- Open an issue in the B4 repository
