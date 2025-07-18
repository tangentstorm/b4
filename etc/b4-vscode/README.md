# B4 Syntax Highlighting for VS Code

This extension provides syntax highlighting for B4 assembler language files (`.b4a`).

## Features

- Syntax highlighting for B4 assembler opcodes
- Comment highlighting (lines starting with `#`)
- Character literals (single quote followed by one character, e.g., `'a`, `'A`, `' `)
- Preprocessor directives (lines starting with `.` or `%`)
- Function names (starting with `:`)
- Names without call (backtick followed by name, e.g., `` `@ ``, `` `A ``, `` `name ``)
- Register operations (two-character ops like `@A`, `!A`, `+A`, `^A` where A is any register)
- Hex constants (2 or more uppercase hex digits, e.g., `01`, `AB`, `ABCD`, `1234`)

## Installation

### From Source
1. Copy this directory to your VS Code extensions folder:
   - Windows: `%USERPROFILE%\.vscode\extensions\`
   - macOS: `~/.vscode/extensions/`
   - Linux: `~/.vscode/extensions/`

2. Restart VS Code

### Development Installation
1. Open VS Code
2. Press `Ctrl+Shift+P` (or `Cmd+Shift+P` on macOS)
3. Type "Extensions: Install from VSIX..."
4. Select this directory

## Usage

Files with `.b4a` extension will automatically use B4 syntax highlighting.

## Supported Syntax

Based on the Emacs b4a-mode, this extension highlights:

- **Comments**: Lines starting with `#`
- **Character literals**: Single quote followed by one character (e.g., `'a`, `'A`, `' `)
- **Preprocessor directives**: Lines starting with `.` or `%`
- **Function names**: Identifiers starting with `:`
- **Names without call**: Backtick followed by name (e.g., `` `@ ``, `` `A ``, `` `name ``)
- **Register operations**: Two-character ops like `@A`, `!A`, `+A`, `^A` where A is any register
- **Hex constants**: 2 or more uppercase hex digits (e.g., `01`, `AB`, `ABCD`, `1234`)
- **Opcode keywords**: All B4 assembler opcodes
- **Variables**: Special variable patterns

## Opcodes Supported

The extension recognizes all B4 assembler opcodes including:
- Arithmetic: `ad`, `sb`, `ml`, `dv`, `md`, `sh`
- Logic: `an`, `or`, `xr`, `nt`, `eq`, `lt`
- Stack: `du`, `sw`, `ov`, `zp`, `dc`, `cd`
- Memory: `rb`, `ri`, `wb`, `wi`, `lb`, `li`, `rs`, `ls`
- Control: `jm`, `hp`, `h0`, `cl`, `rt`, `nx`
- Constants: `c0`, `c1`, `c2`, `n1`, `c4`
- Register operations: `@A`, `!A`, `+A`, `^A` (where A is any register)
- And more...

## Register Operations

B4 supports register operations with the following pattern:
- `@A` - read register A
- `!A` - write to register A
- `+A` - add to register A
- `^A` - call word at address in register A

Where A can be any of: `@ABCDEFGHIJKLMNOPQRSTUVWXYZ^[\]_`

## Contributing

Feel free to submit issues or pull requests to improve the syntax highlighting.