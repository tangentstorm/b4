#!/usr/bin/env node

import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
} from "@modelcontextprotocol/sdk/types.js";
import { spawn } from "child_process";
import { fileURLToPath } from "url";
import { dirname, join } from "path";
import { existsSync } from "fs";
import { platform } from "os";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Path to the b4i executable (relative to this script)
// On Windows, try both with and without .exe extension
let B4I_PATH = join(__dirname, "..", "pas", "b4i");
if (platform() === "win32" && !existsSync(B4I_PATH)) {
  const exePath = B4I_PATH + ".exe";
  if (existsSync(exePath)) {
    B4I_PATH = exePath;
  }
}

class B4iServer {
  constructor() {
    this.server = new Server(
      {
        name: "mcp-b4i",
        version: "1.0.0",
      },
      {
        capabilities: {
          tools: {},
        },
      }
    );

    this.b4iProcess = null;
    this.commandQueue = [];
    this.outputBuffer = "";
    this.setupToolHandlers();
    this.startB4i();

    // Handle cleanup
    process.on("SIGINT", () => this.cleanup());
    process.on("SIGTERM", () => this.cleanup());
  }

  startB4i() {
    // On Windows with Git Bash, we need to use bash -c to run the executable
    const isWindows = platform() === "win32";
    const command = isWindows ? "bash" : B4I_PATH;
    const args = isWindows ? ["-c", B4I_PATH.replace(/\\/g, "/")] : [];

    this.b4iProcess = spawn(command, args, {
      stdio: ["pipe", "pipe", "pipe"],
    });

    this.b4iProcess.stdout.on("data", (data) => {
      this.outputBuffer += data.toString();
    });

    this.b4iProcess.stderr.on("data", (data) => {
      console.error(`b4i stderr: ${data}`);
    });

    this.b4iProcess.on("close", (code) => {
      console.error(`b4i process exited with code ${code}`);
      this.b4iProcess = null;
    });
  }

  async executeCommand(command) {
    // Auto-restart the process if it crashed
    if (!this.b4iProcess) {
      console.error("b4i process was not running, restarting...");
      this.startB4i();
      // Give it a moment to start
      await new Promise((resolve) => setTimeout(resolve, 200));
    }

    if (!this.b4iProcess) {
      throw new Error("b4i process failed to start");
    }

    // Clear output buffer
    this.outputBuffer = "";

    // Send command
    this.b4iProcess.stdin.write(command + "\n");

    // Wait for output (simple timeout-based approach)
    await new Promise((resolve) => setTimeout(resolve, 100));

    const output = this.outputBuffer.trim();
    this.outputBuffer = "";
    return output;
  }

  setupToolHandlers() {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        {
          name: "b4i_execute",
          description:
            "Execute arbitrary B4i commands. Can send multiple commands separated by spaces. Commands include VM state queries (?d, ?c, ?i, ?R, ?addr), assembly (:addr bytes), calculator mode (hex numbers, opcodes), and control (%s step, %q quit, %C clear, %R reset, \\g go).",
          inputSchema: {
            type: "object",
            properties: {
              command: {
                type: "string",
                description:
                  "The B4i command(s) to execute. Examples: '?d' (show data stack), '01 02 ad ?d' (push 1, 2, add, show stack), ':100 AA BB CC' (assemble bytes at 0x100), '%s' (step), '?100' (dump memory at 0x100)",
              },
            },
            required: ["command"],
          },
        },
        {
          name: "b4i_load_image",
          description:
            "Load a B4X binary image file into the VM memory. This uses the \\i command to load and execute a B4i script or binary.",
          inputSchema: {
            type: "object",
            properties: {
              path: {
                type: "string",
                description: "Path to the .b4x, .b4a, or .b4i file to load",
              },
            },
            required: ["path"],
          },
        },
        {
          name: "b4i_query_stack",
          description:
            "Query the VM stacks. Returns both data stack (ds) and control stack (cs).",
          inputSchema: {
            type: "object",
            properties: {},
          },
        },
        {
          name: "b4i_query_memory",
          description:
            "Dump 16 bytes of memory starting at the specified address.",
          inputSchema: {
            type: "object",
            properties: {
              address: {
                type: "string",
                description:
                  "Hexadecimal address to dump (e.g., '100', '1000', 'DEADBEEF')",
              },
            },
            required: ["address"],
          },
        },
        {
          name: "b4i_assemble",
          description:
            "Assemble bytecode at a specific memory address. Can define labels with :label syntax.",
          inputSchema: {
            type: "object",
            properties: {
              address: {
                type: "string",
                description:
                  "Address or label name (e.g., '100' for address 0x100, or 'mylabel' to create a new label)",
              },
              bytes: {
                type: "string",
                description:
                  "Space-separated hex bytes or mnemonics to assemble (e.g., 'AA BB CC', 'lb 42 rt', 'c0 c1 ad rt')",
              },
            },
            required: ["address", "bytes"],
          },
        },
        {
          name: "b4i_step",
          description:
            "Execute a single instruction at the current instruction pointer. Returns the new instruction pointer and stack state.",
          inputSchema: {
            type: "object",
            properties: {},
          },
        },
        {
          name: "b4i_reset",
          description:
            "Reset the VM to initial state. Clears stacks and sets IP to 0x100. Does not clear memory.",
          inputSchema: {
            type: "object",
            properties: {},
          },
        },
        {
          name: "b4i_clear",
          description:
            "Clear the VM completely. Resets stacks, IP, and clears all memory.",
          inputSchema: {
            type: "object",
            properties: {},
          },
        },
        {
          name: "b4i_register",
          description:
            "Read or write a VM register (A-Z, except some reserved). Registers are used to store addresses and can be invoked with ^R syntax.",
          inputSchema: {
            type: "object",
            properties: {
              register: {
                type: "string",
                description: "Single letter register name (A-Z)",
              },
              value: {
                type: "string",
                description:
                  "Optional hex value to write to register. If omitted, reads the register.",
              },
            },
            required: ["register"],
          },
        },
        {
          name: "b4i_send_input",
          description:
            "Send input directly to the b4i process stdin. Useful for testing interactive programs or sending commands to a running VM.",
          inputSchema: {
            type: "object",
            properties: {
              input: {
                type: "string",
                description: "The input to send to b4i stdin",
              },
            },
            required: ["input"],
          },
        },
      ],
    }));

    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;

      try {
        let result;

        switch (name) {
          case "b4i_execute": {
            result = await this.executeCommand(args.command);
            break;
          }

          case "b4i_load_image": {
            // For loading images, we use the \a command for assembly files
            // or direct binary loading for .b4x files
            const ext = args.path.split(".").pop().toLowerCase();
            if (ext === "b4a") {
              result = await this.executeCommand(`\\a ${args.path}`);
            } else if (ext === "b4i") {
              result = await this.executeCommand(`\\i ${args.path}`);
            } else {
              throw new Error(
                `Unsupported file type: ${ext}. Use .b4a or .b4i files.`
              );
            }
            break;
          }

          case "b4i_query_stack": {
            const dsResult = await this.executeCommand("?d");
            const csResult = await this.executeCommand("?c");
            result = `${dsResult}\n${csResult}`;
            break;
          }

          case "b4i_query_memory": {
            result = await this.executeCommand(`?${args.address}`);
            break;
          }

          case "b4i_assemble": {
            result = await this.executeCommand(
              `:${args.address} ${args.bytes}`
            );
            // Show the assembled memory
            const verifyResult = await this.executeCommand(`?${args.address}`);
            result = verifyResult || "Assembled successfully";
            break;
          }

          case "b4i_step": {
            await this.executeCommand("%s");
            const ipResult = await this.executeCommand("?i");
            const dsResult = await this.executeCommand("?d");
            result = `${ipResult}\n${dsResult}`;
            break;
          }

          case "b4i_reset": {
            result = await this.executeCommand("%R");
            const ipResult = await this.executeCommand("?i");
            result = `Reset complete\n${ipResult}`;
            break;
          }

          case "b4i_clear": {
            result = await this.executeCommand("%C");
            result = "VM cleared successfully";
            break;
          }

          case "b4i_register": {
            if (args.value) {
              // Write to register
              result = await this.executeCommand(`${args.value} !${args.register}`);
              // Verify
              const verifyResult = await this.executeCommand(
                `?${args.register}`
              );
              result = `Register ${args.register} set\n${verifyResult}`;
            } else {
              // Read register
              result = await this.executeCommand(`?${args.register}`);
            }
            break;
          }

          case "b4i_send_input": {
            if (!this.b4iProcess) {
              throw new Error("b4i process is not running");
            }
            // Send input directly to stdin
            this.b4iProcess.stdin.write(args.input + "\n");
            // Wait a bit for any output
            await new Promise((resolve) => setTimeout(resolve, 100));
            result = this.outputBuffer.trim();
            this.outputBuffer = "";
            break;
          }

          default:
            throw new Error(`Unknown tool: ${name}`);
        }

        return {
          content: [
            {
              type: "text",
              text: result || "(no output)",
            },
          ],
        };
      } catch (error) {
        return {
          content: [
            {
              type: "text",
              text: `Error: ${error.message}`,
            },
          ],
          isError: true,
        };
      }
    });
  }

  cleanup() {
    if (this.b4iProcess) {
      this.b4iProcess.kill();
      this.b4iProcess = null;
    }
    process.exit(0);
  }

  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("B4i MCP server running on stdio");
  }
}

const server = new B4iServer();
server.run().catch(console.error);
