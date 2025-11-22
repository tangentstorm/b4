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

// Paths to different b4i implementations
const B4I_PATHS = {
  pas: join(__dirname, "..", "pas", "b4i"),
  js: join(__dirname, "..", "js", "b4i.mjs"),
};

class B4iInstance {
  constructor(name, impl) {
    this.name = name;
    this.impl = impl;
    this.process = null;
    this.outputBuffer = "";
    this.start();
  }

  start() {
    const implPath = B4I_PATHS[this.impl];
    if (!implPath) {
      throw new Error(`Unknown implementation: ${this.impl}. Valid values: pas, js`);
    }

    // Check if implementation exists
    if (this.impl === "pas" && !existsSync(implPath)) {
      const errMsg = `Pascal binary not found at ${implPath}. Build it with 'make -C pas' or use impl: "js" instead.`;
      console.error(`b4i[${this.name}] ${errMsg}`);
      this.startError = errMsg;
      return;
    }

    let command, args;
    if (this.impl === "js") {
      command = "node";
      args = [implPath];
    } else {
      // Pascal implementation
      const isWindows = platform() === "win32";
      let path = implPath;
      // On Windows, try .exe extension
      if (isWindows && !existsSync(path)) {
        const exePath = path + ".exe";
        if (existsSync(exePath)) {
          path = exePath;
        }
      }
      command = isWindows ? "bash" : path;
      args = isWindows ? ["-c", path.replace(/\\/g, "/")] : [];
    }

    this.process = spawn(command, args, {
      stdio: ["pipe", "pipe", "pipe"],
    });

    this.process.on("error", (error) => {
      console.error(`b4i[${this.name}] spawn error: ${error.message}`);
      this.startError = error.message;
      this.process = null;
    });

    this.process.stdout.on("data", (data) => {
      this.outputBuffer += data.toString();
    });

    this.process.stderr.on("data", (data) => {
      console.error(`b4i[${this.name}] stderr: ${data}`);
    });

    this.process.on("close", (code) => {
      console.error(`b4i[${this.name}] process exited with code ${code}`);
      this.process = null;
    });
  }

  async executeCommand(command) {
    // Auto-restart if crashed
    if (!this.process) {
      console.error(`b4i[${this.name}] was not running, restarting...`);
      this.start();
      await new Promise((resolve) => setTimeout(resolve, 200));
    }

    if (!this.process) {
      if (this.startError) {
        throw new Error(`b4i[${this.name}] failed to start: ${this.startError}`);
      }
      throw new Error(`b4i[${this.name}] failed to start`);
    }

    // Clear output buffer
    this.outputBuffer = "";

    // Send command
    this.process.stdin.write(command + "\n");

    // Wait for output
    await new Promise((resolve) => setTimeout(resolve, 100));

    const output = this.outputBuffer.trim();
    this.outputBuffer = "";
    return output;
  }

  cleanup() {
    if (this.process) {
      this.process.kill();
      this.process = null;
    }
  }
}

class B4iServer {
  constructor() {
    this.server = new Server(
      {
        name: "mcp-b4i",
        version: "2.0.0",
      },
      {
        capabilities: {
          tools: {},
        },
      }
    );

    this.instances = new Map(); // name -> B4iInstance
    this.nextIds = { pas: 0, js: 0 }; // auto-incrementing IDs per implementation
    this.setupToolHandlers();

    // Handle cleanup
    process.on("SIGINT", () => this.cleanup());
    process.on("SIGTERM", () => this.cleanup());
  }

  resolveInstance(args) {
    // If instance name is provided, use it or create it
    if (args.instance) {
      if (this.instances.has(args.instance)) {
        return this.instances.get(args.instance);
      }
      // Create new instance if impl is provided
      if (args.impl) {
        const instance = new B4iInstance(args.instance, args.impl);
        this.instances.set(args.instance, instance);
        return instance;
      }
      throw new Error(`Instance '${args.instance}' does not exist. Provide 'impl' parameter to create it.`);
    }

    // If impl is provided without instance name, auto-generate name
    if (args.impl) {
      const name = this.generateInstanceName(args.impl);
      const instance = new B4iInstance(name, args.impl);
      this.instances.set(name, instance);
      return instance;
    }

    // Default: use or create 'pas0'
    if (this.instances.has('pas0')) {
      return this.instances.get('pas0');
    }
    const instance = new B4iInstance('pas0', 'pas');
    this.instances.set('pas0', instance);
    return instance;
  }

  generateInstanceName(impl) {
    const id = this.nextIds[impl]++;
    return `${impl}${id}`;
  }

  setupToolHandlers() {
    // Helper to add common instance/impl parameters to a schema
    const addInstanceParams = (schema) => {
      return {
        ...schema,
        properties: {
          instance: {
            type: "string",
            description: "Instance name (e.g., 'pas0', 'js0', 'mytest'). If omitted, uses default 'pas0'. Will be created if it doesn't exist and 'impl' is specified.",
          },
          impl: {
            type: "string",
            enum: ["pas", "js"],
            description: "Implementation to use ('pas' for Pascal, 'js' for JavaScript). Only needed when creating a new instance.",
          },
          ...schema.properties,
        },
      };
    };

    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        {
          name: "b4i_list_instances",
          description:
            "List all active B4i VM instances showing their names and implementations.",
          inputSchema: {
            type: "object",
            properties: {},
          },
        },
        {
          name: "b4i_execute",
          description:
            "Execute arbitrary B4i commands on a specific instance.",
          inputSchema: addInstanceParams({
            type: "object",
            properties: {
              command: {
                type: "string",
                description:
                  "The B4i command(s) to execute.",
              },
            },
            required: ["command"],
          }),
        },
        {
          name: "b4i_load_image",
          description:
            "Load a B4 assembly or script file into an instance.",
          inputSchema: addInstanceParams({
            type: "object",
            properties: {
              path: {
                type: "string",
                description: "Path to the .b4a or .b4i file to load",
              },
            },
            required: ["path"],
          }),
        },
        {
          name: "b4i_query_stack",
          description:
            "Query the data and control stacks of an instance.",
          inputSchema: addInstanceParams({
            type: "object",
            properties: {},
          }),
        },
        {
          name: "b4i_query_memory",
          description:
            "Dump 16 bytes of memory from an instance.",
          inputSchema: addInstanceParams({
            type: "object",
            properties: {
              address: {
                type: "string",
                description:
                  "Hexadecimal address to dump",
              },
            },
            required: ["address"],
          }),
        },
        {
          name: "b4i_assemble",
          description:
            "Assemble bytecode at a specific memory address in an instance.",
          inputSchema: addInstanceParams({
            type: "object",
            properties: {
              address: {
                type: "string",
                description:
                  "Address or label name",
              },
              bytes: {
                type: "string",
                description:
                  "Space-separated hex bytes or mnemonics to assemble",
              },
            },
            required: ["address", "bytes"],
          }),
        },
        {
          name: "b4i_step",
          description:
            "Execute a single instruction in an instance.",
          inputSchema: addInstanceParams({
            type: "object",
            properties: {},
          }),
        },
        {
          name: "b4i_reset",
          description:
            "Reset an instance (clears stacks, sets IP to 0x100).",
          inputSchema: addInstanceParams({
            type: "object",
            properties: {},
          }),
        },
        {
          name: "b4i_clear",
          description:
            "Clear an instance completely (stacks, memory, IP).",
          inputSchema: addInstanceParams({
            type: "object",
            properties: {},
          }),
        },
        {
          name: "b4i_register",
          description:
            "Read or write a VM register in an instance.",
          inputSchema: addInstanceParams({
            type: "object",
            properties: {
              register: {
                type: "string",
                description: "Single letter register name (A-Z)",
              },
              value: {
                type: "string",
                description:
                  "Optional hex value to write to register",
              },
            },
            required: ["register"],
          }),
        },
        {
          name: "b4i_send_input",
          description:
            "Send input directly to an instance's stdin.",
          inputSchema: addInstanceParams({
            type: "object",
            properties: {
              input: {
                type: "string",
                description: "The input to send",
              },
            },
            required: ["input"],
          }),
        },
      ],
    }));

    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;

      try {
        let result;

        switch (name) {
          case "b4i_list_instances": {
            const instances = Array.from(this.instances.entries()).map(
              ([name, inst]) => `${name} (${inst.impl})`
            );
            result = instances.length > 0
              ? `Active instances:\n${instances.join('\n')}`
              : "No active instances";
            break;
          }

          case "b4i_execute": {
            const instance = this.resolveInstance(args);
            result = await instance.executeCommand(args.command);
            result = `[${instance.name}] ${result}`;
            break;
          }

          case "b4i_load_image": {
            const instance = this.resolveInstance(args);
            const ext = args.path.split(".").pop().toLowerCase();
            if (ext === "b4a") {
              result = await instance.executeCommand(`\\a ${args.path}`);
            } else if (ext === "b4i") {
              result = await instance.executeCommand(`\\i ${args.path}`);
            } else {
              throw new Error(
                `Unsupported file type: ${ext}. Use .b4a or .b4i files.`
              );
            }
            result = `[${instance.name}] ${result}`;
            break;
          }

          case "b4i_query_stack": {
            const instance = this.resolveInstance(args);
            const dsResult = await instance.executeCommand("?d");
            const csResult = await instance.executeCommand("?c");
            result = `[${instance.name}]\n${dsResult}\n${csResult}`;
            break;
          }

          case "b4i_query_memory": {
            const instance = this.resolveInstance(args);
            result = await instance.executeCommand(`?${args.address}`);
            result = `[${instance.name}] ${result}`;
            break;
          }

          case "b4i_assemble": {
            const instance = this.resolveInstance(args);
            result = await instance.executeCommand(
              `:${args.address} ${args.bytes}`
            );
            const verifyResult = await instance.executeCommand(`?${args.address}`);
            result = `[${instance.name}] ${verifyResult || "Assembled successfully"}`;
            break;
          }

          case "b4i_step": {
            const instance = this.resolveInstance(args);
            await instance.executeCommand("%s");
            const ipResult = await instance.executeCommand("?i");
            const dsResult = await instance.executeCommand("?d");
            result = `[${instance.name}]\n${ipResult}\n${dsResult}`;
            break;
          }

          case "b4i_reset": {
            const instance = this.resolveInstance(args);
            result = await instance.executeCommand("%R");
            const ipResult = await instance.executeCommand("?i");
            result = `[${instance.name}] Reset complete\n${ipResult}`;
            break;
          }

          case "b4i_clear": {
            const instance = this.resolveInstance(args);
            result = await instance.executeCommand("%C");
            result = `[${instance.name}] VM cleared successfully`;
            break;
          }

          case "b4i_register": {
            const instance = this.resolveInstance(args);
            if (args.value) {
              result = await instance.executeCommand(`${args.value} !${args.register}`);
              const verifyResult = await instance.executeCommand(
                `?${args.register}`
              );
              result = `[${instance.name}] Register ${args.register} set\n${verifyResult}`;
            } else {
              result = await instance.executeCommand(`?${args.register}`);
              result = `[${instance.name}] ${result}`;
            }
            break;
          }

          case "b4i_send_input": {
            const instance = this.resolveInstance(args);
            if (!instance.process) {
              throw new Error("b4i process is not running");
            }
            instance.process.stdin.write(args.input + "\n");
            await new Promise((resolve) => setTimeout(resolve, 100));
            result = instance.outputBuffer.trim();
            instance.outputBuffer = "";
            result = `[${instance.name}] ${result}`;
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
    for (const instance of this.instances.values()) {
      instance.cleanup();
    }
    process.exit(0);
  }

  async run() {
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error("B4i MCP server v2.0 running on stdio");
  }
}

const server = new B4iServer();
server.run().catch(console.error);
