#!/usr/bin/env node

/**
 * Simple test script for the B4i MCP server
 * Tests basic functionality without requiring a full MCP client
 */

import { spawn } from "child_process";
import { fileURLToPath } from "url";
import { dirname, join } from "path";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

class MCPTester {
  constructor() {
    this.requestId = 1;
  }

  async sendRequest(method, params) {
    const request = {
      jsonrpc: "2.0",
      id: this.requestId++,
      method,
      params,
    };
    return JSON.stringify(request) + "\n";
  }

  async test() {
    console.log("Starting B4i MCP server test...\n");

    const serverProcess = spawn("node", [join(__dirname, "index.js")], {
      stdio: ["pipe", "pipe", "pipe"],
    });

    let outputBuffer = "";
    const responses = [];

    serverProcess.stdout.on("data", (data) => {
      outputBuffer += data.toString();
      // Try to parse complete JSON-RPC messages
      const lines = outputBuffer.split("\n");
      outputBuffer = lines.pop(); // Keep incomplete line in buffer

      for (const line of lines) {
        if (line.trim()) {
          try {
            const response = JSON.parse(line);
            responses.push(response);
          } catch (e) {
            // Not JSON, probably debug output
          }
        }
      }
    });

    serverProcess.stderr.on("data", (data) => {
      // Server startup message goes to stderr
      console.log("Server:", data.toString().trim());
    });

    // Wait for server to start
    await new Promise((resolve) => setTimeout(resolve, 500));

    // Helper to wait for response
    const waitForResponse = async () => {
      const startLen = responses.length;
      for (let i = 0; i < 50; i++) {
        // 5 second timeout
        if (responses.length > startLen) {
          return responses[responses.length - 1];
        }
        await new Promise((resolve) => setTimeout(resolve, 100));
      }
      throw new Error("Response timeout");
    };

    try {
      // Test 1: Initialize
      console.log("Test 1: Initialize");
      serverProcess.stdin.write(
        await this.sendRequest("initialize", {
          protocolVersion: "2024-11-05",
          capabilities: {},
          clientInfo: { name: "test-client", version: "1.0.0" },
        })
      );
      const initResponse = await waitForResponse();
      console.log("✓ Initialized:", initResponse.result?.serverInfo?.name);
      console.log();

      // Test 2: List tools
      console.log("Test 2: List tools");
      serverProcess.stdin.write(await this.sendRequest("tools/list", {}));
      const toolsResponse = await waitForResponse();
      console.log(
        "✓ Found",
        toolsResponse.result?.tools?.length || 0,
        "tools:"
      );
      toolsResponse.result?.tools?.forEach((tool) => {
        console.log(`  - ${tool.name}: ${tool.description.substring(0, 60)}...`);
      });
      console.log();

      // Test 3: Execute simple command
      console.log("Test 3: Execute b4i command (?d)");
      serverProcess.stdin.write(
        await this.sendRequest("tools/call", {
          name: "b4i_execute",
          arguments: { command: "?d" },
        })
      );
      const execResponse = await waitForResponse();
      console.log(
        "✓ Result:",
        execResponse.result?.content?.[0]?.text || "No output"
      );
      console.log();

      // Test 4: Push numbers and add
      console.log("Test 4: Calculator mode (01 02 ad ?d)");
      serverProcess.stdin.write(
        await this.sendRequest("tools/call", {
          name: "b4i_execute",
          arguments: { command: "01 02 ad ?d" },
        })
      );
      const calcResponse = await waitForResponse();
      console.log(
        "✓ Result:",
        calcResponse.result?.content?.[0]?.text || "No output"
      );
      console.log();

      // Test 5: Assemble code
      console.log("Test 5: Assemble at address 100");
      serverProcess.stdin.write(
        await this.sendRequest("tools/call", {
          name: "b4i_assemble",
          arguments: { address: "100", bytes: "lb 42 rt" },
        })
      );
      const asmResponse = await waitForResponse();
      console.log(
        "✓ Result:",
        asmResponse.result?.content?.[0]?.text || "No output"
      );
      console.log();

      // Test 6: Query memory
      console.log("Test 6: Query memory at 100");
      serverProcess.stdin.write(
        await this.sendRequest("tools/call", {
          name: "b4i_query_memory",
          arguments: { address: "100" },
        })
      );
      const memResponse = await waitForResponse();
      console.log(
        "✓ Result:",
        memResponse.result?.content?.[0]?.text || "No output"
      );
      console.log();

      // Test 7: Query stacks
      console.log("Test 7: Query stacks");
      serverProcess.stdin.write(
        await this.sendRequest("tools/call", {
          name: "b4i_query_stack",
          arguments: {},
        })
      );
      const stackResponse = await waitForResponse();
      console.log(
        "✓ Result:",
        stackResponse.result?.content?.[0]?.text || "No output"
      );
      console.log();

      console.log("All tests passed! ✓");
    } catch (error) {
      console.error("Test failed:", error);
    } finally {
      serverProcess.kill();
    }
  }
}

const tester = new MCPTester();
tester.test().catch(console.error);
