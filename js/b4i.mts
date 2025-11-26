#!/usr/bin/env node
import * as b4 from './b4.mjs'
import * as readline from 'readline'

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
})

function repl(): void {
  rl.question('', (line: string) => {
    try { b4.b4i(line) }
    catch (e) { console.error(e) }
    repl()
  })
}

repl()
