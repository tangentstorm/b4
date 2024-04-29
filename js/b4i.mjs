#!/bin/node
import * as b4 from './b4.mjs'
import * as readline from "readline"

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout })

function repl() {
  rl.question('', line=>{
    b4.b4i(line, rl)
    repl()})}

repl()
