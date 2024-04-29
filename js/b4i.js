#!/bin/node
import { B4 } from './b4.mjs'

const readline = require("readline")

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout })

const prompt = ''
const out = console.log


const b4 = new B4.CPU()

const hex=b=>b.toString(16,0).toUpperCase()

function dump(xs) {
  return `[${xs.map(hex).join(' ')}]` }

function b4i(line){
  if (line.startsWith('!')) {
    let [a0,...xs] = line.split(/\s+/)
    let a = parseInt(a0.slice(1), 16)
    for (let x of xs) { b4.ram[a++]=asm(x) }
    return }
  else for (let tok of line.split(' ')) {
    switch (tok) {
    case '?c': out(`cs: ${dump(b4.cs)}`); break;
    case '?d': out(`ds: ${dump(b4.ds)}`); break;
    case '?i': out(`ip: ${hex(b4.ip)}`); break;
    case '%q': process.exit(1); break;
    case '%s': b4.step(); break;
    default:
      if (tok.match(/^[0-9A-F]+$/)){
        b4.ds.push(parseInt(tok,16))}
      else if (tok[0]=="'") {
        if (tok.length==2) b4.ds.push(tok[1].charCodeAt(0))
        else if (tok.length==1) b4.ds.push(32)
        else out(`unknown command: ${tok}\n`)}
      else if (tok[0]=="`" && tok.length==2) {
        b4.ds.push(4*(tok[1].charCodeAt(0)-64))}
      else if (tok in b4) { b4[tok]() }
      else if (tok[0]=="@") { out(b4.peek(parseInt(tok.slice(1),16), 16))  }
      else out(`unknown command: ${tok}\n`)}}}

function repl() {
  rl.question(prompt, line=>{
    b4i(line, rl)
    repl()})}

repl()
