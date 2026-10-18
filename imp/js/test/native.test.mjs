import { test } from 'node:test'
import assert from 'node:assert/strict'
import { B4VM } from '../dist/b4.mjs'
import { addNativeIo } from '../dist/b4-native.mjs'

const io = (vm, c, ...args) => { for (const a of args) vm.dput(a); vm.dput(c.charCodeAt(0)); vm.io(); return vm }

function spawn(vm, cmd) {
  vm._writeStr(0x300, cmd)
  return io(vm, 'p', 0x300).dpop()}

const poll = (vm, h) => io(vm, 'r', 0x400, h).dpop()

test('/C on one VM leaves the processes of another VM alone', () => {
  const a = new B4VM(), b = new B4VM()
  addNativeIo(a); addNativeIo(b)
  const ha = spawn(a, 'sleep 5'), hb = spawn(b, 'sleep 5')
  assert.equal(poll(a, ha), 0)
  assert.equal(poll(b, hb), 0)
  a.clear()
  assert.equal(poll(a, ha), -1, "a's process outlived its /C")
  assert.equal(poll(b, hb), 0, "a's /C killed b's process")
  io(b, 'k', hb)
  assert.equal(poll(b, hb), -1)})
