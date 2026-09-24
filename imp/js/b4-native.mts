// b4-native: node.js-specific extensions for b4 VM
// adds process spawning, stdin/stdout, and process.exit support

import { spawn, ChildProcess } from 'child_process'
import * as fs from 'fs'
import { B4VM } from './b4.mjs'

interface ProcEntry { proc: ChildProcess; inbuf: string }

let nextProcId = 0
const procs: Map<number, ProcEntry> = new Map()

export function addNativeIo(vm: B4VM): void {
  vm.onBye = () => process.exit(0)

  vm.addIo('o', (vm) => { // output character directly to stdout
    process.stdout.write(vm._dpopChar())
  })

  vm.addIo('i', (vm) => { // read line from stdin: (dest-ptr maxlen -- actual-len)
    let maxlen = vm.dpop()
    let dest = vm.dpop()
    try {
      let buf = Buffer.alloc(maxlen + 1)
      let n = fs.readSync(0, buf, 0, maxlen, null)
      let line = buf.toString('utf8', 0, n).replace(/\n$/, '')
      let len = Math.min(line.length, maxlen)
      for (let i = 0; i < len; i++) vm.ram[dest + i] = line.charCodeAt(i)
      vm.ram[dest + len] = 0
      vm.dput(len)
    } catch {
      vm.dput(0)
    }
  })

  vm.addIo('p', (vm) => { // spawn process: (cmdline-ptr -- handle|-1)
    let cmdAddr = vm.dpop()
    let cmd = vm._readStr(cmdAddr)
    try {
      let proc = spawn('sh', ['-c', cmd], {
        stdio: ['pipe', 'pipe', 'pipe']
      })
      let id = nextProcId++
      let entry: ProcEntry = {proc, inbuf: ''}
      proc.stdout!.on('data', (data: Buffer) => { entry.inbuf += data.toString() })
      procs.set(id, entry)
      vm.dput(id)
    } catch {
      vm.dput(-1)
    }
  })

  vm.addIo('w', (vm) => { // write line to process: (line-ptr handle --)
    let handle = vm.dpop()
    let lineAddr = vm.dpop()
    let entry = procs.get(handle)
    if (entry && entry.proc.stdin) {
      let line = vm._readStr(lineAddr)
      entry.proc.stdin.write(line + '\n')
    }
  })

  vm.addIo('r', (vm) => { // read line from process: (dest-ptr handle -- length|-1)
    let handle = vm.dpop()
    let dest = vm.dpop()
    let entry = procs.get(handle)
    if (entry) {
      let nlIdx = entry.inbuf.indexOf('\n')
      if (nlIdx >= 0) {
        let line = entry.inbuf.substring(0, nlIdx)
        entry.inbuf = entry.inbuf.substring(nlIdx + 1)
        let len = line.length
        for (let i = 0; i < len; i++) vm.ram[dest + i] = line.charCodeAt(i)
        vm.ram[dest + len] = 0
        vm.dput(len)
      } else {
        vm.dput(0)
      }
    } else {
      vm.dput(-1)
    }
  })

  vm.addIo('k', (vm) => { // kill process: (handle --)
    let handle = vm.dpop()
    let entry = procs.get(handle)
    if (entry) {
      entry.proc.kill()
      procs.delete(handle)
    }
  })
}

const vm = new B4VM()
addNativeIo(vm)

export { B4VM }
export { vm }
export function b4i(line: string): void { vm.b4i(line) }
