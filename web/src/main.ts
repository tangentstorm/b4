import type { B4VM } from '@tangentstorm/b4';
import '@tangentstorm/b4/repl';         // registers <b4-repl>
import '@tangentstorm/b4/mem-browser';  // registers <b4-mem-browser>
import type { B4MemBrowser } from '@tangentstorm/b4/mem-browser';

// The <b4-repl> element creates its own B4PromiseWrapper internally.
// We extract the underlying B4VM so the memory browser can share it.
const replEl = document.querySelector('b4-repl')!;
const browser = document.querySelector('b4-mem-browser')! as B4MemBrowser;

// B4ReplCmpt.vm is a B4PromiseWrapper, which has .vm: B4VM
const wrapper = (replEl as any).vm;
const vm: B4VM = wrapper.vm;

browser.setVM(vm);
browser.scrollToIP();

// After every b4i command processed by the repl, refresh the browser.
const origB4i = vm.b4i.bind(vm);
vm.b4i = function (line: string) {
  origB4i(line);
  browser.refresh();
  browser.scrollToIP();
};

// When the mem-browser changes the VM (step/continue/edit), update the repl stacks.
browser.addEventListener('vm-changed', () => {
  (replEl as any).vm.fmtStacks().then(
    ({ cs, ds, ip }: { cs: string; ds: string; ip: string }) => {
      (replEl as any).updateStacks(cs, ds, ip);
    }
  );
});
