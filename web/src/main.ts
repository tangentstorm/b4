import type { B4VM } from '@tangentstorm/b4';
import '@tangentstorm/b4/repl';         // registers <b4-repl>
import '@tangentstorm/b4/mem-browser';  // registers <b4-mem-browser>
import type { B4MemBrowser } from '@tangentstorm/b4/mem-browser';
import { initVG } from './b4-vg.mts';
import './b4-snippets';
import type { B4Snippets } from './b4-snippets';
import './b4-grid-editor';
import './b4-palette-editor';
import './b4-help';

// The <b4-repl> element creates its own B4PromiseWrapper internally.
// We extract the underlying B4VM so the memory browser can share it.
const replEl = document.querySelector('b4-repl')!;
const browser = document.querySelector('b4-mem-browser')! as B4MemBrowser;

// B4ReplCmpt.vm is a B4PromiseWrapper, which has .vm: B4VM
const wrapper = (replEl as any).vm;
const vm: B4VM = wrapper.vm;

browser.setVM(vm);
browser.scrollToIP();

const vg = initVG(vm);

// Tab switching for bottom panel
const tabMap: Record<string, string> = {
  debugger: 'b4-mem-browser',
  snippets: 'b4-snippets',
  sprites: 'b4-grid-editor[mode="sprite"]',
  map: 'b4-grid-editor[mode="map"]',
  palette: 'b4-palette-editor',
  help: 'b4-help',
};
function switchTab(tab: string) {
  document.querySelectorAll('#bottom-tabs button').forEach(b => {
    b.classList.toggle('active', b.getAttribute('data-tab') === tab);
  });
  const content = document.querySelector('#bottom-content')!;
  for (const [key, sel] of Object.entries(tabMap)) {
    content.querySelector(sel)!.classList.toggle('hidden', tab !== key);
  }
  localStorage.setItem('b4-active-tab', tab);
}
const savedTab = localStorage.getItem('b4-active-tab');
if (savedTab && tabMap[savedTab]) switchTab(savedTab);
document.querySelectorAll('#bottom-tabs button').forEach(btn => {
  btn.addEventListener('click', () => switchTab(btn.getAttribute('data-tab')!));
});

// Play/Pause toggle in tab bar
const btnPP = document.querySelector('#btn-playpause')! as HTMLButtonElement;
vg.onStateChange = (anim: boolean) => {
  btnPP.textContent = anim ? '\u25A0' : '\u25B6';
  btnPP.title = anim ? 'Stop game loop' : 'Start game loop';
  btnPP.classList.toggle('running', anim);
};
btnPP.addEventListener('click', () => {
  if (vg.isAnimating()) {
    vm.b4i("'P gm");
  } else {
    vm.b4i("^I 'p gm");
  }
});

// Patch debugger buttons: %s → /, %g → //, and add a stop button
const memShadow = browser.shadowRoot;
if (memShadow) {
  const btns = memShadow.querySelectorAll('.btns button');
  btns.forEach(btn => {
    if (btn.textContent === '%s') btn.textContent = '/';
    if (btn.textContent === '%g') btn.textContent = '//';
  });
  const btnsContainer = memShadow.querySelector('.btns');
  if (btnsContainer) {
    const stopBtn = document.createElement('button');
    stopBtn.textContent = '\u25A0';
    stopBtn.title = 'Stop game & return to debugger';
    stopBtn.style.cssText = 'color: #f66;';
    stopBtn.addEventListener('click', () => {
      vg.stop();
      browser.refresh();
      browser.scrollToIP();
    });
    btnsContainer.appendChild(stopBtn);
  }
}

const snippets = document.querySelector('b4-snippets')! as B4Snippets;
snippets.setVM(vm);

// Wrap b4i to refresh the memory browser after each command
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
