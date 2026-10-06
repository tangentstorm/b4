/**
 * Thin LD28 host glue: palette toolbar + localStorage levels.
 * Game logic (paint, gravity, draw) lives in examples/ld28/ld28.b4.
 */
import type { B4VM } from '@tangentstorm/b4';
import type { B4Snippets } from './b4-snippets';
import {
  parseCart, applyCartAssets, assembleCartCode, type B4CartData,
} from './b4-cart';
import cartSource from '../examples/ld28/ld28.b4?raw';

export const ROOM_BASE = 0x800;
export const ROOM_LEN = 24 * 16; // 384
export const TOOL_NAMES = ['empty', 'wall', 'door', 'hero', 'baddie', 'gold'] as const;
export const TOOL_COLORS = ['#666', '#999', '#6c9', '#9cf', '#e24', '#fc4'] as const;

const LEVS_KEY = 'levs';
const lvKey = (id: string | number) => `lv:${id}`;

export interface Ld28Controls {
  setTool(i: number): void;
  loadLevel(id: string): void;
  saveLevel(id: string): void;
  newLevel(): void;
  getRoom(): number[];
  setRoom(cells: number[]): void;
  boot(): void;
}

function readLevs(): number[] {
  try {
    const v = JSON.parse(localStorage.getItem(LEVS_KEY) || '[]');
    return Array.isArray(v) ? v.map(Number) : [];
  } catch {
    return [];
  }
}

function writeLevs(levs: number[]) {
  localStorage.setItem(LEVS_KEY, JSON.stringify(levs.sort((a, b) => a - b)));
}

export function mountLd28(
  vm: B4VM,
  host: HTMLElement,
  snippets?: B4Snippets | null,
): Ld28Controls {
  const cart: B4CartData = parseCart(cartSource);

  const bar = document.createElement('div');
  bar.id = 'ld28-bar';
  bar.innerHTML = `
    <style>
      #ld28-bar {
        display: flex; flex-wrap: wrap; gap: 6px; align-items: center;
        padding: 6px 8px; background: #333; border-bottom: 1px solid #444;
        font: 12px verdana, arial, sans-serif; color: #ccc;
      }
      #ld28-bar .tools { display: flex; gap: 2px; }
      #ld28-bar .tool {
        width: 28px; height: 22px; border: 1px solid #333; cursor: pointer;
        padding: 0;
      }
      #ld28-bar .tool.active { outline: 3px solid #fff; outline-offset: 1px; }
      #ld28-bar form, #ld28-bar nav {
        display: inline-flex; gap: 4px; align-items: center;
        background: #555; padding: 4px 6px; margin: 0;
      }
      #ld28-bar input#ld28-lev {
        width: 3em; border: 0; padding: 4px; background: #222; color: #eee;
        font: 12px monospace;
      }
      #ld28-bar button, #ld28-bar nav a {
        border: 0; padding: 4px 8px; background: #999; color: #111;
        font: 10pt verdana, sans-serif; cursor: pointer; text-decoration: none;
      }
      #ld28-bar button:hover, #ld28-bar nav a:hover { background: #ccc; }
      #ld28-bar .hint { color: #888; margin-left: 8px; }
    </style>
    <div class="tools"></div>
    <form id="ld28-form">
      <input id="ld28-lev" type="text" value="" title="level id" />
      <button type="button" data-act="load">load</button>
      <button type="button" data-act="save">save</button>
      <button type="button" data-act="new">new</button>
    </form>
    <nav id="ld28-nav"></nav>
    <span class="hint">LD28 map-editor · source in Snippets → ld28</span>
  `;
  host.prepend(bar);

  const toolsEl = bar.querySelector('.tools')!;
  const levInput = bar.querySelector('#ld28-lev') as HTMLInputElement;
  const navEl = bar.querySelector('#ld28-nav')!;

  let tool = 1;

  function setTool(i: number) {
    tool = Math.max(0, Math.min(5, i | 0));
    (vm as any)._sr('B', tool);
    toolsEl.querySelectorAll('.tool').forEach((el, idx) => {
      el.classList.toggle('active', idx === tool);
    });
  }

  TOOL_COLORS.forEach((color, i) => {
    const btn = document.createElement('button');
    btn.type = 'button';
    btn.className = 'tool' + (i === tool ? ' active' : '');
    btn.style.background = color;
    btn.title = TOOL_NAMES[i];
    btn.addEventListener('click', () => setTool(i));
    toolsEl.appendChild(btn);
  });

  function getRoom(): number[] {
    const ram: Uint8Array = (vm as any).ram;
    return Array.from(ram.subarray(ROOM_BASE, ROOM_BASE + ROOM_LEN));
  }

  function setRoom(cells: number[]) {
    const ram: Uint8Array = (vm as any).ram;
    for (let i = 0; i < ROOM_LEN; i++) ram[ROOM_BASE + i] = (cells[i] ?? 0) & 0xff;
    // keep hero register in sync if a hero tile exists
    const hi = cells.indexOf(3);
    (vm as any)._sr('A', hi >= 0 ? hi : 0);
  }

  function refreshNav() {
    navEl.innerHTML = '';
    for (const id of readLevs()) {
      const a = document.createElement('a');
      a.href = '#';
      a.textContent = String(id);
      a.addEventListener('click', (e) => {
        e.preventDefault();
        loadLevel(String(id));
      });
      navEl.appendChild(a);
    }
  }

  function loadLevel(id: string) {
    levInput.value = id;
    try {
      const raw = localStorage.getItem(lvKey(id));
      const cells = raw ? JSON.parse(raw) : null;
      if (Array.isArray(cells) && cells.length === ROOM_LEN) setRoom(cells.map(Number));
      else setRoom(new Array(ROOM_LEN).fill(0));
    } catch {
      setRoom(new Array(ROOM_LEN).fill(0));
    }
  }

  function saveLevel(id: string) {
    if (!id) return;
    localStorage.setItem(lvKey(id), JSON.stringify(getRoom()));
    const levs = readLevs();
    const n = Number(id);
    if (!Number.isNaN(n) && !levs.includes(n)) {
      levs.push(n);
      writeLevs(levs);
    }
    refreshNav();
  }

  function newLevel() {
    const levs = readLevs();
    let i = 0;
    while (levs.includes(i)) i++;
    levs.push(i);
    writeLevs(levs);
    localStorage.setItem(lvKey(i), JSON.stringify(new Array(ROOM_LEN).fill(0)));
    setRoom(new Array(ROOM_LEN).fill(0));
    levInput.value = String(i);
    refreshNav();
  }

  bar.querySelectorAll('#ld28-form button').forEach((btn) => {
    btn.addEventListener('click', () => {
      const act = (btn as HTMLElement).dataset.act;
      const id = levInput.value.trim();
      if (act === 'load') loadLevel(id);
      else if (act === 'save') saveLevel(id);
      else if (act === 'new') newLevel();
    });
  });

  function boot() {
    applyCartAssets(cart);
    assembleCartCode(vm, cart.code);
    setTool(tool);
    snippets?.setSnippet('ld28', cart.code, true);
    refreshNav();
  }

  return { setTool, loadLevel, saveLevel, newLevel, getRoom, setRoom, boot };
}
