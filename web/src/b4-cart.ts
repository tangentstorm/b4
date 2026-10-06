/** PICO-8-style .b4 cart parser (shared with b4-gd Cart.gd). */

import { palette } from './b4-palette';
import { spriteStore, MAP_W, MAP_H } from './b4-sprites';
import type { B4VM } from '@tangentstorm/b4';

export interface B4CartData {
  code: string;
  gfx: string;
  map: string;
  palette: string;
}

const SEC = {
  code: '__code__',
  gfx: '__gfx__',
  map: '__map__',
  palette: '__palette__',
} as const;

export function parseCart(text: string): B4CartData {
  const out: B4CartData = { code: '', gfx: '', map: '', palette: '' };
  let section = '';
  const buf: string[] = [];
  const flush = () => {
    const body = buf.join('\n').trim();
    if (section === SEC.code) out.code = body;
    else if (section === SEC.gfx) out.gfx = body;
    else if (section === SEC.map) out.map = body;
    else if (section === SEC.palette) out.palette = body;
  };
  for (const raw of text.split('\n')) {
    const t = raw.trim();
    if (t === SEC.code || t === SEC.gfx || t === SEC.map || t === SEC.palette) {
      flush();
      section = t;
      buf.length = 0;
      continue;
    }
    buf.push(raw);
  }
  flush();
  return out;
}

export function applyCartAssets(cart: B4CartData) {
  if (cart.gfx.trim()) spriteStore.importAllHex(cart.gfx);
  if (cart.map.trim()) {
    const lines = cart.map.trim().split('\n');
    for (let y = 0; y < Math.min(lines.length, MAP_H); y++) {
      const line = lines[y].trim();
      for (let x = 0; x < Math.min(line.length / 2, MAP_W); x++) {
        spriteStore.map[y * MAP_W + x] = parseInt(line.slice(x * 2, x * 2 + 2), 16);
      }
    }
    spriteStore.saveMap();
  }
  if (cart.palette.trim()) palette.importHex(cart.palette);
}

/** Assemble cart __code__ via b4i (giraffe-style: each line may start a word). */
export function assembleCartCode(vm: B4VM, code: string) {
  for (const raw of code.split('\n')) {
    let line = raw;
    const hash = line.indexOf('#');
    if (hash >= 0) line = line.slice(0, hash);
    line = line.trim();
    if (line) vm.b4i(line);
  }
}

export async function loadCartUrl(url: string): Promise<B4CartData> {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`cart fetch failed: ${url} (${res.status})`);
  return parseCart(await res.text());
}
