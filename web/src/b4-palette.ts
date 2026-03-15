const STORAGE_KEY = 'b4-palette';

// Default 16-color PICO-8 palette, extended to 256 with grays and color ramps
function defaultPalette(): Uint32Array {
  const pal = new Uint32Array(256);
  // PICO-8 base 16
  const p8 = [
    0xFF000000, 0xFF1D2B53, 0xFF7E2553, 0xFF008751,
    0xFFAB5236, 0xFF5F574F, 0xFFC2C3C7, 0xFFFFF1E8,
    0xFFFF004D, 0xFFFFA300, 0xFFFFEC27, 0xFF00E436,
    0xFF29ADFF, 0xFF83769C, 0xFFFF77A8, 0xFFFFCCAA,
  ];
  for (let i = 0; i < 16; i++) pal[i] = p8[i];
  // 16-31: grayscale ramp
  for (let i = 0; i < 16; i++) {
    const v = Math.round(i * 255 / 15);
    pal[16 + i] = 0xFF000000 | (v << 16) | (v << 8) | v;
  }
  // 32-255: color ramps (7 hues x 32 shades)
  for (let h = 0; h < 7; h++) {
    for (let s = 0; s < 32; s++) {
      const idx = 32 + h * 32 + s;
      if (idx >= 256) break;
      const t = s / 31;
      const hue = h / 7;
      const [r, g, b] = hslToRgb(hue, 0.7 + t * 0.3, 0.1 + t * 0.8);
      pal[idx] = 0xFF000000 | (r << 16) | (g << 8) | b;
    }
  }
  return pal;
}

function hslToRgb(h: number, s: number, l: number): [number, number, number] {
  let r: number, g: number, b: number;
  if (s === 0) { r = g = b = l; }
  else {
    const hue2rgb = (p: number, q: number, t: number) => {
      if (t < 0) t += 1; if (t > 1) t -= 1;
      if (t < 1/6) return p + (q - p) * 6 * t;
      if (t < 1/2) return q;
      if (t < 2/3) return p + (q - p) * (2/3 - t) * 6;
      return p;
    };
    const q = l < 0.5 ? l * (1 + s) : l + s - l * s;
    const p = 2 * l - q;
    r = hue2rgb(p, q, h + 1/3);
    g = hue2rgb(p, q, h);
    b = hue2rgb(p, q, h - 1/3);
  }
  return [Math.round(r * 255), Math.round(g * 255), Math.round(b * 255)];
}

export class B4Palette {
  colors: Uint32Array;
  onChange: (() => void) | null = null;

  constructor() {
    this.colors = this.load() || defaultPalette();
  }

  private load(): Uint32Array | null {
    const s = localStorage.getItem(STORAGE_KEY);
    if (!s) return null;
    try {
      const arr = JSON.parse(s) as number[];
      if (arr.length !== 256) return null;
      return new Uint32Array(arr);
    } catch { return null; }
  }

  save() {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(Array.from(this.colors)));
    if (this.onChange) this.onChange();
  }

  setColor(index: number, aarrggbb: number) {
    this.colors[index] = aarrggbb;
    this.save();
  }

  // AARRGGBB -> "RRGGBB"
  toHex(index: number): string {
    const c = this.colors[index];
    const r = (c >>> 16) & 0xFF;
    const g = (c >>> 8) & 0xFF;
    const b = c & 0xFF;
    return ((r << 16) | (g << 8) | b).toString(16).padStart(6, '0');
  }

  // "RRGGBB" -> AARRGGBB
  fromHex(hex: string): number {
    const v = parseInt(hex.replace('#', ''), 16);
    return 0xFF000000 | v;
  }

  colorToCSS(index: number): string {
    const c = this.colors[index];
    const a = ((c >>> 24) & 0xFF) / 255;
    const r = (c >>> 16) & 0xFF;
    const g = (c >>> 8) & 0xFF;
    const b = c & 0xFF;
    return `rgba(${r},${g},${b},${a})`;
  }

  // Export as lospec .hex format (one RRGGBB per line)
  exportHex(): string {
    const lines: string[] = [];
    for (let i = 0; i < 256; i++) lines.push(this.toHex(i));
    return lines.join('\n');
  }

  // Import from .hex format
  importHex(text: string) {
    const lines = text.trim().split('\n').map(l => l.trim()).filter(l => l && !l.startsWith(';'));
    for (let i = 0; i < Math.min(lines.length, 256); i++) {
      this.colors[i] = this.fromHex(lines[i]);
    }
    this.save();
  }
}

// Shared singleton
export const palette = new B4Palette();
