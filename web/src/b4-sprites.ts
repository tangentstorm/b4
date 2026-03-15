const SPRITE_KEY = 'b4-sprites';
const MAP_KEY = 'b4-map';

export const SPRITE_W = 16;
export const SPRITE_H = 16;
export const SPRITE_COUNT = 256;
export const SPRITE_SIZE = SPRITE_W * SPRITE_H; // 256 bytes per sprite
export const MAP_W = 64;
export const MAP_H = 64;

export class B4SpriteStore {
  sprites: Uint8Array;  // 256 sprites * 256 bytes = 65536
  map: Uint8Array;      // 64 * 64 = 4096

  constructor() {
    this.sprites = this.loadSprites();
    this.map = this.loadMap();
  }

  private loadSprites(): Uint8Array {
    const s = localStorage.getItem(SPRITE_KEY);
    if (!s) return new Uint8Array(SPRITE_COUNT * SPRITE_SIZE);
    try {
      const arr = Uint8Array.from(atob(s), c => c.charCodeAt(0));
      return arr.length === SPRITE_COUNT * SPRITE_SIZE ? arr : new Uint8Array(SPRITE_COUNT * SPRITE_SIZE);
    } catch { return new Uint8Array(SPRITE_COUNT * SPRITE_SIZE); }
  }

  private loadMap(): Uint8Array {
    const s = localStorage.getItem(MAP_KEY);
    if (!s) return new Uint8Array(MAP_W * MAP_H);
    try {
      const arr = Uint8Array.from(atob(s), c => c.charCodeAt(0));
      return arr.length === MAP_W * MAP_H ? arr : new Uint8Array(MAP_W * MAP_H);
    } catch { return new Uint8Array(MAP_W * MAP_H); }
  }

  saveSprites() {
    let bin = '';
    for (let i = 0; i < this.sprites.length; i++) bin += String.fromCharCode(this.sprites[i]);
    localStorage.setItem(SPRITE_KEY, btoa(bin));
  }

  saveMap() {
    let bin = '';
    for (let i = 0; i < this.map.length; i++) bin += String.fromCharCode(this.map[i]);
    localStorage.setItem(MAP_KEY, btoa(bin));
  }

  getSpriteData(index: number): Uint8Array {
    const off = index * SPRITE_SIZE;
    return this.sprites.subarray(off, off + SPRITE_SIZE);
  }

  setSpritePixel(spriteIndex: number, x: number, y: number, colorIndex: number) {
    this.sprites[spriteIndex * SPRITE_SIZE + y * SPRITE_W + x] = colorIndex;
  }

  getSpritePixel(spriteIndex: number, x: number, y: number): number {
    return this.sprites[spriteIndex * SPRITE_SIZE + y * SPRITE_W + x];
  }

  setMapCell(x: number, y: number, spriteIndex: number) {
    this.map[y * MAP_W + x] = spriteIndex;
  }

  getMapCell(x: number, y: number): number {
    return this.map[y * MAP_W + x];
  }

  // Export sprite as hex string (2 hex chars per pixel, 16 per row)
  exportSpriteHex(index: number): string {
    const data = this.getSpriteData(index);
    const lines: string[] = [];
    for (let y = 0; y < SPRITE_H; y++) {
      let line = '';
      for (let x = 0; x < SPRITE_W; x++) {
        line += data[y * SPRITE_W + x].toString(16).padStart(2, '0');
      }
      lines.push(line);
    }
    return lines.join('\n');
  }

  // Import sprite from hex string
  importSpriteHex(index: number, text: string) {
    const lines = text.trim().split('\n');
    const off = index * SPRITE_SIZE;
    for (let y = 0; y < Math.min(lines.length, SPRITE_H); y++) {
      const line = lines[y].trim();
      for (let x = 0; x < Math.min(line.length / 2, SPRITE_W); x++) {
        this.sprites[off + y * SPRITE_W + x] = parseInt(line.slice(x * 2, x * 2 + 2), 16);
      }
    }
    this.saveSprites();
  }

  // Export entire sprite sheet as hex (sprites separated by blank lines)
  exportAllHex(): string {
    const parts: string[] = [];
    for (let i = 0; i < SPRITE_COUNT; i++) {
      parts.push(this.exportSpriteHex(i));
    }
    return parts.join('\n\n');
  }

  // Import entire sprite sheet from hex (sprites separated by blank lines)
  importAllHex(text: string) {
    const blocks = text.trim().split(/\n\s*\n/);
    for (let i = 0; i < Math.min(blocks.length, SPRITE_COUNT); i++) {
      const lines = blocks[i].trim().split('\n');
      const off = i * SPRITE_SIZE;
      for (let y = 0; y < Math.min(lines.length, SPRITE_H); y++) {
        const line = lines[y].trim();
        for (let x = 0; x < Math.min(line.length / 2, SPRITE_W); x++) {
          this.sprites[off + y * SPRITE_W + x] = parseInt(line.slice(x * 2, x * 2 + 2), 16);
        }
      }
    }
    this.saveSprites();
  }
}

export const spriteStore = new B4SpriteStore();
