import { palette } from './b4-palette';
import { spriteStore, SPRITE_W, SPRITE_H, SPRITE_COUNT, MAP_W, MAP_H } from './b4-sprites';

type Tool = 'pencil' | 'box' | 'line' | 'circle' | 'fill';

export class B4GridEditor extends HTMLElement {
  private mode: 'sprite' | 'map' = 'sprite';
  private canvas!: HTMLCanvasElement;
  private ctx!: CanvasRenderingContext2D;
  private selectorCanvas!: HTMLCanvasElement;
  private selectorCtx!: CanvasRenderingContext2D;
  private paletteCanvas!: HTMLCanvasElement;
  private paletteCtx!: CanvasRenderingContext2D;
  private selectedSprite = 0;
  private selectedColor = 1;
  private currentTool: Tool = 'pencil';
  private penSize = 1;
  private hoverPos: [number, number] | null = null;
  private drawing = false;
  private dragStart: [number, number] | null = null;
  private scrollX = 0;
  private scrollY = 0;
  private toolButtons!: HTMLElement;
  private textBtn!: HTMLButtonElement;
  private allBtn!: HTMLButtonElement;
  private hexArea!: HTMLTextAreaElement;
  private spriteLabel!: HTMLElement;
  private paletteExpanded = false;
  private palCount = 32;

  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
  }

  connectedCallback() {
    this.mode = (this.getAttribute('mode') as 'sprite' | 'map') || 'sprite';
    const shadow = this.shadowRoot!;
    const cellSize = this.mode === 'sprite' ? 16 : 16;
    const canvasW = this.mode === 'sprite' ? SPRITE_W * cellSize : 400;
    const canvasH = this.mode === 'sprite' ? SPRITE_H * cellSize : 400;

    shadow.innerHTML = `
      <style>
        :host { display: flex; height: 100%; color: #ccc; font: 12px monospace; }
        .sidebar {
          width: 260px; display: flex; flex-direction: column;
          border-right: 1px solid #333; overflow-y: auto;
        }
        .sidebar canvas { cursor: pointer; display: block; }
        .main { flex: 1; display: flex; flex-direction: column; min-width: 0; }
        .toolbar {
          display: flex; gap: 2px; padding: 4px;
          border-bottom: 1px solid #333; align-items: center; flex-wrap: wrap;
        }
        .toolbar button {
          background: #222; border: 1px solid #444; color: #ccc;
          font: 11px monospace; padding: 2px 6px; cursor: pointer;
        }
        .toolbar button:hover { background: #333; }
        .toolbar button.active { background: #335; color: #fff; border-color: #58f; }
        .toolbar .pen-size { min-width: 20px; text-align: center; }
        .canvas-wrap {
          flex: 1; overflow: auto; background: #0a0a0a;
          display: flex; align-items: flex-start; justify-content: flex-start;
        }
        .canvas-wrap canvas { cursor: crosshair; image-rendering: pixelated; }
        .palette-bar {
          border-top: 1px solid #333; padding: 2px;
          display: flex; align-items: center; gap: 4px;
        }
        .palette-bar canvas { cursor: pointer; display: block; }
        .color-preview {
          width: 24px; height: 24px; border: 1px solid #666;
        }
        .hex-area {
          display: none; position: absolute; bottom: 30px; right: 4px;
          width: 300px; height: 200px; background: #1a1a1a;
          border: 1px solid #444; color: #ccc; font: 11px monospace;
          padding: 4px; resize: none; z-index: 10;
        }
        .hex-area.visible { display: block; }
        .sprite-nav {
          display: flex; align-items: center; justify-content: center;
          gap: 4px; padding: 4px; border-top: 1px solid #333;
        }
        .sprite-nav button {
          background: #222; border: 1px solid #444; color: #ccc;
          font: 11px monospace; padding: 1px 8px; cursor: pointer;
        }
        .sprite-nav button:hover { background: #333; }
        .sprite-nav span { min-width: 32px; text-align: center; font: 11px monospace; }
        .pal-toggle {
          background: #222; border: 1px solid #444; color: #888;
          font: 10px monospace; padding: 1px 6px; cursor: pointer;
        }
        .pal-toggle:hover { background: #333; color: #ccc; }
      </style>
      <div class="sidebar">
        <canvas class="selector"></canvas>
        <div class="sprite-nav">
          <button class="prev-spr">&lt;</button>
          <span class="spr-label">#0</span>
          <button class="next-spr">&gt;</button>
        </div>
        <div class="sprite-nav">
          <button class="all-btn">All Text</button>
        </div>
      </div>
      <div class="main">
        <div class="toolbar">
          <button class="tool active" data-tool="pencil">Pen</button>
          <button class="tool" data-tool="box">Box</button>
          <button class="tool" data-tool="line">Line</button>
          ${this.mode === 'sprite' ? '<button class="tool" data-tool="circle">Circle</button>' : ''}
          ${this.mode === 'sprite' ? '<button class="tool" data-tool="fill">Fill</button>' : ''}
          ${this.mode === 'sprite' ? `
          <span style="margin-left:8px; color:#888;">Size:</span>
          <button class="pen-size active" data-size="1">1</button>
          <button class="pen-size" data-size="2">2</button>
          <button class="pen-size" data-size="3">3</button>
          <button class="pen-size" data-size="4">4</button>
          ` : ''}
          <span style="flex:1"></span>
          <button class="text-btn">Text</button>
        </div>
        <div class="canvas-wrap">
          <canvas width="${canvasW}" height="${canvasH}"
                  style="width:${canvasW}px;height:${canvasH}px"></canvas>
        </div>
        ${this.mode === 'sprite' ? `
        <div class="palette-bar">
          <div class="color-preview"></div>
          <canvas class="palette-canvas" width="256" height="16"
                  style="width:256px;height:16px"></canvas>
          <button class="pal-toggle">256</button>
        </div>` : ''}
        <textarea class="hex-area"></textarea>
      </div>
    `;

    this.canvas = shadow.querySelector('.canvas-wrap canvas')!;
    this.ctx = this.canvas.getContext('2d')!;
    this.selectorCanvas = shadow.querySelector('.selector')!;
    this.selectorCtx = this.selectorCanvas.getContext('2d')!;
    this.toolButtons = shadow.querySelector('.toolbar')!;
    this.textBtn = shadow.querySelector('.text-btn')!;
    this.allBtn = shadow.querySelector('.all-btn')!;
    this.hexArea = shadow.querySelector('.hex-area')!;

    this.spriteLabel = shadow.querySelector('.spr-label')!;
    shadow.querySelector('.prev-spr')?.addEventListener('click', () => {
      this.selectedSprite = (this.selectedSprite - 1 + SPRITE_COUNT) % SPRITE_COUNT;
      this.updateSpriteNav();
      this.renderSelector();
      this.render();
    });
    shadow.querySelector('.next-spr')?.addEventListener('click', () => {
      this.selectedSprite = (this.selectedSprite + 1) % SPRITE_COUNT;
      this.updateSpriteNav();
      this.renderSelector();
      this.render();
    });

    if (this.mode === 'sprite') {
      this.paletteCanvas = shadow.querySelector('.palette-canvas')!;
      this.paletteCtx = this.paletteCanvas.getContext('2d')!;
      this.setupPalette();
      shadow.querySelector('.pal-toggle')!.addEventListener('click', () => {
        this.paletteExpanded = !this.paletteExpanded;
        this.palCount = this.paletteExpanded ? 256 : 32;
        const btn = shadow.querySelector('.pal-toggle')!;
        btn.textContent = this.paletteExpanded ? '32' : '256';
        const cw = this.palCount * 8;
        this.paletteCanvas.width = cw;
        this.paletteCanvas.style.width = cw + 'px';
        this.renderPalette();
      });
    }

    this.setupSelector();
    this.setupCanvas();
    this.setupTools();
    this.setupCopyPaste();
    this.render();

    palette.onChange = () => { this.renderSelector(); this.render(); if (this.paletteCtx) this.renderPalette(); };
  }

  // -- Selector (left sidebar) --
  private setupSelector() {
    if (this.mode === 'sprite') {
      // 16x16 grid of sprite thumbnails, each 16x16px = 256x256
      this.selectorCanvas.width = 256;
      this.selectorCanvas.height = 256;
      this.selectorCanvas.style.width = '256px';
      this.selectorCanvas.style.height = '256px';
    } else {
      // For map mode: show sprite thumbnails to pick stamp
      this.selectorCanvas.width = 256;
      this.selectorCanvas.height = 256;
      this.selectorCanvas.style.width = '256px';
      this.selectorCanvas.style.height = '256px';
    }
    this.selectorCanvas.addEventListener('click', (e) => {
      const rect = this.selectorCanvas.getBoundingClientRect();
      const x = Math.floor((e.clientX - rect.left) / 16);
      const y = Math.floor((e.clientY - rect.top) / 16);
      this.selectedSprite = Math.min(y * 16 + x, SPRITE_COUNT - 1);
      this.updateSpriteNav();
      this.renderSelector();
      this.render();
    });
    this.renderSelector();
    this.updateSpriteNav();
  }

  private updateSpriteNav() {
    if (this.spriteLabel) this.spriteLabel.textContent = `#${this.selectedSprite}`;
  }

  private renderSelector() {
    const ctx = this.selectorCtx;
    ctx.fillStyle = '#111';
    ctx.fillRect(0, 0, 256, 256);
    // Draw each sprite as 16x16 thumbnail
    for (let si = 0; si < SPRITE_COUNT; si++) {
      const sx = (si % 16) * 16;
      const sy = Math.floor(si / 16) * 16;
      const data = spriteStore.getSpriteData(si);
      for (let py = 0; py < SPRITE_H; py++) {
        for (let px = 0; px < SPRITE_W; px++) {
          const ci = data[py * SPRITE_W + px];
          if (ci === 0) continue; // transparent
          const c = palette.colors[ci];
          const r = (c >>> 16) & 0xFF, g = (c >>> 8) & 0xFF, b = c & 0xFF;
          ctx.fillStyle = `rgb(${r},${g},${b})`;
          ctx.fillRect(sx + px, sy + py, 1, 1);
        }
      }
    }
    // Highlight selected
    const hx = (this.selectedSprite % 16) * 16;
    const hy = Math.floor(this.selectedSprite / 16) * 16;
    ctx.strokeStyle = '#fff';
    ctx.lineWidth = 1;
    ctx.strokeRect(hx + 0.5, hy + 0.5, 15, 15);
  }

  // -- Palette bar (sprite mode only) --
  private setupPalette() {
    this.paletteCanvas.addEventListener('click', (e) => {
      const rect = this.paletteCanvas.getBoundingClientRect();
      const cellW = rect.width / this.palCount;
      const x = Math.floor((e.clientX - rect.left) / cellW);
      this.selectedColor = Math.min(x, this.palCount - 1);
      this.renderPalette();
      this.updateColorPreview();
    });
    // Start with 32 colors
    const cw = this.palCount * 8;
    this.paletteCanvas.width = cw;
    this.paletteCanvas.style.width = cw + 'px';
    this.renderPalette();
    this.updateColorPreview();
  }

  private renderPalette() {
    const ctx = this.paletteCtx;
    const cellW = this.paletteCanvas.width / this.palCount;
    ctx.clearRect(0, 0, this.paletteCanvas.width, 16);
    for (let i = 0; i < this.palCount; i++) {
      ctx.fillStyle = palette.colorToCSS(i);
      ctx.fillRect(i * cellW, 0, cellW, 16);
    }
    // Highlight selected
    if (this.selectedColor < this.palCount) {
      ctx.strokeStyle = '#fff';
      ctx.lineWidth = 1;
      ctx.strokeRect(this.selectedColor * cellW, 0, cellW, 16);
    }
  }

  private updateColorPreview() {
    const preview = this.shadowRoot!.querySelector('.color-preview') as HTMLElement;
    if (preview) preview.style.background = palette.colorToCSS(this.selectedColor);
  }

  // -- Main canvas --
  private setupCanvas() {
    const wrap = this.canvas.parentElement!;

    this.canvas.addEventListener('mousedown', (e) => {
      const [gx, gy] = this.canvasToGrid(e);
      this.drawing = true;
      this.dragStart = [gx, gy];
      if (this.currentTool === 'pencil') this.paint(gx, gy);
      else if (this.currentTool === 'fill' && this.mode === 'sprite') this.floodFill(gx, gy);
    });

    this.canvas.addEventListener('mousemove', (e) => {
      const [gx, gy] = this.canvasToGrid(e);
      this.hoverPos = [gx, gy];
      if (this.drawing) {
        if (this.currentTool === 'pencil') this.paint(gx, gy);
        else this.renderPreview(gx, gy);
      } else {
        this.render();
      }
    });

    this.canvas.addEventListener('mouseleave', () => {
      this.hoverPos = null;
      if (!this.drawing) this.render();
    });

    this.canvas.addEventListener('mouseup', (e) => {
      if (!this.drawing) return;
      const [gx, gy] = this.canvasToGrid(e);
      this.drawing = false;
      if (this.dragStart && this.currentTool !== 'pencil' && this.currentTool !== 'fill') {
        this.applyShape(this.dragStart[0], this.dragStart[1], gx, gy);
      }
      this.dragStart = null;
      this.render();
    });

    // Scroll for map mode
    if (this.mode === 'map') {
      wrap.addEventListener('scroll', () => {
        this.scrollX = wrap.scrollLeft;
        this.scrollY = wrap.scrollTop;
      });
      // Size the canvas to fit the full map
      const cs = 16;
      this.canvas.width = MAP_W * cs;
      this.canvas.height = MAP_H * cs;
      this.canvas.style.width = MAP_W * cs + 'px';
      this.canvas.style.height = MAP_H * cs + 'px';
    }
  }

  private canvasToGrid(e: MouseEvent): [number, number] {
    const rect = this.canvas.getBoundingClientRect();
    const cs = this.mode === 'sprite' ? 16 : 16;
    const maxW = this.mode === 'sprite' ? SPRITE_W : MAP_W;
    const maxH = this.mode === 'sprite' ? SPRITE_H : MAP_H;
    const x = Math.floor((e.clientX - rect.left) / cs);
    const y = Math.floor((e.clientY - rect.top) / cs);
    return [Math.max(0, Math.min(x, maxW - 1)), Math.max(0, Math.min(y, maxH - 1))];
  }

  /** Return top-left of brush centered on gx,gy */
  private brushOrigin(gx: number, gy: number): [number, number] {
    const half = Math.floor(this.penSize / 2);
    return [gx - half, gy - half];
  }

  private paint(gx: number, gy: number) {
    const s = this.penSize;
    const [ox, oy] = this.brushOrigin(gx, gy);
    if (this.mode === 'sprite') {
      for (let dy = 0; dy < s; dy++) {
        for (let dx = 0; dx < s; dx++) {
          const px = ox + dx, py = oy + dy;
          if (px >= 0 && px < SPRITE_W && py >= 0 && py < SPRITE_H) {
            spriteStore.setSpritePixel(this.selectedSprite, px, py, this.selectedColor);
          }
        }
      }
      spriteStore.saveSprites();
    } else {
      for (let dy = 0; dy < s; dy++) {
        for (let dx = 0; dx < s; dx++) {
          const px = ox + dx, py = oy + dy;
          if (px >= 0 && px < MAP_W && py >= 0 && py < MAP_H) {
            spriteStore.setMapCell(px, py, this.selectedSprite);
          }
        }
      }
      spriteStore.saveMap();
    }
    this.render();
    this.renderSelector();
  }

  private floodFill(startX: number, startY: number) {
    const target = spriteStore.getSpritePixel(this.selectedSprite, startX, startY);
    if (target === this.selectedColor) return;
    const stack: [number, number][] = [[startX, startY]];
    while (stack.length > 0) {
      const [x, y] = stack.pop()!;
      if (x < 0 || x >= SPRITE_W || y < 0 || y >= SPRITE_H) continue;
      if (spriteStore.getSpritePixel(this.selectedSprite, x, y) !== target) continue;
      spriteStore.setSpritePixel(this.selectedSprite, x, y, this.selectedColor);
      stack.push([x - 1, y], [x + 1, y], [x, y - 1], [x, y + 1]);
    }
    spriteStore.saveSprites();
    this.render();
    this.renderSelector();
  }

  private applyShape(x0: number, y0: number, x1: number, y1: number) {
    const points = this.getShapePoints(x0, y0, x1, y1);
    for (const [px, py] of points) {
      if (this.mode === 'sprite') {
        if (px >= 0 && px < SPRITE_W && py >= 0 && py < SPRITE_H)
          spriteStore.setSpritePixel(this.selectedSprite, px, py, this.selectedColor);
      } else {
        if (px >= 0 && px < MAP_W && py >= 0 && py < MAP_H)
          spriteStore.setMapCell(px, py, this.selectedSprite);
      }
    }
    if (this.mode === 'sprite') spriteStore.saveSprites();
    else spriteStore.saveMap();
    this.renderSelector();
  }

  private getShapePoints(x0: number, y0: number, x1: number, y1: number): [number, number][] {
    const points: [number, number][] = [];
    switch (this.currentTool) {
      case 'box': {
        const minX = Math.min(x0, x1), maxX = Math.max(x0, x1);
        const minY = Math.min(y0, y1), maxY = Math.max(y0, y1);
        for (let y = minY; y <= maxY; y++)
          for (let x = minX; x <= maxX; x++)
            points.push([x, y]);
        break;
      }
      case 'line': {
        // Bresenham
        let dx = Math.abs(x1 - x0), dy = Math.abs(y1 - y0);
        let sx = x0 < x1 ? 1 : -1, sy = y0 < y1 ? 1 : -1;
        let err = dx - dy, x = x0, y = y0;
        while (true) {
          points.push([x, y]);
          if (x === x1 && y === y1) break;
          const e2 = 2 * err;
          if (e2 > -dy) { err -= dy; x += sx; }
          if (e2 < dx) { err += dx; y += sy; }
        }
        break;
      }
      case 'circle': {
        const r = Math.round(Math.sqrt((x1 - x0) ** 2 + (y1 - y0) ** 2));
        // Filled circle
        for (let dy = -r; dy <= r; dy++)
          for (let dx = -r; dx <= r; dx++)
            if (dx * dx + dy * dy <= r * r)
              points.push([x0 + dx, y0 + dy]);
        break;
      }
    }
    return points;
  }

  private renderPreview(gx: number, gy: number) {
    this.render();
    if (!this.dragStart) return;
    const points = this.getShapePoints(this.dragStart[0], this.dragStart[1], gx, gy);
    const cs = 16;
    this.ctx.fillStyle = this.mode === 'sprite'
      ? palette.colorToCSS(this.selectedColor)
      : 'rgba(255,255,255,0.3)';
    for (const [px, py] of points) {
      this.ctx.fillRect(px * cs, py * cs, cs, cs);
    }
  }

  render() {
    const cs = 16;
    const ctx = this.ctx;

    if (this.mode === 'sprite') {
      ctx.fillStyle = '#111';
      ctx.fillRect(0, 0, SPRITE_W * cs, SPRITE_H * cs);
      const data = spriteStore.getSpriteData(this.selectedSprite);
      for (let y = 0; y < SPRITE_H; y++) {
        for (let x = 0; x < SPRITE_W; x++) {
          const ci = data[y * SPRITE_W + x];
          ctx.fillStyle = palette.colorToCSS(ci);
          ctx.fillRect(x * cs, y * cs, cs, cs);
        }
      }
      // Grid lines
      ctx.strokeStyle = 'rgba(255,255,255,0.08)';
      ctx.lineWidth = 1;
      for (let x = 0; x <= SPRITE_W; x++) {
        ctx.beginPath(); ctx.moveTo(x * cs + 0.5, 0); ctx.lineTo(x * cs + 0.5, SPRITE_H * cs); ctx.stroke();
      }
      for (let y = 0; y <= SPRITE_H; y++) {
        ctx.beginPath(); ctx.moveTo(0, y * cs + 0.5); ctx.lineTo(SPRITE_W * cs, y * cs + 0.5); ctx.stroke();
      }
    } else {
      // Map mode: draw sprite thumbnails in cells
      ctx.fillStyle = '#111';
      ctx.fillRect(0, 0, MAP_W * cs, MAP_H * cs);
      for (let my = 0; my < MAP_H; my++) {
        for (let mx = 0; mx < MAP_W; mx++) {
          const si = spriteStore.getMapCell(mx, my);
          if (si === 0) continue;
          this.drawSpriteThumbnail(ctx, si, mx * cs, my * cs, cs);
        }
      }
      // Grid lines
      ctx.strokeStyle = 'rgba(255,255,255,0.05)';
      ctx.lineWidth = 1;
      for (let x = 0; x <= MAP_W; x++) {
        ctx.beginPath(); ctx.moveTo(x * cs + 0.5, 0); ctx.lineTo(x * cs + 0.5, MAP_H * cs); ctx.stroke();
      }
      for (let y = 0; y <= MAP_H; y++) {
        ctx.beginPath(); ctx.moveTo(0, y * cs + 0.5); ctx.lineTo(MAP_W * cs, y * cs + 0.5); ctx.stroke();
      }
    }

    // Brush preview on hover
    if (this.hoverPos && this.currentTool === 'pencil') {
      const [hx, hy] = this.hoverPos;
      const [ox, oy] = this.brushOrigin(hx, hy);
      const maxW = this.mode === 'sprite' ? SPRITE_W : MAP_W;
      const maxH = this.mode === 'sprite' ? SPRITE_H : MAP_H;
      ctx.fillStyle = 'rgba(255,255,255,0.25)';
      for (let dy = 0; dy < this.penSize; dy++) {
        for (let dx = 0; dx < this.penSize; dx++) {
          const px = ox + dx, py = oy + dy;
          if (px >= 0 && px < maxW && py >= 0 && py < maxH) {
            ctx.fillRect(px * cs, py * cs, cs, cs);
          }
        }
      }
    }
  }

  private drawSpriteThumbnail(ctx: CanvasRenderingContext2D, si: number, dx: number, dy: number, size: number) {
    const data = spriteStore.getSpriteData(si);
    const scale = size / SPRITE_W;
    for (let y = 0; y < SPRITE_H; y++) {
      for (let x = 0; x < SPRITE_W; x++) {
        const ci = data[y * SPRITE_W + x];
        if (ci === 0) continue;
        ctx.fillStyle = palette.colorToCSS(ci);
        ctx.fillRect(dx + x * scale, dy + y * scale, scale, scale);
      }
    }
  }

  // -- Tools --
  private setupTools() {
    this.toolButtons.querySelectorAll('.tool').forEach(btn => {
      btn.addEventListener('click', () => {
        this.toolButtons.querySelectorAll('.tool').forEach(b => b.classList.remove('active'));
        btn.classList.add('active');
        this.currentTool = btn.getAttribute('data-tool') as Tool;
      });
    });
    this.toolButtons.querySelectorAll('.pen-size').forEach(btn => {
      btn.addEventListener('click', () => {
        this.toolButtons.querySelectorAll('.pen-size').forEach(b => b.classList.remove('active'));
        btn.classList.add('active');
        this.penSize = parseInt(btn.getAttribute('data-size')!);
      });
    });
  }

  // -- Text toggle (show/edit hex) --
  private setupCopyPaste() {
    let activeImportFn: ((text: string) => void) | null = null;

    const closeHex = (doImport: boolean) => {
      if (doImport && activeImportFn) {
        const text = this.hexArea.value.trim();
        if (text) activeImportFn(text);
      }
      this.hexArea.classList.remove('visible');
      this.hexArea.onkeydown = null;
      activeImportFn = null;
      this.render();
      this.renderSelector();
    };

    const toggleHex = (exportFn: () => string, importFn: (text: string) => void) => {
      if (this.hexArea.classList.contains('visible')) {
        closeHex(true);
      } else {
        activeImportFn = importFn;
        const hex = exportFn();
        this.hexArea.value = hex;
        this.hexArea.classList.add('visible');
        this.hexArea.select();
        navigator.clipboard.writeText(hex).catch(() => {});
        this.hexArea.onkeydown = (e) => {
          if (e.key === 'Enter' && e.ctrlKey) {
            e.preventDefault();
            closeHex(true);
          } else if (e.key === 'Escape') {
            closeHex(false);
          }
        };
      }
    };

    this.textBtn.addEventListener('click', () => {
      if (this.mode === 'sprite') {
        toggleHex(
          () => spriteStore.exportSpriteHex(this.selectedSprite),
          (text) => spriteStore.importSpriteHex(this.selectedSprite, text),
        );
      }
    });

    this.allBtn.addEventListener('click', () => {
      toggleHex(
        () => spriteStore.exportAllHex(),
        (text) => spriteStore.importAllHex(text),
      );
    });
  }
}

customElements.define('b4-grid-editor', B4GridEditor);
