export class B4Canvas extends HTMLElement {
  private canvas!: HTMLCanvasElement;
  private ctx!: CanvasRenderingContext2D;
  private W = 320;
  private H = 200;
  private fillColor = 0xFFFFFFFF;  // AARRGGBB (white, opaque)

  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
  }

  connectedCallback() {
    this.W = parseInt(this.getAttribute('width') || '320', 10);
    this.H = parseInt(this.getAttribute('height') || '200', 10);

    const shadow = this.shadowRoot!;
    shadow.innerHTML = `
      <style>
        :host {
          display: block;
          position: relative;
          width: 100%;
          height: 100%;
        }
        .wrap {
          position: relative;
          background: #000;
          border: 1px solid #333;
          width: 100%;
          height: 100%;
          display: flex;
          align-items: center;
          justify-content: center;
        }
        canvas {
          display: block;
          image-rendering: pixelated;
          image-rendering: crisp-edges;
          width: 100%;
          height: 100%;
          object-fit: contain;
        }
        button {
          position: absolute;
          top: 2px; right: 2px;
          background: rgba(255,255,255,0.15);
          border: none;
          color: #ccc;
          font: 12px monospace;
          padding: 2px 6px;
          cursor: pointer;
          opacity: 0;
          transition: opacity 0.2s;
        }
        .wrap:hover button { opacity: 1; }
        button:hover { background: rgba(255,255,255,0.3); }

        :host(.maximized) {
          position: fixed;
          inset: 0;
          z-index: 9999;
          display: flex;
          align-items: center;
          justify-content: center;
          background: #333;
        }
        :host(.maximized) .wrap {
          width: 95vw;
          height: 95vh;
          border: none;
          box-shadow: 0 4px 24px rgba(0,0,0,0.6);
        }
        :host(.maximized) canvas {
          width: 100%;
          height: 100%;
          object-fit: contain;
        }
        :host(.maximized) button { opacity: 1; }
      </style>
      <div class="wrap">
        <canvas width="${this.W}" height="${this.H}"></canvas>
        <button title="Maximize">&#x26F6;</button>
      </div>
    `;
    this.canvas = shadow.querySelector('canvas')!;
    this.ctx = this.canvas.getContext('2d')!;
    shadow.querySelector('button')!.addEventListener('click', () => this.toggleMaximize());
  }

  private toggleMaximize() {
    this.classList.toggle('maximized');
  }

  private colorToCSS(c: number): string {
    const a = ((c >>> 24) & 0xFF) / 255;
    const r = (c >>> 16) & 0xFF;
    const g = (c >>> 8) & 0xFF;
    const b = c & 0xFF;
    return `rgba(${r},${g},${b},${a})`;
  }

  invert() {
    const img = this.ctx.getImageData(0, 0, this.W, this.H);
    const d = img.data;
    for (let i = 0; i < d.length; i += 4) {
      d[i]     ^= 0xFF;
      d[i + 1] ^= 0xFF;
      d[i + 2] ^= 0xFF;
      d[i + 3] = 0xFF;
    }
    this.ctx.putImageData(img, 0, 0);
  }

  setFill(color: number) {
    this.fillColor = color;
  }

  drawCircle(cx: number, cy: number, r: number) {
    this.ctx.fillStyle = this.colorToCSS(this.fillColor);
    this.ctx.beginPath();
    this.ctx.arc(cx, cy, r, 0, Math.PI * 2);
    this.ctx.fill();
  }

  drawBox(x: number, y: number, w: number, h: number) {
    this.ctx.fillStyle = this.colorToCSS(this.fillColor);
    this.ctx.fillRect(x, y, w, h);
  }

  clear() {
    this.ctx.fillStyle = this.colorToCSS(this.fillColor);
    this.ctx.fillRect(0, 0, this.W, this.H);
  }

  drawSprite(pixels: Uint8Array, pal: Uint32Array, x: number, y: number, spriteW = 16, spriteH = 16) {
    const img = this.ctx.createImageData(spriteW, spriteH);
    const d = img.data;
    for (let i = 0; i < spriteW * spriteH; i++) {
      const ci = pixels[i];
      if (ci === 0) { d[i * 4 + 3] = 0; continue; } // color 0 = transparent
      const c = pal[ci];
      d[i * 4]     = (c >>> 16) & 0xFF; // R
      d[i * 4 + 1] = (c >>> 8) & 0xFF;  // G
      d[i * 4 + 2] = c & 0xFF;          // B
      d[i * 4 + 3] = (c >>> 24) & 0xFF; // A
    }
    // Use temp canvas + drawImage so alpha composites correctly
    const tmp = document.createElement('canvas');
    tmp.width = spriteW; tmp.height = spriteH;
    tmp.getContext('2d')!.putImageData(img, 0, 0);
    this.ctx.drawImage(tmp, x, y);
  }

  drawMapRegion(
    map: Uint8Array, mapW: number,
    sprites: Uint8Array, spriteSize: number,
    pal: Uint32Array,
    mx: number, my: number,
    sx: number, sy: number,
    w: number, h: number,
    spriteW = 16, spriteH = 16
  ) {
    for (let ty = 0; ty < h; ty++) {
      for (let tx = 0; tx < w; tx++) {
        const mapIdx = (my + ty) * mapW + (mx + tx);
        const si = map[mapIdx];
        if (si === 0) continue;
        const off = si * spriteSize;
        this.drawSprite(
          sprites.subarray(off, off + spriteSize),
          pal,
          sx + tx * spriteW,
          sy + ty * spriteH,
          spriteW, spriteH
        );
      }
    }
  }

  getContext(): CanvasRenderingContext2D { return this.ctx; }
  getCanvas(): HTMLCanvasElement { return this.canvas; }
  getWidth(): number { return this.W; }
  getHeight(): number { return this.H; }
}

customElements.define('b4-canvas', B4Canvas);
