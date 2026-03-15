import { palette } from './b4-palette';

export class B4PaletteEditor extends HTMLElement {
  private gridCanvas!: HTMLCanvasElement;
  private gridCtx!: CanvasRenderingContext2D;
  private hexInput!: HTMLInputElement;
  private hexArea!: HTMLTextAreaElement;
  private selected = 0;

  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
  }

  connectedCallback() {
    const shadow = this.shadowRoot!;
    shadow.innerHTML = `
      <style>
        :host { display: flex; flex-direction: column; height: 100%; color: #ccc; font: 12px monospace; }
        .toolbar {
          display: flex; gap: 4px; padding: 4px;
          border-bottom: 1px solid #333; align-items: center;
        }
        .toolbar button {
          background: #222; border: 1px solid #444; color: #ccc;
          font: 11px monospace; padding: 2px 8px; cursor: pointer;
        }
        .toolbar button:hover { background: #333; }
        .color-info { display: flex; align-items: center; gap: 4px; }
        .preview { width: 20px; height: 20px; border: 1px solid #666; }
        .toolbar input {
          background: #1a1a1a; border: 1px solid #444; color: #ccc;
          font: 12px monospace; padding: 2px 4px; width: 70px;
        }
        .grid-wrap { flex: 1; overflow: auto; padding: 4px; }
        canvas { cursor: pointer; display: block; image-rendering: pixelated; }
        textarea {
          display: none; width: 100%; height: 200px; background: #1a1a1a;
          color: #ccc; border: 1px solid #444; font: 11px monospace;
          padding: 4px; resize: vertical;
        }
        textarea.visible { display: block; }
      </style>
      <div class="toolbar">
        <div class="color-info">
          <div class="preview"></div>
          <span class="idx">#0</span>
          <input type="text" class="hex-input" value="000000" maxlength="6" />
        </div>
        <span style="flex:1"></span>
        <button class="import-btn">Import .hex</button>
        <button class="export-btn">Export .hex</button>
      </div>
      <div class="grid-wrap">
        <canvas width="256" height="64" style="width:512px;height:128px"></canvas>
      </div>
      <textarea class="hex-area"></textarea>
    `;

    this.gridCanvas = shadow.querySelector('.grid-wrap canvas')!;
    this.gridCtx = this.gridCanvas.getContext('2d')!;
    this.hexInput = shadow.querySelector('.hex-input')!;
    this.hexArea = shadow.querySelector('.hex-area')!;

    this.gridCanvas.addEventListener('click', (e) => {
      const rect = this.gridCanvas.getBoundingClientRect();
      const scaleX = 256 / rect.width;
      const scaleY = 64 / rect.height;
      const x = Math.floor((e.clientX - rect.left) * scaleX);
      const y = Math.floor((e.clientY - rect.top) * scaleY);
      // 16 colors per row, each cell 16x16 -> 256x64 for 256 colors (16 rows of 16)
      const col = Math.floor(x / 16);
      const row = Math.floor(y / 4);
      this.selected = Math.min(row * 16 + col, 255);
      this.updateUI();
    });

    // Actually let's make it 16 wide x 16 tall = 256 colors
    this.gridCanvas.width = 16 * 8;  // 128
    this.gridCanvas.height = 16 * 8; // 128
    this.gridCanvas.style.width = '256px';
    this.gridCanvas.style.height = '256px';

    this.hexInput.addEventListener('change', () => {
      const hex = this.hexInput.value.trim();
      if (/^[0-9a-fA-F]{6}$/.test(hex)) {
        palette.setColor(this.selected, palette.fromHex(hex));
        this.renderGrid();
        this.updatePreview();
      }
    });

    shadow.querySelector('.export-btn')!.addEventListener('click', () => {
      this.hexArea.value = palette.exportHex();
      this.hexArea.classList.add('visible');
      this.hexArea.select();
    });

    shadow.querySelector('.import-btn')!.addEventListener('click', () => {
      this.hexArea.classList.toggle('visible');
      if (this.hexArea.classList.contains('visible')) {
        this.hexArea.value = '';
        this.hexArea.focus();
        this.hexArea.onkeydown = (e) => {
          if (e.key === 'Enter' && e.ctrlKey) {
            palette.importHex(this.hexArea.value);
            this.hexArea.classList.remove('visible');
            this.hexArea.onkeydown = null;
            this.renderGrid();
            this.updateUI();
          }
        };
      }
    });

    this.renderGrid();
    this.updateUI();
  }

  private renderGrid() {
    const ctx = this.gridCtx;
    const cs = 8; // cell size in canvas pixels
    for (let i = 0; i < 256; i++) {
      const x = (i % 16) * cs;
      const y = Math.floor(i / 16) * cs;
      ctx.fillStyle = palette.colorToCSS(i);
      ctx.fillRect(x, y, cs, cs);
    }
    // Highlight
    const sx = (this.selected % 16) * cs;
    const sy = Math.floor(this.selected / 16) * cs;
    ctx.strokeStyle = '#fff';
    ctx.lineWidth = 1;
    ctx.strokeRect(sx + 0.5, sy + 0.5, cs - 1, cs - 1);
  }

  private updateUI() {
    this.hexInput.value = palette.toHex(this.selected);
    this.updatePreview();
    const idx = this.shadowRoot!.querySelector('.idx')!;
    idx.textContent = `#${this.selected}`;
    this.renderGrid();
  }

  private updatePreview() {
    const preview = this.shadowRoot!.querySelector('.preview') as HTMLElement;
    preview.style.background = palette.colorToCSS(this.selected);
  }
}

customElements.define('b4-palette-editor', B4PaletteEditor);
