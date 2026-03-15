import type { B4VM } from '@tangentstorm/b4';

const COLS = 16;
const TOTAL_BYTES = 4096;
const TOTAL_ROWS = TOTAL_BYTES / COLS;

const hex4 = (n: number) => n.toString(16).toUpperCase().padStart(4, '0');

const STYLE = `
:host {
  display: flex;
  font-family: monospace;
  font-size: 12.5pt;
  background: #000;
  color: #bbb;
  outline: none;
}
.grid-wrap {
  flex: none;
  overflow-y: auto;
  overflow-x: hidden;
  scrollbar-color: #335 #111;
}
.grid-wrap::-webkit-scrollbar { width: 10px; }
.grid-wrap::-webkit-scrollbar-track { background: #111; }
.grid-wrap::-webkit-scrollbar-thumb { background: #335; border-radius: 5px; }
.grid-wrap::-webkit-scrollbar-thumb:hover { background: #447; }
table { border-collapse: collapse; }
th, td {
  padding: 1px 4px;
  text-align: center;
  white-space: nowrap;
  line-height: 1.5;
}
th {
  background: #1a5c5c;
  color: #dff;
  position: sticky;
  top: 0;
  z-index: 1;
  font-weight: normal;
}
td { cursor: pointer; user-select: none; }
.addr {
  background: #0c2e2e;
  color: #5aa;
  text-align: right;
  padding-right: 8px;
  font-weight: bold;
  cursor: default;
}
.z  { color: #555; }
.ip { background: #7a1a1a; color: #fcc; }
.ed { outline: 2px solid #3cb; outline-offset: -1px; }
.ip.ed { outline-color: #f93; }
.cell-edit {
  position: absolute;
  width: 2.2em;
  box-sizing: border-box;
  background: #0a2a2a;
  color: #fff;
  border: 2px solid #3cb;
  font: inherit;
  text-align: center;
  padding: 0 2px;
  line-height: 1.5;
  outline: none;
  z-index: 2;
}

/* sidebar */
.side {
  flex: 1;
  background: #181818;
  border-left: 1px solid #333;
  display: flex;
  flex-direction: column;
  padding: 8px;
  gap: 4px;
}
.side label { color: #688; }
.side input {
  width: 100%;
  background: #0a0a0a;
  color: #eee;
  border: 1px solid #444;
  font-family: inherit;
  font-size: inherit;
  padding: 3px 5px;
  box-sizing: border-box;
}
.side input:focus { border-color: #3cb; outline: none; }
.btns {
  display: flex;
  gap: 4px;
  margin: 4px 0;
}
.btns button {
  flex: 1;
  background: #222;
  color: #ccc;
  border: 1px solid #444;
  padding: 5px;
  font-family: inherit;
  font-size: inherit;
  cursor: pointer;
  font-weight: bold;
}
.btns button:hover { background: #333; color: #fff; }
.btns button:active { background: #444; }
.lbl-wrap {
  flex: 1;
  overflow-y: auto;
  border: 1px solid #333;
  margin-top: 2px;
  scrollbar-color: #335 #111;
}
.lbl-wrap::-webkit-scrollbar { width: 8px; }
.lbl-wrap::-webkit-scrollbar-track { background: #111; }
.lbl-wrap::-webkit-scrollbar-thumb { background: #335; border-radius: 4px; }
.lbl-wrap::-webkit-scrollbar-thumb:hover { background: #447; }
.lbl-wrap table { width: 100%; }
.lbl-wrap th {
  background: #222;
  color: #688;
  position: sticky;
  top: 0;
}
.lbl-wrap td {
  padding: 2px 5px;
  text-align: left;
}
.lbl-wrap tr:hover td { background: #1a3030; }
.lbl-wrap .la { color: #7cb; }
.lbl-wrap .lv { color: #999; text-align: right; }
`;

type RowRef = {
  tr: HTMLTableRowElement;
  cells: HTMLTableCellElement[];
  addrTd: HTMLTableCellElement;
};

export class B4MemBrowser extends HTMLElement {
  private vm: B4VM | null = null;
  private _ed = 0;
  private _cellInput: HTMLInputElement | null = null;

  // cached DOM
  private ipIn!: HTMLInputElement;
  private edIn!: HTMLInputElement;
  private lblBody!: HTMLTableSectionElement;
  private gridWrap!: HTMLDivElement;
  private rows: RowRef[] = [];
  private headerHeight = 0;

  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
  }

  setVM(vm: B4VM): void {
    this.vm = vm;
    this.refresh();
  }

  get ed() { return this._ed; }
  set ed(a: number) {
    this._ed = Math.max(0, Math.min(a, TOTAL_BYTES - 1));
    this.closeCellEdit();
    this.ensureVisible(this._ed);
    this.refresh();
  }

  connectedCallback() {
    this.setAttribute('tabindex', '0');
    this.buildDOM();
    this.setupEvents();
  }

  private buildDOM() {
    let hdr = '<th class="addr"></th>';
    for (let c = 0; c < COLS; c++)
      hdr += `<th>+${c.toString(16).toUpperCase()}</th>`;

    this.shadowRoot!.innerHTML = `
      <style>${STYLE}</style>
      <div class="grid-wrap">
        <table><thead><tr>${hdr}</tr></thead><tbody></tbody></table>
      </div>
      <div class="side">
        <label>ip:</label>
        <input id="ip" value="0100" />
        <label>ed:</label>
        <input id="ed" value="0000" />
        <div class="btns">
          <button id="xs" title="step one instruction">%s</button>
          <button id="xc" title="run until halt or breakpoint">%g</button>
        </div>
        <div class="lbl-wrap">
          <table><thead><tr><th>label</th><th>addr</th></tr></thead>
          <tbody id="lb"></tbody></table>
        </div>
      </div>`;

    // cache refs
    this.ipIn = this.shadowRoot!.getElementById('ip') as HTMLInputElement;
    this.edIn = this.shadowRoot!.getElementById('ed') as HTMLInputElement;
    this.lblBody = this.shadowRoot!.getElementById('lb') as HTMLTableSectionElement;
    this.gridWrap = this.shadowRoot!.querySelector('.grid-wrap') as HTMLDivElement;

    const tbody = this.shadowRoot!.querySelector('.grid-wrap tbody')!;

    // Build all 256 rows directly — no virtual scrolling needed
    for (let r = 0; r < TOTAL_ROWS; r++) {
      const tr = document.createElement('tr');
      const addrTd = document.createElement('td');
      addrTd.className = 'addr';
      addrTd.textContent = hex4(r * COLS);
      tr.appendChild(addrTd);
      const cells: HTMLTableCellElement[] = [];
      for (let c = 0; c < COLS; c++) {
        const td = document.createElement('td');
        td.dataset.a = String(r * COLS + c);
        td.textContent = '..';
        tr.appendChild(td);
        cells.push(td);
      }
      this.rows.push({ tr, cells, addrTd });
      tbody.appendChild(tr);
    }
  }

  private ensureVisible(addr: number) {
    const row = Math.floor(addr / COLS);
    if (row < 0 || row >= this.rows.length) return;
    this.rows[row].tr.scrollIntoView({ block: 'nearest' });
  }

  private cellForAddr(addr: number): HTMLTableCellElement | null {
    const row = Math.floor(addr / COLS);
    const col = addr % COLS;
    if (row < 0 || row >= this.rows.length) return null;
    return this.rows[row].cells[col];
  }

  // -- events --

  private setupEvents() {
    // cell click → select ed
    this.shadowRoot!.querySelector('.grid-wrap tbody')!
      .addEventListener('click', (e) => {
        const td = (e.target as HTMLElement).closest('td[data-a]') as HTMLTableCellElement;
        if (td) {
          this.ed = parseInt(td.dataset.a!, 10);
          this.focus();
        }
      });

    // buttons
    this.shadowRoot!.getElementById('xs')!.addEventListener('click', () => this.doStep());
    this.shadowRoot!.getElementById('xc')!.addEventListener('click', () => this.doContinue());

    // ip/ed inputs
    this.ipIn.addEventListener('change', () => {
      if (!this.vm) return;
      const v = parseInt(this.ipIn.value, 16);
      if (!isNaN(v)) { this.vm.ip = v; this.refresh(); }
    });
    this.edIn.addEventListener('change', () => {
      const v = parseInt(this.edIn.value, 16);
      if (!isNaN(v)) this.ed = v;
    });

    // keyboard
    this.addEventListener('keydown', (e) => this.onKey(e));
  }

  // -- refresh --

  refresh(): void {
    if (!this.vm || !this.ipIn) return;
    const { ip, ram } = this.vm;
    const ed = this._ed;

    for (let r = 0; r < TOTAL_ROWS; r++) {
      const base = r * COLS;
      const p = this.rows[r];
      for (let c = 0; c < COLS; c++) {
        const addr = base + c;
        const b = ram[addr];
        const cell = p.cells[c];
        cell.textContent = this.vm!.dis(b);
        let cls = '';
        if (b === 0) cls = 'z';
        if (addr === ip) cls += (cls ? ' ' : '') + 'ip';
        if (addr === ed) cls += (cls ? ' ' : '') + 'ed';
        cell.className = cls;
      }
    }

    this.ipIn.value = hex4(this.vm.ip);
    this.edIn.value = hex4(this._ed);
    this.refreshLabels();
  }

  private refreshLabels() {
    if (!this.vm) return;
    const labels: Record<string, number> = (this.vm as any)._labels;
    if (!labels) return;

    const entries = Object.entries(labels).sort((a, b) => a[1] - b[1]);
    // only rebuild if count changed
    if (this.lblBody.rows.length !== entries.length) {
      this.lblBody.innerHTML = '';
      for (const [name, addr] of entries) {
        const tr = document.createElement('tr');
        tr.innerHTML = `<td class="la">${name}</td><td class="lv">${hex4(addr)}</td>`;
        tr.addEventListener('click', () => { this.ed = addr; this.scrollToAddress(addr); });
        this.lblBody.appendChild(tr);
      }
    } else {
      let i = 0;
      for (const [name, addr] of entries) {
        const tr = this.lblBody.rows[i++];
        tr.cells[0].textContent = name;
        tr.cells[1].textContent = hex4(addr);
      }
    }
  }

  scrollToAddress(addr: number) {
    const row = Math.floor(addr / COLS);
    if (row < 0 || row >= this.rows.length) return;
    this.rows[row].tr.scrollIntoView({ block: 'center' });
  }

  scrollToIP() {
    if (!this.vm) return;
    this.scrollToAddress(this.vm.ip);
  }

  // -- actions --

  private doStep() {
    if (!this.vm) return;
    this.vm.step();
    this.refresh();
    this.scrollToIP();
    this.emitChanged();
  }

  private doContinue() {
    if (!this.vm) return;
    this.vm.st = 1;
    this.vm.dbg = 0;
    let limit = 100_000;
    while (this.vm.st && !this.vm.dbg && --limit > 0) this.vm.step();
    this.refresh();
    this.scrollToIP();
    this.emitChanged();
  }

  private emitChanged() {
    this.dispatchEvent(new CustomEvent('vm-changed', { bubbles: true }));
  }

  // -- keyboard hex editor --

  private onKey(e: KeyboardEvent) {
    if (!this.vm) return;
    if (this._cellInput) return; // edit box handles its own keys
    // ignore if focus is on a sidebar input
    if ((e.target as HTMLElement).tagName === 'INPUT') return;

    switch (e.key) {
      case 'ArrowRight': this.ed = this._ed + 1; e.preventDefault(); break;
      case 'ArrowLeft':  this.ed = this._ed - 1; e.preventDefault(); break;
      case 'ArrowDown':  this.ed = this._ed + COLS; e.preventDefault(); break;
      case 'ArrowUp':    this.ed = this._ed - COLS; e.preventDefault(); break;
      case 'Enter':
        this.openCellEdit();
        e.preventDefault();
        break;
    }
  }

  private openCellEdit() {
    if (!this.vm || this._cellInput) return;
    const cell = this.cellForAddr(this._ed);
    if (!cell) return;

    const inp = document.createElement('input');
    inp.className = 'cell-edit';
    inp.maxLength = 2;
    inp.value = this.vm.ram[this._ed].toString(16).toUpperCase().padStart(2, '0');

    // position over the cell
    cell.style.position = 'relative';
    inp.style.top = '0';
    inp.style.left = '0';
    cell.appendChild(inp);
    inp.focus();
    inp.select();

    let done = false;
    const commit = () => {
      if (done) return;
      done = true;
      const v = parseInt(inp.value, 16);
      const valid = !isNaN(v) && v >= 0 && v <= 0xFF;
      if (valid) this.vm!.ram[this._ed] = v;
      this.closeCellEdit();
      this.refresh();
      this.focus();
      if (valid) this.emitChanged();
    };
    const cancel = () => {
      if (done) return;
      done = true;
      this.closeCellEdit();
      this.refresh();
      this.focus();
    };

    inp.addEventListener('keydown', (e) => {
      if (e.key === 'Enter') { commit(); e.preventDefault(); }
      else if (e.key === 'Escape') { cancel(); e.preventDefault(); }
      else if (e.key === 'Tab') {
        commit();
        done = false; // allow next cell's commit
        this.ed = this._ed + (e.shiftKey ? -1 : 1);
        this.openCellEdit();
        e.preventDefault();
      }
      e.stopPropagation();
    });
    inp.addEventListener('blur', () => commit());

    this._cellInput = inp;
  }

  private closeCellEdit() {
    if (!this._cellInput) return;
    const inp = this._cellInput;
    this._cellInput = null; // clear first to prevent re-entry from blur
    if (inp.parentElement) {
      inp.parentElement.style.position = '';
      inp.remove();
    }
  }
}

customElements.define('b4-mem-browser', B4MemBrowser);
