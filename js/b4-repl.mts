import { B4VM } from './b4.mjs';

interface ElectronAPI {
  ipcRenderer: {
    on(channel: string, listener: (...args: any[]) => void): void;
  };
  fmtStacks(): { cs: string; ds: string };
  replInput(input: string): void;
}

declare global {
  interface Window {
    electron?: ElectronAPI;
  }
}

interface B4Interface {
  out: (msg: string) => void;
  fmtStacks(): Promise<{ cs: string; ds: string }>;
  b4i(input: string): Promise<void>;
}

export class B4ElectronIpc implements B4Interface {
  private electron: ElectronAPI;
  private _out: (msg: string) => void;

  constructor() {
    if (!window.electron) {
      throw new Error('Electron API not found');
    }
    this.electron = window.electron;
    this._out = console.log;
  }

  set out(listener: (msg: string) => void) {
    this._out = listener;
    this.electron.ipcRenderer.on('repl-output', (msg: string) => {
      listener(msg);
    });
  }

  get out(): (msg: string) => void {
    return this._out;
  }

  fmtStacks(): Promise<{ cs: string; ds: string }> {
    return Promise.resolve(this.electron.fmtStacks());
  }

  b4i(input: string): Promise<void> {
    return Promise.resolve(this.electron.replInput(input));
  }
}

export class B4PromiseWrapper implements B4Interface {
  private vm: B4VM;
  private _out: (msg: string) => void;

  constructor() {
    this.vm = new B4VM();
    this._out = console.log;
  }

  set out(listener: (msg: string) => void) {
    this._out = listener;
    this.vm.out = listener;
  }

  get out(): (msg: string) => void {
    return this._out;
  }

  fmtStacks(): Promise<{ cs: string; ds: string }> {
    return Promise.resolve(this.vm.fmtStacks());
  }

  b4i(input: string): Promise<void> {
    return new Promise((resolve) => {
      this.vm.b4i(input);
      resolve();
    });
  }
}

export class B4ReplCmpt extends HTMLElement {
  private history: string[];
  private historyIndex: number;
  private vm: B4Interface | null;

  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
    this.history = [];
    this.historyIndex = -1;
    this.vm = null;
  }

  connectedCallback(): void {
    this.render();
    this.shadowRoot!.getElementById('repl-input')!.focus();
    this.shadowRoot!.getElementById('repl-submit')!.addEventListener('click', this.submitCommand.bind(this));
    this.shadowRoot!.getElementById('repl-input')!.addEventListener('keydown', this.handleKeyDown.bind(this));
    if (this.getAttribute('connect') === 'electron') {
      this.vm = new B4ElectronIpc();
    } else {
      this.vm = new B4PromiseWrapper();
    }
    this.vm.out = this.handleReplOutput.bind(this);
    this.vm.fmtStacks().then(({ cs, ds }) => {
      this.updateStacks(cs, ds);
    });
  }

  render(): void {
    this.shadowRoot!.innerHTML = `
      <style>
        :host {
          display: flex;
          flex-direction: column;
          height: 100%;
          box-sizing: border-box;
          padding: 10px;
        }
        #repl-output-container {
          flex: 1;
          overflow-y: auto;
          border: 1px solid lightgray;
          padding: 10px;
          display: flex;
          flex-direction: column;
        }
        #spacer {
          flex-grow: 1;
        }
        #repl-output {
          display: flex;
          flex-direction: column;
          font-family: monospace;
          white-space: pre;
        }
        #stack-container {
          display: flex;
          justify-content: space-between;
        }
        #repl-input-container {
          display: flex;
        }
        #repl-input {
          flex: 1;
          padding: 10px;
          font-family: monospace;
        }
        #repl-submit {
          padding: 10px;
        }
        .command {
          font-weight: bold;
        }
      </style>
      <div id="repl-output-container">
        <div id="spacer"></div>
        <div id="repl-output"></div>
      </div>
      <div id="stack-container">
        <div id="data-stack"></div>
        <div id="control-stack"></div>
      </div>
      <div id="repl-input-container">
        <input id="repl-input" type="text" />
        <button id="repl-submit">Submit</button>
      </div>
    `;
  }

  updateStacks(cs: string, ds: string): void {
    this.shadowRoot!.getElementById('data-stack')!.textContent = ds;
    this.shadowRoot!.getElementById('control-stack')!.textContent = cs;
  }

  submitCommand(): void {
    const input = (this.shadowRoot!.getElementById('repl-input') as HTMLInputElement).value;
    if (input.trim() === '') return;
    if (this.history.length === 0 || this.history[this.history.length - 1] !== input) {
      this.history.push(input);
      this.historyIndex = this.history.length;
    }
    const outputArea = this.shadowRoot!.getElementById('repl-output')!;
    const commandElement = document.createElement('div');
    commandElement.className = 'command';
    commandElement.textContent = `> ${input}`;
    outputArea.appendChild(commandElement);
    this.vm!.b4i(input).then(() => {
      this.vm!.fmtStacks().then(({ cs, ds }) => {
        this.updateStacks(cs, ds);
      });
    });
    (this.shadowRoot!.getElementById('repl-input') as HTMLInputElement).value = '';
    commandElement.scrollIntoView({ behavior: 'smooth' });
  }

  handleKeyDown(event: KeyboardEvent): void {
    const inputElement = this.shadowRoot!.getElementById('repl-input') as HTMLInputElement;
    if (event.key === 'Enter') {
      this.submitCommand();
      this.historyIndex = this.history.length; // Reset history index to the end
    } else if (event.key === 'ArrowUp') {
      if (this.historyIndex > 0) {
        this.historyIndex--;
        inputElement.value = this.history[this.historyIndex];
      }
    } else if (event.key === 'ArrowDown') {
      if (this.historyIndex < this.history.length - 1) {
        this.historyIndex++;
        inputElement.value = this.history[this.historyIndex];
      } else {
        this.historyIndex = this.history.length;
        inputElement.value = this.history[this.historyIndex - 1] || '';
      }
    }
  }

  handleReplOutput(msg: string): void {
    const outputArea = this.shadowRoot!.getElementById('repl-output')!;
    const outputElement = document.createElement('div');
    outputElement.textContent = msg;
    outputArea.appendChild(outputElement);
    outputElement.scrollIntoView({ behavior: 'smooth' });
  }
}

customElements.define('b4-repl', B4ReplCmpt);
