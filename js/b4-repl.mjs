import { B4VM } from './b4.mjs';

export class B4ElectronIpc {
  constructor() {
    this.electron = window.electron;
    this.out = console.log;
  }

  set out(listener) {
    this.electron.ipcRenderer.on('repl-output', (...args) => {
      listener(...args)});
  }

  fmtStacks() {
    return this.electron.fmtStacks();
  }

  b4i(input) {
    return this.electron.replInput(input);
  }
}

export class B4PromiseWrapper {
  constructor() {
    this.vm = new B4VM();
    this.out = console.log;
  }

  set out(listener) {
    this.vm.out = listener;
  }

  fmtStacks() {
    return new Promise((resolve) => {
      resolve(this.vm.fmtStacks());
    });
  }

  b4i(input) {
    return new Promise((resolve) => {
      this.vm.b4i(input);
      resolve();
    });
  }
}

export class B4ReplCmpt extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
    this.history = [];
    this.historyIndex = -1;
    this.vm = null;
  }

  connectedCallback() {
    this.render();
    this.shadowRoot.getElementById('repl-input').focus();
    this.shadowRoot.getElementById('repl-submit').addEventListener('click', this.submitCommand.bind(this));
    this.shadowRoot.getElementById('repl-input').addEventListener('keydown', this.handleKeyDown.bind(this));
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

  render() {
    this.shadowRoot.innerHTML = `
      <style>
        :host {
          display: flex;
          flex-direction: column;
          height: 100%;
          box-sizing: border-box;
          padding: 10px;
        }
        #repl-output {
          flex: 1;
          overflow-y: auto;
          border: 1px solid lightgray;
          padding: 10px;
          display: flex;
          flex-direction: column-reverse;
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
      <div id="repl-output"></div>
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

  updateStacks(cs, ds) {
    this.shadowRoot.getElementById('data-stack').textContent = ds;
    this.shadowRoot.getElementById('control-stack').textContent = cs;
  }

  submitCommand() {
    const input = this.shadowRoot.getElementById('repl-input').value;
    if (input.trim() === '') return;
    if (this.history.length === 0 || this.history[this.history.length - 1] !== input) {
      this.history.push(input);
      this.historyIndex = this.history.length;
    }
    const outputArea = this.shadowRoot.getElementById('repl-output');
    const commandElement = document.createElement('div');
    commandElement.className = 'command';
    commandElement.textContent = `> ${input}`;
    outputArea.prepend(commandElement);
    this.vm.b4i(input).then(() => {
      this.vm.fmtStacks().then(({ cs, ds }) => {
        this.updateStacks(cs, ds);
      });
    });
    this.shadowRoot.getElementById('repl-input').value = '';
    outputArea.scrollTop = 0;
  }

  handleKeyDown(event) {
    if (event.key === 'Enter') {
      this.submitCommand();
      this.historyIndex = this.history.length; // Reset history index to the end
    } else if (event.key === 'ArrowUp') {
      if (this.historyIndex > 0) {
        this.historyIndex--;
        this.shadowRoot.getElementById('repl-input').value = this.history[this.historyIndex];
      }
    } else if (event.key === 'ArrowDown') {
      if (this.historyIndex < this.history.length - 1) {
        this.historyIndex++;
        this.shadowRoot.getElementById('repl-input').value = this.history[this.historyIndex];
      } else {
        this.historyIndex = this.history.length;
        this.shadowRoot.getElementById('repl-input').value = this.history[this.historyIndex - 1] || '';
      }
    }
  }

  handleReplOutput(msg) {
    const outputArea = this.shadowRoot.getElementById('repl-output');
    const outputElement = document.createElement('div');
    outputElement.textContent = msg;
    outputArea.appendChild(outputElement);
    outputArea.scrollTop = outputArea.scrollHeight;
  }
}

customElements.define('b4-repl', B4ReplCmpt);
