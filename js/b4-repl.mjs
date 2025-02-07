class ReplComponent extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
    this.history = [];
    this.historyIndex = -1;
  }

  connectedCallback() {
    this.render();
    this.shadowRoot.getElementById('repl-input').focus();
    this.shadowRoot.getElementById('repl-submit').addEventListener('click', this.submitCommand.bind(this));
    this.shadowRoot.getElementById('repl-input').addEventListener('keydown', this.handleKeyDown.bind(this));
    window.electron.ipcRenderer.on('repl-output', this.handleReplOutput.bind(this));
    window.electron.getStacks().then(({ cs, ds }) => {
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
        }
        #repl-output {
          flex: 1;
          overflow-y: auto;
          border: 1px solid lightgray;
          padding: 10px;
          display: flex;
          flex-direction: column-reverse;
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
    window.electron.replInput(input).then(() => {
      window.electron.getStacks().then(({ cs, ds }) => {
        this.updateStacks(cs, ds);
      });
    });
    this.shadowRoot.getElementById('repl-input').value = '';
    outputArea.scrollTop = 0;
  }

  handleKeyDown(event) {
    if (event.key === 'Enter') {
      this.submitCommand();
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

customElements.define('repl-component', ReplComponent);

export { ReplComponent };