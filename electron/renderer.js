/**
 * This file is loaded via the <script> tag in the index.html file and will
 * be executed in the renderer process for that window. No Node.js APIs are
 * available in this process because `nodeIntegration` is turned off and
 * `contextIsolation` is turned on. Use the contextBridge API in `preload.js`
 * to expose Node.js functionality from the main process.
 */

document.getElementById('repl-input').focus(); // Auto-focus the REPL prompt

const history = [];
let historyIndex = -1;

const updateStacks = (cs, ds) => {
  const stackOutput = document.getElementById('stack-output');
  stackOutput.textContent = `${cs}\n${ds}`;
};

const submitCommand = () => {
  const input = document.getElementById('repl-input').value;
  if (input.trim() === '') return;
  history.push(input);
  historyIndex = history.length;
  const outputArea = document.getElementById('repl-output');
  const commandElement = document.createElement('div');
  commandElement.className = 'command';
  commandElement.textContent = `> ${input}`;
  outputArea.appendChild(commandElement);
  window.electron.replInput(input).then(() => {
    window.electron.getStacks().then(({ cs, ds }) => {
      updateStacks(cs, ds);
    });
  });
  document.getElementById('repl-input').value = '';
  outputArea.scrollTop = outputArea.scrollHeight;
};

document.getElementById('repl-submit').addEventListener('click', submitCommand);

document.getElementById('repl-input').addEventListener('keydown', (event) => {
  if (event.key === 'Enter') {
    submitCommand();
  } else if (event.key === 'ArrowUp') {
    if (historyIndex > 0) {
      historyIndex--;
      document.getElementById('repl-input').value = history[historyIndex];
    }
  } else if (event.key === 'ArrowDown') {
    if (historyIndex < history.length - 1) {
      historyIndex++;
      document.getElementById('repl-input').value = history[historyIndex];
    } else {
      historyIndex = history.length;
      document.getElementById('repl-input').value = '';
    }
  }
});

window.electron.ipcRenderer.on('repl-output', (msg) => {
  const outputArea = document.getElementById('repl-output');
  const outputElement = document.createElement('div');
  outputElement.textContent = msg;
  outputArea.appendChild(outputElement);
  outputArea.scrollTop = outputArea.scrollHeight;
});

window.electron.getStacks().then(({ cs, ds }) => {
  updateStacks(cs, ds);
});
