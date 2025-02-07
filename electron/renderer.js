import { ReplComponent } from '../js/b4-repl.mjs';

/**
 * This file is loaded via the <script> tag in the index.html file and will
 * be executed in the renderer process for that window. No Node.js APIs are
 * available in this process because `nodeIntegration` is turned off and
 * `contextIsolation` is turned on. Use the contextBridge API in `preload.js`
 * to expose Node.js functionality from the main process.
 */

document.body.innerHTML = `
  <repl-component id="frontend-repl"></repl-component>
  <repl-component id="backend-repl"></repl-component>
`;

document.body.style.display = 'flex';
document.body.style.flexDirection = 'row';
document.body.style.height = '100vh';
document.body.style.width = '100vw';
document.body.style.margin = '0';

const replComponents = document.querySelectorAll('repl-component');
replComponents.forEach(component => {
  component.style.flex = '1';
  component.style.height = '100%';
  component.style.boxSizing = 'border-box';
  component.style.padding = '10px';
});
