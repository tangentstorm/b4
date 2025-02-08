import { app, BrowserWindow, ipcMain } from 'electron';
import path from 'node:path';
import { fileURLToPath } from 'url';
import * as b4 from '../js/b4.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

let vm;

function createWindow () {
  const mainWindow = new BrowserWindow({
    width: 1000, // Increase width by 200 pixels
    height: 600,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  });

  mainWindow.loadFile('index.html');
  mainWindow.webContents.openDevTools(); // Open Developer Tools
}

app.whenReady().then(() => {
  createWindow();

  app.on('activate', function () {
    if (BrowserWindow.getAllWindows().length === 0) createWindow();
  });
});

app.on('window-all-closed', function () {
  if (process.platform !== 'darwin') app.quit();
});

ipcMain.handle('repl-input', (event, input) => {
  if (!vm) vm = new b4.B4VM();
  vm.out = (msg) => event.sender.send('repl-output', msg);
  try {
    vm.b4i(input);
  } catch (e) {
    return `Error: ${e.message}`;
  }
});

ipcMain.handle('fmt-stacks', () => {
  if (!vm) vm = new b4.B4VM();
  return vm.fmtStacks();
});
