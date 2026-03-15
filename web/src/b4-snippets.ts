import type { B4VM } from '@tangentstorm/b4';
import { EditorView, basicSetup } from 'codemirror';
import { EditorState } from '@codemirror/state';
import { oneDark } from '@codemirror/theme-one-dark';
import { ViewUpdate } from '@codemirror/view';
import { b4a } from './cm6-lang-b4.mts';

const STORAGE_KEY = 'b4-snippets';

export class B4Snippets extends HTMLElement {
  private vm!: B4VM;
  private snippets: Record<string, string> = {};
  private selected: string | null = null;
  private listEl!: HTMLElement;
  private editorView!: EditorView;
  private nameEl!: HTMLInputElement;

  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
  }

  connectedCallback() {
    this.snippets = JSON.parse(localStorage.getItem(STORAGE_KEY) || '{}');
    const shadow = this.shadowRoot!;
    shadow.innerHTML = `
      <style>
        :host { display: flex; height: 100%; color: #ccc; font: 12px monospace; }
        .sidebar {
          width: 140px; min-width: 100px;
          border-right: 1px solid #333;
          display: flex; flex-direction: column;
          overflow-y: auto;
        }
        .sidebar .items { flex: 1; overflow-y: auto; }
        .sidebar .item {
          padding: 4px 8px; cursor: pointer;
          white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
        }
        .sidebar .item:hover { background: #222; }
        .sidebar .item.active { background: #335; color: #fff; }
        .sidebar button {
          background: #222; border: 1px solid #444; color: #ccc;
          font: 12px monospace; padding: 4px; cursor: pointer;
        }
        .sidebar button:hover { background: #333; }
        .editor {
          flex: 1; display: flex; flex-direction: column; min-width: 0;
        }
        .toolbar {
          display: flex; gap: 4px; padding: 4px;
          border-bottom: 1px solid #333; align-items: center;
        }
        .toolbar input {
          flex: 1; background: #1a1a1a; border: 1px solid #444;
          color: #ccc; font: 12px monospace; padding: 2px 6px;
        }
        .toolbar button {
          background: #222; border: 1px solid #444; color: #ccc;
          font: 12px monospace; padding: 2px 8px; cursor: pointer;
        }
        .toolbar button:hover { background: #333; }
        .toolbar button.run { color: #6f6; border-color: #484; }
        .toolbar button.run:hover { background: #243; }
        .toolbar button.del { color: #f66; border-color: #844; }
        .toolbar button.del:hover { background: #422; }
        .cm-host {
          flex: 1; overflow: auto;
        }
        .cm-host .cm-editor { height: 100%; }
        .empty { padding: 16px; color: #666; }
      </style>
      <div class="sidebar">
        <div class="items"></div>
        <button class="new-btn">+ New</button>
      </div>
      <div class="editor">
        <div class="toolbar">
          <input type="text" placeholder="snippet name" />
          <button class="run">Run</button>
          <button class="del">Del</button>
        </div>
        <div class="cm-host"></div>
      </div>
    `;
    this.listEl = shadow.querySelector('.items')!;
    this.nameEl = shadow.querySelector('.toolbar input')!;

    // Create CodeMirror editor
    const cmHost = shadow.querySelector('.cm-host')!;
    this.editorView = new EditorView({
      root: shadow,
      parent: cmHost,
      state: EditorState.create({
        doc: '',
        extensions: [
          basicSetup,
          oneDark,
          b4a(),
          EditorView.updateListener.of((update: ViewUpdate) => {
            if (update.docChanged) this.saveCurrentSnippet();
          }),
        ],
      }),
    });

    shadow.querySelector('.new-btn')!.addEventListener('click', () => this.newSnippet());
    shadow.querySelector('.run')!.addEventListener('click', () => this.runSnippet());
    shadow.querySelector('.del')!.addEventListener('click', () => this.deleteSnippet());
    this.nameEl.addEventListener('change', () => this.renameSnippet());

    this.renderList();
    const names = Object.keys(this.snippets);
    if (names.length > 0) this.select(names[0]);
  }

  setVM(vm: B4VM) { this.vm = vm; }

  private save() {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(this.snippets));
  }

  private setDoc(text: string) {
    this.editorView.dispatch({
      changes: { from: 0, to: this.editorView.state.doc.length, insert: text },
    });
  }

  private getDoc(): string {
    return this.editorView.state.doc.toString();
  }

  private renderList() {
    this.listEl.innerHTML = '';
    for (const name of Object.keys(this.snippets)) {
      const div = document.createElement('div');
      div.className = 'item' + (name === this.selected ? ' active' : '');
      div.textContent = name;
      div.addEventListener('click', () => this.select(name));
      this.listEl.appendChild(div);
    }
  }

  private select(name: string) {
    this.selected = name;
    this.nameEl.value = name;
    this.setDoc(this.snippets[name] || '');
    this.renderList();
  }

  private newSnippet() {
    let i = 1;
    while (this.snippets[`snippet${i}`]) i++;
    const name = `snippet${i}`;
    this.snippets[name] = '';
    this.save();
    this.select(name);
    this.renderList();
    this.nameEl.select();
  }

  private saveCurrentSnippet() {
    if (!this.selected) return;
    this.snippets[this.selected] = this.getDoc();
    this.save();
  }

  private renameSnippet() {
    if (!this.selected) return;
    const newName = this.nameEl.value.trim();
    if (!newName || newName === this.selected) return;
    if (this.snippets[newName] !== undefined) return;
    const code = this.snippets[this.selected];
    delete this.snippets[this.selected];
    this.snippets[newName] = code;
    this.selected = newName;
    this.save();
    this.renderList();
  }

  private runSnippet() {
    if (!this.vm || !this.selected) return;
    const code = this.snippets[this.selected] || '';
    for (const line of code.split('\n')) {
      const trimmed = line.trim();
      if (trimmed) this.vm.b4i(trimmed);
    }
  }

  private deleteSnippet() {
    if (!this.selected) return;
    delete this.snippets[this.selected];
    this.save();
    const names = Object.keys(this.snippets);
    this.selected = names.length > 0 ? names[0] : null;
    if (this.selected) this.select(this.selected);
    else {
      this.nameEl.value = '';
      this.setDoc('');
    }
    this.renderList();
  }
}

customElements.define('b4-snippets', B4Snippets);
