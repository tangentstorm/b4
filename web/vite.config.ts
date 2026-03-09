import { defineConfig } from 'vite';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export default defineConfig(({ mode }) => ({
  resolve: {
    preserveSymlinks: true,
    alias: mode === 'development' ? {
      '@tangentstorm/b4/repl':         path.resolve(__dirname, '../js/b4-repl.mts'),
      '@tangentstorm/b4/mem-browser':  path.resolve(__dirname, '../js/b4-mem-browser.mts'),
      '@tangentstorm/b4':              path.resolve(__dirname, '../js/b4.mts'),
    } : {},
  }
}));
