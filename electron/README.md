# B4 REPL Electron App

This is an Electron application for the B4 REPL.

## How to Run

1. Install dependencies:

```sh
npm install
```

2. Start the Electron application:

```sh
npm start
```

This will open the B4 REPL Electron app.

## Development

- The main Electron process code is in `main.js`.
- The renderer process code is in `renderer.js`.
- The preload script is in `preload.js`.
- The custom elements and B4 VM code are in the `js` directory.
- The HTML and CSS files are in the `electron` directory.

## File Structure

```
electron/
  index.html
  main.js
  package.json
  preload.js
  renderer.js
  styles.css
js/
  b4-repl.mjs
  b4.mjs
  b4i.mjs
  package.json
```

## License

This project is licensed under the MIT License.
