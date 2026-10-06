# LD28 map-editor → b4

## Source
- Live JSFiddle: https://jsfiddle.net/tangentstorm/3tDTY/19/
- Local mirror: `ld28.html` / `ld28.coffee` (CoffeeScript + d3)
- Issue: https://github.com/tangentstorm/b4-gd/issues/6

## Behavior (parity)
| Feature | Coffee/d3 | b4 cart |
|---------|-----------|---------|
| Grid | 24×16, cell 25px | 24×16, cell 10px (fits 320×200 canvas) |
| Palette | empty/wall/door/hero/baddie/gold | same colors via `:color` |
| Paint | toolbar + drag on SVG | toolbar sets `B`; `:U` paints from mouse `W` |
| Hero gravity | `step` each frame (~20fps) | `step` every other tick |
| Levels | `localStorage` `levs` / `lv:N` | same keys via thin TS bar |

## Layout
- **Browsable source:** `ld28.b4` `__code__` loaded into the Snippets tab as `ld28` (edit + Run).
- Room RAM: `$800` .. `$97F` (384 bytes) — visible in the mem-browser.
- Registers: `A` hero index, `B` tool, `C` drag, `W` mouse, `T` tick, `U`/`R` callbacks.

## Run (web)
```bash
cd web && npm install && npm run dev
```
Open the Vite URL (usually http://localhost:5173/). The LD28 toolbar boots the cart automatically; or open `?cart=ld28`.

Play/Pause uses the existing ▶ button (`'p gm` / `'P gm`).

## Run (optional Godot)
```bash
# from b4-gd, with carts/ld28.b4 present
godot --path . -- cart=ld28
```

## Thin TS glue
`src/b4-ld28.ts` — palette toolbar, load/save/new against `localStorage`, room↔RAM sync. Game logic stays in the `.b4` cart.
