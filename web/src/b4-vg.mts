import type { B4VM } from '@tangentstorm/b4';
import './b4-canvas';
import type { B4Canvas } from './b4-canvas';
import { palette } from './b4-palette';
import { spriteStore, SPRITE_SIZE, SPRITE_W, SPRITE_H, MAP_W, MAP_H } from './b4-sprites';

export interface VGControls {
  isAnimating(): boolean;
  start(): void;
  stop(): void;
  onStateChange: ((animating: boolean) => void) | null;
}

export function initVG(vm: B4VM): VGControls {
  const b4canvas = document.querySelector('b4-canvas')! as B4Canvas;

  let mouseX = 0, mouseY = 0, mouseButtons = 0;
  let frameCount = 0;
  let animating = false;
  let stateCallback: ((animating: boolean) => void) | null = null;

  function setAnimating(v: boolean) {
    animating = v;
    if (stateCallback) stateCallback(v);
  }

  // Keyboard state: bit0=left, bit1=right, bit2=up/jump, bit3=down
  const keyState: Record<string, boolean> = {};
  document.addEventListener('keydown', (e) => {
    keyState[e.key] = true;
    if (animating && ['ArrowLeft','ArrowRight','ArrowUp','ArrowDown','z','x'].includes(e.key))
      e.preventDefault();
  });
  document.addEventListener('keyup', (e) => { keyState[e.key] = false; });

  // Register 'vg' opcode for video graphics
  vm.addOp(0xA0, 'vg', () => {
    const cmd = (vm as any)._dpopChar();
    switch (cmd) {
      case 'v': b4canvas.invert(); break;
      case 'f': b4canvas.setFill(vm.dpop()); break;
      case 'b': {
        const h = vm.dpop(), w = vm.dpop(), y = vm.dpop(), x = vm.dpop();
        b4canvas.drawBox(x, y, w, h);
      } break;
      case 'X': b4canvas.clear(); break;
      case 'c': {
        const r = vm.dpop(), cy = vm.dpop(), cx = vm.dpop();
        b4canvas.drawCircle(cx, cy, r);
      } break;
      case 'm': (vm as any).dput(mouseX); (vm as any).dput(mouseY); break;
      case 'p': setAnimating(true); frameCount = 0; requestAnimationFrame(animationFrame); break;
      case 'P': setAnimating(false); break;
      case 's': {
        // Draw sprite: ( sprite-index x y -- )
        const y = vm.dpop(), x = vm.dpop(), si = vm.dpop();
        b4canvas.drawSprite(spriteStore.getSpriteData(si), palette.colors, x, y);
      } break;
      case 'g': {
        // Map get: ( x y -- tile )
        const gy = vm.dpop(), gx = vm.dpop();
        if (gx >= 0 && gx < MAP_W && gy >= 0 && gy < MAP_H)
          (vm as any).dput(spriteStore.map[gy * MAP_W + gx]);
        else (vm as any).dput(0);
      } break;
      case 'F': {
        // Draw sprite flipped horizontally: ( sprite-index x y -- )
        const fy = vm.dpop(), fx = vm.dpop(), fsi = vm.dpop();
        const src = spriteStore.getSpriteData(fsi);
        const flipped = new Uint8Array(SPRITE_SIZE);
        for (let py = 0; py < SPRITE_H; py++)
          for (let px = 0; px < SPRITE_W; px++)
            flipped[py * SPRITE_W + px] = src[py * SPRITE_W + (SPRITE_W - 1 - px)];
        b4canvas.drawSprite(flipped, palette.colors, fx, fy);
      } break;
      case 'M': {
        // Draw map region: ( map-x map-y screen-x screen-y w h -- )
        const h = vm.dpop(), w = vm.dpop();
        const sy = vm.dpop(), sx = vm.dpop();
        const my = vm.dpop(), mx = vm.dpop();
        b4canvas.drawMapRegion(
          spriteStore.map, MAP_W,
          spriteStore.sprites, SPRITE_SIZE,
          palette.colors,
          mx, my, sx, sy, w, h
        );
      } break;
    }
  });

  // Track mouse over the canvas element
  const canvasEl = b4canvas.getCanvas();
  const wrapEl = canvasEl.parentElement!;
  wrapEl.addEventListener('mousemove', (e: MouseEvent) => {
    const rect = canvasEl.getBoundingClientRect();
    const scaleX = b4canvas.getWidth() / rect.width;
    const scaleY = b4canvas.getHeight() / rect.height;
    mouseX = Math.floor((e.clientX - rect.left) * scaleX);
    mouseY = Math.floor((e.clientY - rect.top) * scaleY);
    mouseX = Math.max(0, Math.min(b4canvas.getWidth() - 1, mouseX));
    mouseY = Math.max(0, Math.min(b4canvas.getHeight() - 1, mouseY));
  });
  wrapEl.addEventListener('mousedown', (e: MouseEvent) => { mouseButtons |= (1 << e.button); });
  wrapEl.addEventListener('mouseup', (e: MouseEvent) => { mouseButtons &= ~(1 << e.button); });

  const controls: VGControls = {
    isAnimating: () => animating,
    start() {
      if (!animating) {
        setAnimating(true);
        frameCount = 0;
        requestAnimationFrame(animationFrame);
      }
    },
    stop() {
      setAnimating(false);
    },
    get onStateChange() { return stateCallback; },
    set onStateChange(fn: ((a: boolean) => void) | null) { stateCallback = fn; },
  };

  // Animation loop: sets input registers and calls ^U / ^R each frame
  // S = game input state (button bitmask)
  // T = frame count since start
  // W = mouse: X in low 16 bits, Y in high 16 bits
  const FRAME_MS = 1000 / 30; // 30 fps
  let lastFrame = 0;
  function animationFrame(now: number) {
    if (!animating) return;
    requestAnimationFrame(animationFrame);
    if (now - lastFrame < FRAME_MS) return;
    lastFrame = now;
    (vm as any)._sr('T', frameCount);
    (vm as any)._sr('W', (mouseY << 16) | (mouseX & 0xFFFF));
    let kbits = 0;
    if (keyState['ArrowLeft']) kbits |= 1;
    if (keyState['ArrowRight']) kbits |= 2;
    if (keyState['ArrowUp'] || keyState['z'] || keyState['x']) kbits |= 4;
    if (keyState['ArrowDown']) kbits |= 8;
    (vm as any)._sr('S', mouseButtons | kbits);
    const addrU = (vm as any)._gr('U');
    const addrR = (vm as any)._gr('R');
    if (addrU) (vm as any).imrun(addrU);
    if (addrR) (vm as any).imrun(addrR);
    frameCount++;
  }

  return controls;
}
