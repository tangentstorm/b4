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

export interface VGRegisters {
  keys?: string;    // keyboard input bitmask (default 'Q')
  tick?: string;    // frame counter (default 'T')
  mouse?: string;   // mouse state (default 'W')
                    //   bits 0-9:  x (0-1023)
                    //   bits 10-19: y (0-1023)
                    //   bits 20-25: wheel (signed, 6-bit)
                    //   bits 26-31: buttons (up to 6)
  update?: string;  // update callback address (default 'U')
  render?: string;  // render callback address (default 'R')
}

const DEFAULT_REGS: Required<VGRegisters> = {
  keys: 'Q', tick: 'T', mouse: 'W', update: 'U', render: 'R',
};

export function initVG(vm: B4VM, regs?: VGRegisters): VGControls {
  const reg = { ...DEFAULT_REGS, ...regs };
  const b4canvas = document.querySelector('b4-canvas')! as B4Canvas;

  let mouseX = 0, mouseY = 0, mouseButtons = 0, mouseWheel = 0;
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

  // Register 'gm' opcode for game engine
  vm.addOp(0xA0, 'gm', () => {
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
  wrapEl.addEventListener('wheel', (e: WheelEvent) => {
    if (animating) { e.preventDefault(); mouseWheel += Math.sign(e.deltaY); }
  }, { passive: false });

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

  // Animation loop: sets input registers and calls update/render each frame
  const FRAME_MS = 1000 / 30; // 30 fps
  let lastFrame = 0;
  function animationFrame(now: number) {
    if (!animating) return;
    requestAnimationFrame(animationFrame);
    if (now - lastFrame < FRAME_MS) return;
    lastFrame = now;
    (vm as any)._sr(reg.tick, frameCount);
    // Pack mouse into W: bits 0-9 x, 10-19 y, 20-25 wheel (signed 6-bit), 26-31 buttons
    const mx = mouseX & 0x3FF;
    const my = (mouseY & 0x3FF) << 10;
    const mw = (mouseWheel & 0x3F) << 20;
    const mb = (mouseButtons & 0x3F) << 26;
    (vm as any)._sr(reg.mouse, (mx | my | mw | mb) >>> 0);
    mouseWheel = 0; // reset wheel accumulator each frame
    let kbits = 0;
    if (keyState['ArrowLeft']) kbits |= 1;
    if (keyState['ArrowRight']) kbits |= 2;
    if (keyState['ArrowUp'] || keyState['z'] || keyState['x']) kbits |= 4;
    if (keyState['ArrowDown']) kbits |= 8;
    // Gamepad input → merge into kbits
    const gamepads = navigator.getGamepads?.();
    if (gamepads) {
      for (const gp of gamepads) {
        if (!gp) continue;
        // D-pad buttons (standard mapping: 12=up 13=down 14=left 15=right)
        if (gp.buttons[14]?.pressed) kbits |= 1; // left
        if (gp.buttons[15]?.pressed) kbits |= 2; // right
        if (gp.buttons[12]?.pressed) kbits |= 4; // up
        if (gp.buttons[13]?.pressed) kbits |= 8; // down
        // Left stick with deadzone
        if (gp.axes[0] < -0.3) kbits |= 1;       // left
        if (gp.axes[0] > 0.3) kbits |= 2;        // right
        if (gp.axes[1] < -0.3) kbits |= 4;       // up
        if (gp.axes[1] > 0.3) kbits |= 8;        // down
        // A/B buttons → jump
        if (gp.buttons[0]?.pressed) kbits |= 4;
        break; // use first connected gamepad only
      }
    }
    (vm as any)._sr(reg.keys, kbits);
    const addrU = (vm as any)._gr(reg.update);
    const addrR = (vm as any)._gr(reg.render);
    if (addrU) (vm as any).imrun(addrU);
    if (addrR) (vm as any).imrun(addrR);
    frameCount++;
  }

  return controls;
}
