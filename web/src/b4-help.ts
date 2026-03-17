export class B4Help extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: 'open' });
  }

  connectedCallback() {
    this.shadowRoot!.innerHTML = `
      <style>
        :host {
          display: block; height: 100%; overflow-y: auto;
          color: #ccc; font: 12px monospace; background: #111;
          padding: 8px 16px;
        }
        h1 { font-size: 16px; color: #fff; margin: 0 0 4px; }
        h2 {
          font-size: 13px; color: #8af; margin: 16px 0 4px;
          border-bottom: 1px solid #333; padding-bottom: 2px;
        }
        h3 { font-size: 12px; color: #aaa; margin: 10px 0 2px; font-style: italic; }
        .cols { display: flex; gap: 24px; flex-wrap: wrap; }
        .col { flex: 1; min-width: 200px; }
        table { border-collapse: collapse; width: 100%; margin: 2px 0 8px; }
        td { padding: 1px 6px; vertical-align: top; }
        td:first-child { color: #ff6; white-space: nowrap; font-weight: bold; }
        td:last-child { color: #aaa; }
        tr:hover { background: #1a1a2a; }
        code { color: #ff6; }
        .note { color: #888; font-size: 11px; font-style: italic; }
        .section { margin-bottom: 12px; }
      </style>

      <h1>B4 VM Reference</h1>
      <p class="note">A tiny forth-like virtual machine</p>

      <div class="cols">
        <div class="col">
          <h2>Syntax</h2>
          <table>
            <tr><td>FF</td><td>hex byte literal</td></tr>
            <tr><td>$FF</td><td>hex literal (assembler)</td></tr>
            <tr><td>'c</td><td>ASCII char literal</td></tr>
            <tr><td>:foo</td><td>define word</td></tr>
            <tr><td>:X</td><td>set register X</td></tr>
            <tr><td>foo</td><td>call word</td></tr>
            <tr><td>^X</td><td>call register X</td></tr>
            <tr><td>#</td><td>line comment</td></tr>
            <tr><td>;</td><td>end definition (= <code>rt</code>)</td></tr>
          </table>

          <h2>Stack Notation</h2>
          <p class="note">( before &mdash; after ) &nbsp; x=nos, y=tos</p>

          <h2>Constants</h2>
          <table>
            <tr><td>c0</td><td>push 0</td></tr>
            <tr><td>c1</td><td>push 1</td></tr>
            <tr><td>c2</td><td>push 2</td></tr>
            <tr><td>c4</td><td>push 4</td></tr>
            <tr><td>n1</td><td>push -1</td></tr>
          </table>

          <h2>Literals</h2>
          <table>
            <tr><td>lb</td><td>load unsigned byte ( &mdash; n)</td></tr>
            <tr><td>li</td><td>load int32 ( &mdash; n)</td></tr>
            <tr><td>ls</td><td>load signed byte ( &mdash; n)</td></tr>
          </table>

          <h2>Registers</h2>
          <p class="note">32 registers: @ through _ and A&ndash;Z</p>
          <table>
            <tr><td>@X</td><td>fetch register X ( &mdash; n)</td></tr>
            <tr><td>!X</td><td>store to register X (n &mdash; )</td></tr>
            <tr><td>^X</td><td>invoke register X as function</td></tr>
            <tr><td>+X</td><td>add to register X (m &mdash; old)</td></tr>
          </table>
        </div>

        <div class="col">
          <h2>Arithmetic</h2>
          <table>
            <tr><td>ad</td><td>add (xy &mdash; x+y)</td></tr>
            <tr><td>sb</td><td>subtract (xy &mdash; x-y)</td></tr>
            <tr><td>ml</td><td>multiply (xy &mdash; x*y)</td></tr>
            <tr><td>dv</td><td>divide (xy &mdash; x/y)</td></tr>
            <tr><td>md</td><td>modulo (xy &mdash; x%y)</td></tr>
            <tr><td>sh</td><td>shift left (xy &mdash; x&lt;&lt;y)</td></tr>
          </table>

          <h2>Logic</h2>
          <table>
            <tr><td>an</td><td>bitwise AND (xy &mdash; r)</td></tr>
            <tr><td>or</td><td>bitwise OR (xy &mdash; r)</td></tr>
            <tr><td>xr</td><td>bitwise XOR (xy &mdash; r)</td></tr>
            <tr><td>nt</td><td>bitwise NOT (x &mdash; ~x)</td></tr>
          </table>

          <h2>Comparison</h2>
          <table>
            <tr><td>eq</td><td>equal? (xy &mdash; b)</td></tr>
            <tr><td>lt</td><td>less than? (xy &mdash; b)</td></tr>
          </table>
          <p class="note">b = -1 if true, else 0</p>

          <h2>Stack</h2>
          <table>
            <tr><td>du</td><td>duplicate (x &mdash; xx)</td></tr>
            <tr><td>sw</td><td>swap (xy &mdash; yx)</td></tr>
            <tr><td>ov</td><td>over (xy &mdash; xyx)</td></tr>
            <tr><td>zp</td><td>drop (x &mdash; )</td></tr>
            <tr><td>dc</td><td>data&rarr;ctrl (ds:x &mdash; cs:x)</td></tr>
            <tr><td>cd</td><td>ctrl&rarr;data (cs:x &mdash; ds:x)</td></tr>
          </table>

          <h2>Memory</h2>
          <table>
            <tr><td>rb</td><td>read unsigned byte (a &mdash; n)</td></tr>
            <tr><td>rs</td><td>read signed byte (a &mdash; n)</td></tr>
            <tr><td>ri</td><td>read int32 (a &mdash; n)</td></tr>
            <tr><td>wb</td><td>write byte (x a &mdash; )</td></tr>
            <tr><td>wi</td><td>write int32 (x a &mdash; )</td></tr>
          </table>
        </div>

        <div class="col">
          <h2>Control Flow</h2>
          <table>
            <tr><td>jm</td><td>jump to addr in next 4 bytes</td></tr>
            <tr><td>hp</td><td>hop: add signed byte to ip</td></tr>
            <tr><td>h0</td><td>hop if zero (x &mdash; )</td></tr>
            <tr><td>cl</td><td>call: push ip, jump to addr</td></tr>
            <tr><td>rt</td><td>return: pop cs to ip</td></tr>
            <tr><td>nx</td><td>next: dec cs, hop if &gt; 0</td></tr>
            <tr><td>hl</td><td>halt vm</td></tr>
            <tr><td>db</td><td>trigger debugger</td></tr>
          </table>

          <h2>Assembler Macros</h2>
          <table>
            <tr><td>.f</td><td>for: begin counted loop</td></tr>
            <tr><td>.n</td><td>next: loop or break</td></tr>
            <tr><td>.i</td><td>if: skip block if tos = 0</td></tr>
            <tr><td>.e</td><td>else</td></tr>
            <tr><td>.t</td><td>then: end if/else</td></tr>
            <tr><td>.w</td><td>while: begin loop</td></tr>
            <tr><td>.d</td><td>do: begin body</td></tr>
            <tr><td>.o</td><td>od: end while</td></tr>
          </table>

          <h2>b4i Interactive</h2>
          <h3>Inspect</h3>
          <table>
            <tr><td>?d</td><td>data stack</td></tr>
            <tr><td>?c</td><td>control stack</td></tr>
            <tr><td>?i</td><td>instruction pointer</td></tr>
            <tr><td>?R</td><td>register R (e.g. ?H)</td></tr>
            <tr><td>?100</td><td>dump 16 bytes at addr</td></tr>
          </table>
          <h3>Control</h3>
          <table>
            <tr><td>/</td><td>step one instruction</td></tr>
            <tr><td>//</td><td>run until db or hl</td></tr>
            <tr><td>/1234</td><td>jump to hex address</td></tr>
            <tr><td>/C</td><td>clear VM, dictionary, forwards</td></tr>
            <tr><td>/R</td><td>reset stacks and IP</td></tr>
          </table>
        </div>

        <div class="col">
          <h2>Game Engine (ge)</h2>
          <p class="note">Custom opcode at $A0. Sub-commands via <code>lb 'X gm</code></p>

          <h3>Drawing</h3>
          <table>
            <tr><td>'f gm</td><td>set fill color (color &mdash; )</td></tr>
            <tr><td>'X gm</td><td>clear screen</td></tr>
            <tr><td>'b gm</td><td>draw box (x y w h &mdash; )</td></tr>
            <tr><td>'c gm</td><td>draw circle (cx cy r &mdash; )</td></tr>
            <tr><td>'s gm</td><td>draw sprite (si x y &mdash; )</td></tr>
            <tr><td>'F gm</td><td>draw sprite flipped (si x y &mdash; )</td></tr>
            <tr><td>'v gm</td><td>invert screen</td></tr>
          </table>

          <h3>Map</h3>
          <table>
            <tr><td>'g gm</td><td>get map tile (x y &mdash; tile)</td></tr>
            <tr><td>'M gm</td><td>draw map region (mx my sx sy w h &mdash; )</td></tr>
          </table>

          <h3>Input</h3>
          <table>
            <tr><td>'m gm</td><td>push mouse x, y</td></tr>
            <tr><td>@Q</td><td>keyboard state bitmask</td></tr>
            <tr><td>@W</td><td>mouse (packed 32-bit)</td></tr>
            <tr><td>@T</td><td>frame count since start</td></tr>
          </table>
          <p class="note">Q bits: 0=left 1=right 2=up/jump 3=down<br>
          W bits: 0-9=x, 10-19=y, 20-25=wheel, 26-31=buttons</p>

          <h3>Game Loop</h3>
          <table>
            <tr><td>'p gm</td><td>start animation (30fps)</td></tr>
            <tr><td>'P gm</td><td>stop animation</td></tr>
          </table>
          <p class="note">Define <code>:I</code> (init), <code>:U</code> (update), <code>:R</code> (render).<br>
          Use <code>^I 'p gm</code> to init &amp; start the loop.</p>

          <h2>Colors</h2>
          <p class="note">32-bit AARRGGBB format.<br>
          Example: <code>li $FF000000</code> = opaque black<br>
          <code>li $FFFF0000</code> = red, <code>li $FF00FF00</code> = green</p>
        </div>
      </div>
    `;
  }
}

customElements.define('b4-help', B4Help);
