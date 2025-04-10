// b4 virtual machine - javascript edition

function todo(s) { console.error(`TODO: ${s}`) }
function err(s) { throw new Error(s) }

// ops: select code op where !code=(list "%2H") format op where index > 127 from mb4.TBL
//  ", " fuse (list "%u = %i") format ops
const
AD = 128, SB = 129, ML = 130, DV = 131, MD = 132, SH = 133,
AN = 134, OR = 135, XR = 136, NT = 137, EQ = 138, LT = 139,
DU = 140, SW = 141, OV = 142, ZP = 143, DC = 144, CD = 145,
RB = 146, RI = 147, WB = 148, WI = 149, LB = 150, LI = 151,
RS = 152, LS = 153,
JM = 154, HP = 155, H0 = 156, CL = 157, RT = 158, NX = 159,
TM = 190, CV = 191,
C0 = 192, C1 = 193,
IO = 253, DB = 254, HL = 255

// ", " fuse (list "op[%u]='%s'") format select code code from ops
const op = Array(256)
op[ 0]='..',
op[AD]='ad', op[SB]='sb', op[ML]='ml', op[DV]='dv', op[MD]='md', op[SH]='sh',
op[AN]='an', op[OR]='or', op[XR]='xr', op[NT]='nt', op[EQ]='eq', op[LT]='lt',
op[DU]='du', op[SW]='sw', op[OV]='ov', op[ZP]='zp', op[DC]='dc', op[CD]='cd',
op[RB]='rb', op[RI]='ri', op[WB]='wb', op[WI]='wi', op[LB]='lb', op[LI]='li',
op[RS]='rs', op[LS]='ls',
op[JM]='jm', op[HP]='hp', op[H0]='h0', op[CL]='cl', op[RT]='rt', op[NX]='nx',
op[TM]='tm', op[CV]='cv',
op[C0]='c0', op[C1]='c1',
op[IO]='io', op[DB]='db', op[HL]='hl'
for (let i=1;i<32;i++) {
  let c=String.fromCharCode(64+i);
  op[i]=`^${c}`; op[i+32]=`@${c}`; op[i+64]=`!${c}`; op[i+96]=`+${c}` }

const REGS="@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
const rega=(c)=>4*(c.charCodeAt(0)-64)
const HERE=rega('_')
const THERE=rega('\\')
const LAST=rega('^')


// helper routines
const hex=(b)=>b.toString(16,0).toUpperCase()
const hexp=(b,w)=>hex(b).padStart(w,'0')
const isHex=(s)=>s.match(/^-?[0-9A-F]+$/)
const isRegOp=s=>s.match(/[+`@:?!^][@A-Z[\\\]^_]/)

export class B4VM {
  constructor() {
    this.ip = 0x100
    this.cs = []
    this.ds = []
    this.vw = 4
    this.st = 0
    this.ram = new Uint8Array(4096).fill(0)
    this.ob = [] // output buffer
    this._wi(rega("_"), 0x100)
    this._conb = new Map() // custom op name -> byte
    this._cobn = new Map() // custom op byte -> name
    this._cobf = new Map() // custom op byte -> func
    this._labels={}
    this.out = console.log; }
  reset() {
    this.ip = 0x100; this.vw = 4;
    this.cs=[]; this.ds=[]; this.ram = new Uint8Array(4096).fill(0) }

  // microcode
  dtos() { return this.ds[this.ds.length-1]}
  dnos() { return this.ds[this.ds.length-2]}
  dpop() { return this.ds.pop()}
  ctos() { return this.cs[this.cs.length-1]}
  cpop() { return this.cs.pop() }
  dput(x) { this.ds.push(x); return this }
  cput(x) { this.cs.push(x); return this }
  peek(a,len) {
    let res = []
    for (let i=0; i<len; i++) res.push(this.dis(this.ram[a++]))
    return res.join(' ')}

  // arithmetic/logic ops
  ad() { return this.dput(this.dpop() + this.dpop()) }
  ml() { return this.dput(this.dpop() * this.dpop()) }
  sb() { let y=this.dpop(); return this.dput(this.dpop() -y) }
  dv() { let y=this.dpop();
         return this.dput(Math.floor(this.dpop()/y)) }
  md() { let y=this.dpop(); return this.dput(this.dpop()%y) }
  sh() { let y=this.dpop(); return this.dput(this.dpop()<<y) }
  an() { return this.dput(this.dpop()&this.dpop()) }
  or() { return this.dput(this.dpop()|this.dpop()) }
  xr() { return this.dput(this.dpop()^this.dpop()) }
  nt() { return this.dput(~this.dpop()) }
  eq() { let y=this.dpop(); return this.dput(-(this.dpop()==y)) }
  lt() { let y=this.dpop(); return this.dput(-(this.dpop()<y)) }

  // stack ops
  zp() { this.dpop(); return this }
  du() { return this.dput(this.dtos()) }
  sw() { let y=this.dpop(),x=this.dpop(); return this.dput(y).dput(x) }
  ov() { return this.dput(this.dnos()) }
  dc() { return this.cput(this.dpop()) }
  cd() { return this.dput(this.cpop()) }

  // memory ops
  _ri(a0) { let a = a0+3, r =0
    for (let i=0;i<4;i++) { r<<=8; r+=this.ram[a--] } return r }
  _wi(a,n) {
    for (let i=0;i<4;i++) { this.ram[a++]=n&0xff; n>>=8 }}

  // _gr: get register
  _gr(r) { return this._ri(rega(r)) }
  // _sr: set register
  _sr(r, x) { return this._wi(rega(r), x) }


  wb() { this.ram[this.dpop()]=this.dpop(); return this }
  rb() { return this.dput(this.ram[this.dpop()]) }
  ri() { return this.dput(this._ri(this.dpop())) }
  wi() { this._wi(this.dpop(), this.dpop()); return this}

  wv() { return this.wi() } rv(){ return this.ri() }
  c0() { return this.dput(0) }
  c1() { return this.dput(1) }

  // control ops
  lb() { this.dput(this.ram[this.ip+++1]); }
  li() { this.dput(this._ri(this.ip++)); this.ip+=3; }
  _go(a) { this.ip = Math.max(0x100,a)-1 }
  _i8(a) { return (this.ram[a]<<24)>>24 }
  hp() { this._go(this.ip+this._i8(this.ip+1))}
  h0() { if (this.dpop()==0) this.hp(); else this.ip++ }
  jm() { this._go(this._ri(this.ip+1)) }
  cl() { this.cput(this.ip+5); this.jm() }
  rt() { let a = this.cpop(); if (a) this._go(a); else this.st=0 }
  nx() { if (this.ctos()>0) this.cput(this.cpop()-1)
         if (this.ctos()==0) { this.cpop(); this.ip++ }
         else this.hp() }

  _dpopChar() { return String.fromCharCode(this.dpop()) }

  // TODO: i think this needs to be replaced entirely?
  io() {
    let c=this._dpopChar()
    switch (c) {
    case 'e': this.ob.push(this._dpopChar()); break
    default: break }}

  // terminal handler [TODO]
  tm() {
    let op=this._dpopChar();
    // stub emit handler for tests:
    if (op==='e') console.log(this._dpopChar())}

  addOp(opbyte, opname, opfunc) {
    this._conb[opname]=opbyte
    this._cobn[opbyte]=opname
    this._cobf[opbyte]=opfunc}

  er(r) { this.cput(this.ip+1); this._go(this._ri(r)) }
  rr(r) { this.dput(this._ri(r)) }
  wr(r) { this._wi(r, this.dpop()) }
  ir(r) { let d=this.dpop(), v=this._ri(r); this.dput(v); this._wi(r, v+d) }

  step() {
    let op = this.ram[this.ip];
    if (op==0) {}
    else if (op < 0x20) this.er(4*op)
    else if (op < 0x40) this.rr(4*(op-0x20))
    else if (op < 0x60) this.wr(4*(op-0x40))
    else if (op < 0x80) this.ir(4*(op-0x60))
    else switch(op){ // TODO: use a map of bound methods instead
      case DU: this.du(); break; case SW: this.sw(); break
      case OV: this.ov(); break; case ZP: this.zp(); break
      case DC: this.dc(); break; case CD: this.cd(); break
      case AD: this.ad(); break; case SB: this.sb(); break
      case ML: this.ml(); break; case DV: this.dv(); break
      case MD: this.md(); break; case SH: this.sh(); break
      case AN: this.an(); break; case OR: this.or(); break
      case XR: this.xr(); break; case NT: this.nt(); break
      case EQ: this.eq(); break; case LT: this.lt(); break
      case RB: this.rb(); break; case RI: this.ri(); break
      case WB: this.wb(); break; case WI: this.wi(); break
      case LB: this.lb(); break; case LI: this.li(); break
      case JM: this.jm(); break; case HP: this.hp(); break
      case H0: this.h0(); break; case CL: this.cl(); break
      case RT: this.rt(); break; case NX: this.nx(); break
      case C0: this.c0(); break; case C1: this.c1(); break
      case TM: this.tm(); break // todo
      case IO: this.io(); break;
      case DB: this.db(); break; case HL: this.hl(); break
      default:
      let cof=this._cobf[op]; if (cof) { cof() }}
    this.ip++}

  asm(x) {
    if (x.match(/^-?[0-9A-F]+$/)) return parseInt(x,16)
    let res = op.indexOf(x)
    if (res == -1) {
      res = this._conb[x]
      if (!res) err(`unknown token: ${x}`)}
    return res }

  dis(x) {
    //if (x>=32 && x<0x7F) return `'${String.fromCharCode(x)}`
    return op[x] || this._cobn[x] || x.toString(16,0).padStart(2,'0').toUpperCase() }

  fmtStack(which) {
    return `${which}: [${this[which].map(hex).join(' ')}]` }
  fmtStacks() { return {
    cs: this.fmtStack('cs'),
    ds: this.fmtStack('ds') }}
  fmtIp() { return `ip: ${hex(this.ip)}` }

  isRegLabel(x) { return x.length==2 && x[0]==':' && REGS.includes(x[1]) }
  here(){return this._gr("_")}
  regHere(x) { let a=this.here(); this._wi(rega(x),a); return a }
  labelHere(s) { return this._labels[s]=this.here() }

  imrun(a) {
    this.st=1; this.cput(this.ip); this.cput(0); this.ip=a;
    while (this.st) this.step()
    this.ip = this.cpop()}

  // assemble ops to address a0. return byte count
  assemble(a0, ops) {
    let toks = ops.split(' ');
    for (let i=0; i<toks.length; i++) this.ram[a0+i]=this.asm(toks[i]);
    return toks.length}

  // hp is a "here" pointer (generally HERE or THERE)
  // we write to the address in this pointer, then increment it
  asmVia(hp, ops) { let a=this._ri(hp), n=this.assemble(a, ops); this._wi(hp,a+n)}


  b4i(line) {
    const IMM=0, ASM=1, CMT=2, BYE=3;
    let state=IMM, hp=HERE;
    for (let tok of line.split(' ')) {
      let t = tok[0];
      if (state===CMT | t==="#") state = CMT;
      else if (isRegOp(tok)) {
        let r = rega(tok[1]);
        if (t==="?") this.out(hexp(this._ri(r),8))
        else if (t===":") {
          this.regHere(tok[1])
          if (r=='\\') this._go(this._gr('\\')+1);
          state=ASM}
        else if (state===ASM) this.asmVia(hp, t==='`' ? `lb ${tok[1]}` : tok)
        else switch(t) {
          case "`": this.dput(r); break;
          case "^": this.imrun(this._ri(r)); break;
          case "@": this.rr(r); break;
          case "!": this.wr(r); break;
          case "+": this.ir(r); break;
          default: console.warn(`Matched Unknown RegOp: ${tok}`) }}
      else if (t==="\\") { // set the instruction pointer directly
        if (state===ASM) err(`can't use ${tok} in asm mode`)
        let w = tok.slice(1),
            a = ( Object.hasOwn(this._labels, w) ? this._labels[w]
                  : isHex(w) ? parseInt(w, 16)
                  : w.length==1 && REGS.includes(w) ? this._ri(rega(w))
                  : err(`invalid address for '\\': ${tok}`))
        this._go(a+1)}
      else if (t==="%") switch(tok) {
        case '%q': state=BYE; break
        case '%s': this.step(); break
        case '%C': break; // TODO clear
        case '%R': this.reset(); break
        case '%e': break; // TODO run to end
        case '%\\': break; // TODO jump to '\' register
        default: this.out(`%.no: ${tok}`)}
      else if (t==="?") switch(tok) {
        case '?c': this.out(this.fmtStack('cs')); break;
        case '?d': this.out(this.fmtStack('ds')); break;
        case '?i': this.out(this.fmtIp()); break;
        default:
        let a = tok.slice(1);
        if (Object.hasOwn(this._labels, a)) this.out(this.peek(this._labels[a], 16))
        else if (isHex(a)) this.out(this.peek(parseInt(a,16), 16))
        else this.out(`?.no: ${tok}`)}
      else if (t==="$") {
        let a = tok.slice(1); if (Object.hasOwn(this._labels, a)) {
          if (state===ASM) { } // TODO: assemble the label
          else this.dput(this._labels[a])}
        else this.out(`\$.no: ${tok}`)}
      else if (t===":") {
        state = ASM; let a = tok.slice(1);
        if (tok==="::") {} //  ok. assemble from here
        else if (tok===":") this.out("no: :")
        else if (isHex(a)) this._wi(hp=THERE, parseInt(a, 16))
        else this.labelHere(a) }
      else if (t==="'") {
        let a = this._ri(hp);
        for (let ch of tok.slice(1).split("'")) {
          let c;
          if (ch.length == 0) c=32
          else if (ch.length==1) c =ch.codePointAt(0)
          else err(`invalid literal: '${ch}`)
          if (state===ASM) this.ram[a++]=c
          else this.dput(c) }
        if (state===ASM) this._wi(hp,a)}
      else if (tok==="..") { if (state===ASM) this.asmVia(hp, "00"); else {} }
      else if (t===".") {
        // TODO: test for error when not in ASM mode
        if (state===ASM) this.macro(hp, tok)
        else this.out(`not in asm mode: ${tok}`) }
      else if (op.includes(tok)) {
        if (state===ASM) this.asmVia(hp, tok)
        else try { this[tok]() }
        catch (e) { console.log('tried running op:', tok);
                    console.log(op); console.error(e) }}
      else if (isHex(tok)) {
        if (state===ASM) this.asmVia(hp, tok)
        else this.dput(parseInt(tok,16)) }
      else if (Object.hasOwn(this._labels, tok)) {
        if (state===ASM) this.asmCall(hp, tok)
        else this.imrun(this._labels[tok]) }
      else if ('`@!+'.includes(t)) {
        if (Object.hasOwn(this._labels, tok.slice(1))) {
          let a = this._labels[tok.slice(1)];
          switch (t) { // TODO: all of the asmXXX things (regs are handled above)
          case "`":
            if (state===ASM) this.asmInt(hp, a); else this.dput(a)
            break
          case "@":
            if (state===ASM) this.asmGet(hp, a); else {this.dput(a); this.ri()}
            break;
          case "!":
            if (state===ASM) this.asmSet(hp, a); else {this.dput(a); this.wi()}
            break;
          case "+":
            if (state===ASM) this.asmInc(hp, a); else {todo(`+label: ${tok}`)}
            break;}}
        else this.out(`unknown label: ${tok}`); }
      else this.out(`unknown token: ${tok}`); }
    // done with line, now dump output:
    if (this.ob.length) { this.out(this.ob.join("")); this.ob=[] }
    if (state==BYE) process.exit(0) }

  macro(hp, tok) {
    const A = s => this.asmVia(hp, s)
    const M = s => this.macro(hp, s)
    const TO = a => {
      let slot = this.dpop(), dist = (a-slot)+1
      if (dist < 0) console.warn("invalid forward hop")
      else if (dist > 126) console.warn(`hop too far! dist: ${dist}`)
      else this.ram[slot]=dist }
    const HR = _ => this._ri(hp) // read the here-pointer
    const HH = _ => TO(HR())  // hop here
    const BK = _ => { let dist=this.dpop()-HR(); EB(dist+1) } // back
    const EB = x => { let a=HR(); this.ram[a]=x; this._wi(hp, a+1) } // emit byte
    const EI = x => { let a=HR(); this._wi(a, x); this._wi(hp, a+4) } // emit int
    switch (tok) {
    case '._': this.dput(HR()); A(".."); break // emit blank, leaving addr on stack
    case '..': A(tok); break
    // -- if/else/then: cond .i if-body .e .else-body .t
    case '.i': A("h0"); M("._"); break
    case '.e': A("hp"); M("._"); this.sw(); HH(); break
    case '.t': HH(); break
    // -- while loops: .w cond .d body .z
    case '.w': this.dput(HR()); break
    case '.d': A("h0"); M("._"); break
    case '.z': A("hp");this.sw();BK();HH(); break// hop to .w, tell .d where to exit
    case '.o': err(`.o is deprecated. use .z instead`)
    // -- for/next
    case '.f': A("dc"); this.dput(HR()); break
    case '.n': A("nx"); BK(); break
    // linked list builder
    case '.^': let a=HR(); EI(this._gr("^")); this._sr("^", a); break
    default: err(`.no: ${tok}`)}
  }
}


const vm = new B4VM()

export function b4i(line){ vm.b4i(line) }
