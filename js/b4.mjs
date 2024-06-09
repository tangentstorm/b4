// b4 virtual machine - javascript edition

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

// helper routines
const hex=(b)=>b.toString(16,0).toUpperCase()
const hexp=(b,w)=>hex(b).padStart(w,'0')



export class B4VM {
  constructor() {
    this.ip = 0x100
    this.cs = []
    this.ds = []
    this.vw = 4
    this.st = 0
    this.ram = new Uint8Array(4096).fill(0)
    this.ob = [] // output buffer
    this._wi(this.rega("_"), 0x100)
    this._conb = new Map() // custom op name -> byte
    this._cobn = new Map() // custom op byte -> name
    this._cobf = new Map() // custom op byte -> func
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
  _i8(a) { let r=this.ram[a]; if (r>=0x80) r=-(r&0x7F)-1; return r }
  hp() { this._go(this.ip+this._i8(this.ip+1))}
  h0() { if (this.dpop()==0) this.hp(); else this.ip++ }
  jm() { this._go(this._ri(this.ip+1)) }
  cl() { this.cput(this.ip+5); this.jm() }
  rt() { let a = this.cpop(); if (a) this._go(a); else this.st=0 }
  nx() { if (this.ctos()>0) this.cput(this.cpop()-1)
         if (this.ctos()==0) { this.cpop(); this.ip++ }
         else this.hp() }

  io() {
    let c=String.fromCharCode(this.dpop()&0x7F)
    switch (c) {
    case 'e': this.ob.push(String.fromCharCode(this.dpop())); break
    default: break }}

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
      if (!res) throw new Error(`unknown token: ${x}`)}
    return res }

  dis(x) {
    //if (x>=32 && x<0x7F) return `'${String.fromCharCode(x)}`
    return op[x] || this._cobn[x] || x.toString(16,0).padStart(2,'0').toUpperCase() }

  fmtStack(which) {
    return `${which}: [${this[which].map(hex).join(' ')}]` }
  fmtIp() { return `ip: ${hex(this.ip)}` }

  rega(c) { return 4*(c.charCodeAt(0)-64) }
  isRegLabel(x) { return x.length==2 && x[0]==':' && REGS.includes(x[1]) }
  regHere(x) { let a=this._ri(this.rega("_")); this._wi(this.rega(x),a); return a }

  imrun(a) {
    this.st=1; this.cput(this.ip); this.cput(0); this.ip=a;
    while (this.st) this.step()
    this.ip = this.cpop()}


  b4i(line) {
    let done =0;
    if (line.startsWith(':')) {
      let a0,[x0,...xs] = line.split(/\s+/)
      let a=a0=this.isRegLabel(x0) ? this.regHere(x0[1]) : parseInt(x0.slice(1), 16)
      for (let x of xs) {
        if (x[0]=="'") for (let c of x.slice(1).split("'")) {
          this.ram[a++]=c.codePointAt(0)}
        else if (this.isRegLabel(x)) this.regHere(x[1])
        else this.ram[a++]=this.asm(x) }
      this.dput(a-a0); this.ir(this.rega("_")); this.dpop() // update "HERE" pointer
      return }
    else for (let tok of line.split(' ')) {
      if (done) break;
      switch (tok) {
      case '?c': this.out(this.fmtStack('cs')); break;
      case '?d': this.out(this.fmtStack('ds')); break;
      case '?i': this.out(this.fmtIp()); break;
      case '%q': done=1; break;
      case '%s': this.step(); break;
      case '%C': ; break;
      default:
        if (tok.match(/^[0-9A-F]+$/)){
          this.ds.push(parseInt(tok,16))}
        else if (tok[0]=="'") {
          if (tok.length==2) this.dput(tok[1].charCodeAt(0))
          else if (tok.length==1) this.dput(32)
          else this.out(`unknown command: ${tok}\n`)}
        else if (tok.length==2 && REGS.includes(tok[1])) {
          let r = this.rega(tok[1]); switch(tok[0]) {
            case "?": this.out(hexp(this._ri(r),8)); break;
            case "`": this.dput(r); break;
            case "^": this.imrun(this._ri(r)); break;
            case "@": this.rr(r); break;
            case "!": this.wr(r); break;
            case "+": this.ir(r); break;}}
        else if (tok in this) { this[tok]() }
        else if (tok in this._conb) {  this._cobf[this._conb[tok]]() }
        else if (tok[0]=="?") {
          this.out(this.peek(parseInt(tok.slice(1),16), 16))  }
        else this.out(`unknown command: ${tok}\n`)}}
    if (this.ob.length) { this.out(this.ob.join("")); this.ob=[] }
    if (done) process.exit(0) }}


const vm = new B4VM()

export function b4i(line){ vm.b4i(line) }
