// b4 virtual machine - javascript edition

const
LB = 128, LI = 129, DU = 130, SW = 131, OV = 132, ZP = 133,
DC = 134, CD = 135, AD = 136, SB = 137, ML = 138, DV = 139,
MD = 140, SH = 141, AN = 142, OR = 143, XR = 144, NT = 145,
EQ = 146, LT = 147, JM = 148, HP = 149, H0 = 150, CL = 151,
RT = 152, NX = 153, RB = 154, WB = 155, RI = 156, WI = 157,
RX = 158, RY = 159, WZ = 160, TG = 176, TA = 177, TW = 178,
TR = 179, TK = 180, TS = 181, TL = 182, TC = 183,
DB = 254, HL = 255;

const op = Array(256)
op[ 0]='..',
op[LB]='lb', op[LI]='li', op[DU]='du', op[SW]='sw', op[OV]='ov', op[ZP]='zp',
op[DC]='dc', op[CD]='cd', op[AD]='ad', op[SB]='sb', op[ML]='ml', op[DV]='dv',
op[MD]='md', op[SH]='sh', op[AN]='an', op[OR]='or', op[XR]='xr', op[NT]='nt',
op[EQ]='eq', op[LT]='lt', op[JM]='jm', op[HP]='hp', op[H0]='h0', op[CL]='cl',
op[RT]='rt', op[NX]='nx', op[RB]='rb', op[WB]='wb', op[RI]='ri', op[WI]='wi',
op[RX]='rx', op[RY]='ry', op[WZ]='wz', op[TG]='tg', op[TA]='ta', op[TW]='tw',
op[TR]='tr', op[TK]='tk', op[TS]='ts', op[TL]='tl', op[TC]='tc',
op[DB]='db', op[HL]='hl';
for (let i=1;i<32;i++) op[i]='^'+String.fromCharCode(64+i)
op[0x7F]='^?'

const [RegX,RegY,RegZ] = [4*24,4*25,4*26]

// helper routines
const hex=b=>b.toString(16,0).toUpperCase()


export class B4VM {
  constructor() {
    this.ip = 0x100
    this.cs = []
    this.ds = []
    this.reg = new Array(32)
    this.ram = new Uint8Array(4096).fill(0)
    this.out = console.log }

  // microcode
  dtos() { return this.ds[this.ds.length-1]}
  dnos() { return this.ds[this.ds.length-2]}
  dpop() { return this.ds.pop()}
  ctos() { return this.cs[this.cs.length-1]}
  cpop() { return this.cs.pop() }
  dput(x) { this.ds.push(x); return this }
  cput(x) { this.cs.push(x) }
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
  wb() { this.ram[this.dpop()]=this.dpop(); return this }
  rb() { return this.dput(this.ram[this.dpop()]) }
  _ri(a0) {
    let a = a0+3, r =0
    for (let i=0;i<4;i++) { r<<=8; r+=this.ram[a--] } return r }
  ri() { return this.dput(this._ri(this.dpop())) }
  wi() {
    let a = this.dpop(), n=this.dpop()
    for (let i=0;i<4;i++) { this.ram[a++]=n&0xff; n>>=8 }
    return this}
  rx() { this.dput(RegX).ri().ri()
         return this.dput(RegX).du().ri().dput(4).ad().sw().wi() }
  ry() { this.dput(RegY).ri().ri()
         return this.dput(RegY).du().ri().dput(4).ad().sw().wi() }
  wz() { this.dput(RegZ).ri().wi()
         return this.dput(RegZ).du().ri().dput(4).ad().sw().wi() }

  // control ops
  lb() { this.dput(this.ram[this.ip+++1]); }
  li() { this.dput(this._ri(this.ip++)); this.ip+=3; }
  _go(a) { this.ip = Math.max(0x100,a)-1 }
  _i8(a) { let r=this.ram[a]; if (r>=0x80) r=-(r&0x7F)-1; return r }
  hp() { this._go(this.ip+this._i8(this.ip+1))}
  h0() { if (this.dpop()==0) this.hp(); else this.ip++ }
  jm() { this._go(this._ri(this.ip+1)) }
  cl() { this.cput(this.ip+4); this.jm() }
  rt() { this._go(this.cpop()) }
  nx() { if (this.ctos()>0) this.cput(this.cpop()-1)
         if (this.ctos()==0) { this.cpop(); this.ip++ }
         else this.hp() }

  step() {
    switch(this.ram[this.ip]){ // TODO: use a map of bound methods instead
    case LB: this.lb(); break; case LI: thisli(); break
    case DU: this.du(); break; case SW: this.sw(); break
    case OV: this.ov(); break; case ZP: this.zp(); break
    case DC: this.dc(); break; case CD: this.cd(); break
    case AD: this.ad(); break; case SB: this.sb(); break
    case ML: this.ml(); break; case DV: this.dv(); break
    case MD: this.md(); break; case SH: this.sh(); break
    case AN: this.an(); break; case OR: this.or(); break
    case XR: this.xr(); break; case NT: this.nt(); break
    case EQ: this.eq(); break; case LT: this.lt(); break
    case JM: this.jm(); break; case HP: this.hp(); break
    case H0: this.h0(); break; case CL: this.cl(); break
    case RT: this.rt(); break; case NX: this.nx(); break
    case RB: this.rb(); break; case WB: this.wb(); break
    case RI: this.ri(); break; case WI: this.wi(); break
    case RX: this.rx(); break; case RY: this.ry(); break
    case WZ: this.wz(); break; case TG: this.tg(); break
    case TA: this.ta(); break; case TW: this.tw(); break
    case TR: this.tr(); break; case TK: this.tk(); break
    case TS: this.ts(); break; case TL: this.tl(); break
    case TC: this.tc(); break
    case DB: this.db(); break; case HL: this.hl(); break
    default: break}
    this.ip++}

  asm(x) {
    if (x.match(/^-?[0-9A-F]+$/)) return parseInt(x,16)
    let res = op.indexOf(x)
    if (res == -1) throw new Error(`unknown token: ${x}`)
    else return res }

  dis(x) {
    if (x>=32 && x<0x7F) return `'${String.fromCharCode(x)}`
    return op[x] || x.toString(16,0).padStart(2,'0').toUpperCase() }

  fmtStack(which) {
    return `${which}: [${this[which].map(hex).join(' ')}]` }

  b4i(line) {
    if (line.startsWith('!')) {
      let [a0,...xs] = line.split(/\s+/)
      let a = parseInt(a0.slice(1), 16)
      for (let x of xs) { this.ram[a++]=this.asm(x) }
      return }
    else for (let tok of line.split(' ')) {
      switch (tok) {
      case '?c': this.out(this.fmtStack('cs')); break;
      case '?d': this.out(this.fmtStack('ds')); break;
      case '?i': this.out(`ip: ${hex(this.ip)}`); break;
      case '%q': process.exit(1); break;
      case '%s': this.step(); break;
      default:
        if (tok.match(/^[0-9A-F]+$/)){
          this.ds.push(parseInt(tok,16))}
        else if (tok[0]=="'") {
          if (tok.length==2) this.ds.push(tok[1].charCodeAt(0))
          else if (tok.length==1) this.ds.push(32)
          else this.out(`unknown command: ${tok}\n`)}
        else if (tok[0]=="`" && tok.length==2) {
          this.ds.push(4*(tok[1].charCodeAt(0)-64))}
        else if (tok in this) { this[tok]() }
        else if (tok[0]=="@") {
          this.out(this.peek(parseInt(tok.slice(1),16), 16))  }
        else this.out(`unknown command: ${tok}\n`)}}} }

const vm = new B4VM()

export function b4i(line){ vm.b4i(line) }
