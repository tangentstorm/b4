const
LB = 128, LI = 129, DU = 130, SW = 131, OV = 132, ZP = 133,
DC = 134, CD = 135, AD = 136, SB = 137, ML = 138, DV = 139,
MD = 140, SH = 141, AN = 142, OR = 143, XR = 144, NT = 145,
EQ = 146, LT = 147, JM = 148, HP = 149, H0 = 150, CL = 151,
RT = 152, NX = 153, RB = 154, WB = 155, RI = 156, WI = 157,
RX = 158, RY = 159, WZ = 160, TG = 176, TA = 177, TW = 178,
TR = 179, TK = 180, TS = 181, TL = 182, TC = 183,
DB = 254, HL = 255;

const [RegX,RegY,RegZ] = [4*24,4*25,4*26]

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

function asm(x) {
  if (x.match(/^-?[0-9A-F]+$/)) return parseInt(x,16)
  let res = op.indexOf(x)
  if (res == -1) throw new Error(`unknown token: ${x}`)
  else return res }

export function dis(x) {
  return op[x] || x.toString(16,0).padStart(2,'0').toUpperCase() }

export class CPU {
  constructor() {
    this.ip = 0x100
    this.cs = []
    this.ds = []
    this.reg = new Array(32)
    this.ram = new Array(4096).fill(0) }
  tos() { return this.ds[this.ds.length-1]}
  nos() { return this.ds[this.ds.length-2]}
  dpop() { return this.ds.pop()}
  cpop() { return this.cs.pop() }
  dput(x) { this.ds.push(x); return this }
  cput(x) { this.cs.push(x) }
  peek(a,len) {
    let res = []
    for (let i=0; i<len; i++) res.push(dis(this.ram[a++]))
    return res.join(' ')}

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
  zp() { this.dpop(); return this }
  du() { return this.dput(this.tos()) }
  sw() { let y=this.dpop(),x=this.dpop(); return this.dput(y).dput(x) }
  ov() { return this.dput(this.nos()) }
  dc() { return this.cput(this.dpop()) }
  cd() { return this.dput(this.cpop()) }
  wb() { this.ram[this.dpop()]=this.dpop(); return this }
  rb() { return this.dput(this.ram[this.dpop()]) }
  ri() {
    let a = this.dpop()+3, r =0
    for (let i=0;i<4;i++) { r<<=8; r+=this.ram[a--] }
    return this.dput(r) }
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
  step() {
    switch(this.ram[this.ip++]){
    case LB: this.dput(this.ram[this.ip++]); break
/*    LB = 128, LI = 129, DU = 130, SW = 131, OV = 132, ZP = 133,
      DC = 134, CD = 135, AD = 136, SB = 137, ML = 138, DV = 139,
      MD = 140, SH = 141, AN = 142, OR = 143, XR = 144, NT = 145,
      EQ = 146, LT = 147, JM = 148, HP = 149, H0 = 150, CL = 151,
      RT = 152, NX = 153, RB = 154, WB = 155, RI = 156, WI = 157,
      RX = 158, RY = 159, WZ = 160, TG = 176, TA = 177, TW = 178,
      TR = 179, TK = 180, TS = 181, TL = 182, TC = 183,
      DB = 254, HL = 255; */
    default: break}}}



const b4 = new CPU()

const hex=b=>b.toString(16,0).toUpperCase()

function dump(xs) {
  return `[${xs.map(hex).join(' ')}]` }

export let out = console.log
export function b4i(line){
  if (line.startsWith('!')) {
    let [a0,...xs] = line.split(/\s+/)
    let a = parseInt(a0.slice(1), 16)
    for (let x of xs) { b4.ram[a++]=asm(x) }
    return }
  else for (let tok of line.split(' ')) {
    switch (tok) {
    case '?c': out(`cs: ${dump(b4.cs)}`); break;
    case '?d': out(`ds: ${dump(b4.ds)}`); break;
    case '?i': out(`ip: ${hex(b4.ip)}`); break;
    case '%q': process.exit(1); break;
    case '%s': b4.step(); break;
    default:
      if (tok.match(/^[0-9A-F]+$/)){
        b4.ds.push(parseInt(tok,16))}
      else if (tok[0]=="'") {
        if (tok.length==2) b4.ds.push(tok[1].charCodeAt(0))
        else if (tok.length==1) b4.ds.push(32)
        else out(`unknown command: ${tok}\n`)}
      else if (tok[0]=="`" && tok.length==2) {
        b4.ds.push(4*(tok[1].charCodeAt(0)-64))}
      else if (tok in b4) { b4[tok]() }
      else if (tok[0]=="@") { out(b4.peek(parseInt(tok.slice(1),16), 16))  }
      else out(`unknown command: ${tok}\n`)}}}
