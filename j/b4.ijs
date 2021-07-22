NB. b4 virtual machine
NB. cocurrent 'b4'

new =: {{
  NB. CPU
  P =: 0                        NB. program counter
  G =: 0                        NB. group/flags register (private)
  X =: Y =: Z =: 0              NB. registers that programs can use
  C =: 32$0                     NB. addresses of 'character handler'
  NB. Stacks and other RAM
  D =: 0$0                      NB. data stack
  R =: 0$0                      NB. return stack
  M =: (32$0{a.)"_^:(''&-:) y   NB. user-addressable memory
  END =: #M
  0 0$0}}

NB. helper defs. these are internal definitions
NB. that are only used to define the instruction set.
NB. (at some point, the stacks may just be designated
NB. regions in M, so these let me decouple the op
NB. definitions from the physical memory layout.)
AND =: 2b10001 b.               NB. bitwise and
VEL =: 2b10111 b.               NB. bitwise inclusive or
SHL =: 34 b.                    NB. signed shift left
SHR =: -@[ 34 b. ]              NB. signed shift right
NOT =: 0 &(26 b.)               NB. 26 b. is 'not y'. ignores left arg.
bits =: #.32#1                  NB. bitmask (4294967295 in j, _1 in vm)
byte =: #. 8#1                  NB. bitmask (255)
mask =: 32&([SHR SHL)           NB. signed mask

NB. flags/masks for the G register
OGMASK =: 2b0111  NB. op group mask
SKIPPY =: 2b1000  NB. skipping evaluation until we see 'go' op

CMASK =: 31 NB. ctrl code mask


NB. "microcode"
NB. not part of the instruction set, but each instruction
NB. is composed of these building blocks.

dput =: {{ D =: D, y }}
rput =: {{ R =: R, y }}
dtos =: {{ {: D }}
dnos =: {{ {: }: D }}
dswp =: {{ D =: 1 A. D }}
dpop =: {{ r =. 0 if. #D do. D =: }: D [ r =. {: D end. r }}
rpop =: {{ r =. 0 if. #R do. R =: }: R [ r =. {: R end. r }}
bget =: {{ a. i. y { M }}                  NB. fetch a byte  (u8)
sget =: (_127-~-)^:(127&<)@bget            NB. fetch a short (i8)
bput =: {{ M =: x (a. { 256#:y) } M }}     NB. write y as u8 (_1 as 255)
mget =: {{ (4#256)#.a.i.(y+i.4) { M }}     NB. fetch i32
mput =: {{ M =: x (a.{~(4#256)#:y) } M }}  NB. store i32
skpy =: {{ G =: (G AND NOT SKIPPY) VEL SKIPPY * y }}  NB. set skippy bit to y

incp =: {{ P =: mask P + 1 }}         NB. ++p
inc4 =: {{ 3-~P =: mask P + 4 }}      NB. p+=4,p-3
yinc =: {{ r [ X =: mask 1 + r=.X }}  NB. x++
zinc =: {{ r [ Y =: mask 1 + r=.Y }}  NB. y++

NB. get/set each register
(xget =: {{ X }}) ` (xset =: {{ X =: y }})
(yget =: {{ Y }}) ` (yset =: {{ Y =: y }})
(zget =: {{ Z }}) ` (zset =: {{ Z =: y }})
(pget =: {{ P }}) ` (pset =: {{ P =: y }})

NB. there are 32 "C" registers, so cget/set take an argument
(cget =: {{ (32#:y) { C }}) ` (cset =: {{ C =: x (32#:y) } C }})

NB. monad/dyad: lifts 1/2-arg J verbs to VM
dy =: {{ dput mask (dpop u dpop) y }}
mo =: {{ dput mask u dpop y }}

NB. instruction set
NB. ---------------------------------------------------------------------
s_ops=:' si li sw du ov zp dr rd' [ n_ops=:' ad sb ml dv md ng sl sr'
b_ops=:' an or xr nt'             [ c_ops=:' eq ne gt lt ge le'
r_ops=:' dx xd dy yd dz zd dc cd' [ f_ops=:' hl jm hp h0 h1 cl rt r0 r1 ev'
m_ops=:' rm wm yr zw wp rp qp'    [ d_ops=:' bw go'

ops =: ;: s_ops,n_ops,b_ops,c_ops,r_ops,f_ops,m_ops,d_ops

NB. stack instructions
si =: dput@sget@incp             NB. push next short int (signed byte) to data stack
li =: dput@mget@inc4             NB. literal/longint -> push next 4 bytes to data stack as i32
sw =: swap                       NB. swap: xy->yx
du =: dput@dtos                  NB. dup: x->xx
ov =: dput@dnos                  NB. over: xy->xyx
zp =: dpop                       NB. zap: xy->x
dr =: rput@dpop                  NB. data -> return stack
rd =: dput@rpop                  NB. return -> data stack

NB. numeric instructions
(ad =: + dy)`(sb =: - dy)`(ml =: * dy)`(dv =: % dy)`(md =: |~dy)
(ng =: - mo)`(sl =: SHL dy)`(sr =: SHR dy)

NB. bitwise instructions (and or xor not)
(an =: (2b1001 b.) dy)` (or =: (2b0111 b.) dy)` (xr =: (2b0110 b.) dy)
nt =: NOT mo

NB. comparison instructions
(eq =: =  dy)`(gt =: >  dy)`(lt =: <  dy)
(ne =: ~: dy)`(ge =: >: dy)`(le =: <: dy)

NB. register instructions
(dx =: xset@dpop)` (dy=: yset@dpop)` (dz=: zset@dpop) `(dc=: dpop cset AND@CMASK@dpop)
(xd =: dput@xget)` (yd=: dput@yget)` (zd=: dput@zget) `(cd=: dput@cget@AND@CMASK@dpop)

NB. control flow instructions
hl =: pset@END                  NB. halt
jm =: pset@<:@mget@inc4         NB. jump to M[P+1 2 3 4]-1 (because every op is followed by p++)
hp =: pset@<:@(pget+bget)@incp  NB. hop to P+M[P+1]-1 (relative short jump)
h0 =: hp`incp@.(0-:dpop)        NB. hop if tos==0 else skip addr
h1 =: hp`incp@.(0~:dpop)        NB. hop if tos!=0 else skip addr
nx =: (hp@rput@<:)`incp@.(0-:])@ rpop  NB. 'next': if (rtos--)==0 proceed, else hop
cl =: jm@rput@pget              NB. call. like jump, but push P to return stack first
rt =: pset&(rpop-1:)            NB. return
r0 =: rt^:(0-:dpop)             NB. return if tos==0
r1 =: rt^:(0~:dpop)             NB. return if tos!=0
ev =: pset@<:@dpop@rput@pget    NB. eval. like call, but take address from stack instead of M[1+P]

NB. memory / messaging instructions
rm =: dput@mget@dpop         NB. ( a-n) copy memory addr y to stack (i32)
wm =: (dpop mput dpop)       NB. (na- ) write x to addr y
yr =: dput@bget@yinc         NB. (yr -> v) read byte from M[Y:b] and increment Y
zw =: ((8|dpop) bput zinc)   NB. (n zw->)  write low byte of tos to M[Z:b] and increment Z
wp =: [:                     NB. TODO: write to port
rp =: [:                     NB. TODO: read from port
qp =: [:                     NB. TODO: query port

NB. dictionary instructions
NB. 'bw' provides a simple 'dictionary' system at the opcode level.
NB.      the 'words' in the dictionary are the ascii control characters,
NB.      (a.{~i.32) ops 'bw cw' define the 'control word' cw by setting
NB.      the corresponding 'c' register to the adress of the next byte.
NB.      Finally, enter skippy mode so that the definition is ignored.
bw =: skpy@1@(pget cset AND@CMASK@bget@incp)
go =: skpy@0   NB. exit skippy mode

NB. character handler. call address in C with char on stack
NB. this does not have a specific opcode, but is invoked for
NB. every input sequence that designates a valid utf-8
NB. codepoint.
chev =: ev@cget@0@dput

NB. cpu emulator
NB. ---------------------------------------------------------------------
GO =: 128 + ops i. <'go'
grps =: <:dfh&> cut'01 80 c0 c2 e0 f0 f5' NB. start of each group of ops
'gOk gCh gOp gU2 gU3 gU4 gUx' =: i.7

NB. eval: evaluate a single byte code. this allows for a
NB. 'calculator' mode, where the VM is passive and driven
NB. entirely by input from an external source.
eval =: {{
  if. G AND SKIPPY do.
    if. y = GO do. G =: G XOR SKIPPY end.
    return.
  end.
  pg =. G AND OGMASK
  NB. inside a utf-8 sequence, we calculate a number on the stack
  select. pg, g =. grps I. op =. y
  case. gU4, gU3 do. dput (16b1000 * op-16be0) dpop''
  case. gU3, gU2 do. dput (  16b40 * op-16bc0) dpop''
  case. gU2, gOp do. dput (          op-16b80) dpop''
  case. do. NB. otherwise, previous op does not matter.
    select. g
    case. gOk do. NB. nothing. 00 is no-op
    case. gCh do. chev op
    case. gOp do. (ops{~op-128)`:0'' NB. execute vm instruction
    case. gU2 do. dput     16b40 * op - 16bc0
    case. gU3 do. dput   16b1000 * op - 16be0
    case. gU4 do. dput  16b40000 * op - 16bf0
    case. do. NB. nothing. utf-8 bytes>$f5 would encode invalid codepoints,
      NB. and require 4-6 bytes. gU4 makes sense because we want to handle
      NB. valid 4-byte utf-8 encodings. For building numbers higher than
      NB. this, we can just use 'li'.
    end.
  end.
  G =: (G AND NOT OGMASK) XOR g }}

NB. step: read instruction from memory, eval, advance program counter
NB. loop: does same until the program halts.
loop =: {{ while. END > pc=.pget'' do. incp@'' eval bget pc end. 0 0$0 }}
step =: {{ if.    END > pc=.pget'' do. 0 0 $ incp@'' eval bget pc end. }}

NB. these are helpers for tracing the vm in J:
fmt =: ;:'P X Y Z R D M'
log =: {{ (}:,<@,@(' ',.~hfd)@(a.&i.)@>@{:)":@". &.> fmt }}
sho =: fmt ,: log
s =: log@step

NB. launch a new vm with given memory image and trace what happens:
trace =: {{ (fmt,s 0}[:,/ log@step^:a:) 0 0$s=.log new y }}
boot =: [: ". 'D' [[: (log@step^:a:) new

NB. asm: bootstrap assembler: 2 chars per byte
NB. ---------------------------------------------------------------------
NB. a0: strip out spaces. every pair of characters remaining
NB. should be either an opcode mnemonic or a pair of hex digits.
NB. ('ad' for 'add' is both, but you can use 'AD' for the hex number)
NB. other strings are ignored and compile to 00.
NB. Following the retroforth/muri convention, you can use '..'
NB. for 0-padding when you are not explicitly referring to the number 0.

assert 0 128 255 0 -: _2 dfh\'0080ff..'

a0 =: ((128+])`(dfh&>@[)@.((#ops)=]) ops&i.)"0  NB. match boxed 2 char str
a1 =: {{ a0 (_2<\' '-.~]) y }}  NB. src str -> bytecode str (strips all spaces)
asm =: a. {~ a1

assert (128 1 131 136 {a.) -: asm 'si 01 du ad'

NB. '1 dup +' should result in D=.,2
assert (,2) -: boot asm 'si 01 du ad'
assert (,3) -: boot asm 'si 01 du du ad ad'
assert (,4) -: boot asm 'si 01 du ad du ad'

new''
