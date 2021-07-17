NB. b4 virtual machine
NB. cocurrent 'b4'

NB. CPU
P =: 0              NB. program counter
G =: 0              NB. group/flags register (private)
X =: Y =: Z =: 0    NB. registers that programs can use

NB. Stacks and other RAM
D =:    0$0         NB. data stack
R =:    0$0         NB. return stack
M =: 4096$0         NB. user-addressable memory

NB. helper defs. these are internal definitions
NB. that are only used to define the instruction set.
NB. (at some point, the stacks may just be designated
NB. regions in M, so these let me decouple the op
NB. definitions from the physical memory layout.)
AND =: 2b10001 b.
SHL =: 34 b.            NB. signed shift left
SHR =: -@[ 34 b. ]      NB. signed shift right
NOT =: 0 &(26 b.)       NB. 26 b. is 'not y'. ignores left arg.
bits =: #.32#1          NB. bitmask (4294967295 in j, _1 in vm)
byte =: #. 8#1          NB. bitmask (255)
mask =: 32&([SHR SHL)   NB. signed mask
bytes =: (4#256)&#:     NB. i32 -> 4 bytes
END =: #M

NB. "microcode"

dput =: {{ D =: D, y }}
rput =: {{ R =: R, y }}
dtos =: {{ {: D }}
dnos =: {{ {: }: D }}
dswp =: {{ D =: 1 A. D }}
dpop =: {{ r =. 0 if. #D do. D =: }: D [ r =. {. D end. r }}
rpop =: {{ r =. 0 if. #R do. R =: }: R [ r =. {. R end. r }}
rtos =: {{ {: R }}
mget =: {{ y { M }}                   NB. fetch i32 from ram
mput =: {{ M =: x y } M }}
bget =: {{ (y AND 3) { bytes mget 2 SHR y }}
bput =: {{ t =. (mget a) AND NOT (s=.8*b) SHL byte [ a=. 2 SHR y [ b=.y AND 3
           (t XOR s SHL x) mput a }}

incp =: {{ P =: mask P + 1 }}         NB. ++p
yinc =: {{ r [ X =: mask 1 + r=.X }}  NB. x++
zinc =: {{ r [ Y =: mask 1 + r=.Y }}  NB. y++
(pset =: {{ P =: y }})`(xset =: {{ X =: y }})`(yset =: {{ Y =: y }})`(zset =: {{ Z=: y }})
(pget =: {{ P }})     `(xget =: {{ X }})     `(yget =: {{ Y }})     `(zget =: {{ Z }})

NB. monad/dyad: lifts 1/2-arg J verbs to VM
dy =: {{ dput mask (dpop u dpop) y }}
mo =: {{ dput mask u dpop y }}


NB. instruction set
NB. ---------------------------------------------------------------------
s_ops=:' li sw du ov zp dr rd'
n_ops=:' ad sb ml dv md ng sl sr'
b_ops=:' an or xr nt'
c_ops=:' eq ne gt lt ge le'
r_ops=:' dx dy dz xd yd zd'
f_ops=:' ok hl jm j0 j1 cl rt r0 r1 nx'
m_ops=:' rm wm wp rp qp ry wz'
ops =: ;: s_ops,n_ops,b_ops,c_ops,r_ops,f_ops,m_ops

NB. stack instructions
li =: dput@mget@incp             NB. literal->stack
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
(dx =: xset@dpop)` (dy=: yset@dpop)` (dz=: zset@dpop)
(xd =: dput@xget)` (yd=: dput@yget)` (zd=: dput@zget)

NB. control flow instructions
ok =: ]                         NB. ok : no-op
hl =: pset@END                  NB. halt
jm =: pset@<:@mget@incp         NB. jump to M[P+1]-1 (because every op is followed by p++)
j0 =: jm`incp@.(0-:dpop)        NB. jump if tos==0 else skip addr
j1 =: jm`incp@.(0~:dpop)        NB. jump if tos!=0 else skip addr
cl =: jm@rput@pget              NB. call. like jump, but push P to return stack first
rt =: pset&(rpop-1:)            NB. return.
r0 =: rt^:(0-:dpop)             NB. return if tos==0
r1 =: rt^:(0~:dpop)             NB. return if tos!=0
nx =: (jm@rput@<:)`incp@.(0-:])@ rpop  NB. 'next': rpop if (rtos--)==0, else jump

NB. memory / messaging instructions
rm =: dput@mget@dpop         NB. (y rm -> x) copy memory addr y to stack (i32)
wm =: (dpop mput dpop)       NB. (x y wm -> ) write x to addr y
ry =: dput@bget@yinc         NB. (rx -> v) read byte from M[x:b] and increment x
wz =: ((8|dpop) bput zinc)   NB. (n wy->)  write low byet of tos to M[y:b] and increment y
wp =: [:                     NB. TODO: write to port
rp =: [:                     NB. TODO: read from port
qp =: [:                     NB. TODO: query port

NB. interpreter
NB. ---------------------------------------------------------------------
grps =: dfh&> cut '7f 80 c0 e0 f0 f5 f' NB. cutoffs for groups of ops
step =: {{
  if. END >: pc =.pget'' do.
    pg =. G AND 7
    NB. instructions are 1 per byte, packed into 32-bit cells.
    for_op. bytes mget pc do.
      NB. inside a utf-8 sequence, we calculate a number on the stack
      select. pg, g =. grps I. op
      case. 4 3 do. dput (16b1000 * op-16be0) dpop''
      case. 3 2 do. dput (  16b40 * op-16bc0) dpop''
      case. 2 1 do. dput (          op-16b80) dpop''
      case. do. NB. otherwise, previous op does not matter.
        select. g
        case. 0 do. NB. if op is non-zero ascii char c, M[Z++:b]:=c
          if. op > 0 do. wz dput op end.
        case. 1 do. (ops{~128-op)`:0'' NB. execute vm instruction
        case. 2 do. dput     16b40 * op - 16bc0
        case. 3 do. dput   16b1000 * op - 16be0
        case. 4 do. dput  16b40000 * op - 16bf0
        case.   do.
        end.
      end.
    end.
    pset 1 + pget''
  end. 0 0$0}}

s =: {{ echo P;X;Y;Z;R;D[step'' }}


NB. bootstrap assembler: 2-char assembly code
NB. ---------------------------------------------------------------------
NB. a0: strip out spaces. every pair of characters remaining
NB. should be either an opcode mnemonic or a pair of hex digits.
NB. ('ad' for 'add' is both, but you can use 'AD' for the hex number)
NB. other strings are ignored and compile to 00.
NB. Following the retroforth/muri convention, you can use '..'
NB. for 0-padding when you are not explicitly referring to the number 0.

asm =: ((128+])`(dfh&>@[)@.((#ops)=]) ops&i.)"0 @ (_2<\' '-.~])
