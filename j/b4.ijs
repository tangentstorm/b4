NB. b4 virtual machine
NB. cocurrent 'b4'

NB.
M =: 4096 # 0

NB. CPU
P =: X =: Y =: Z =: 0
D =: 0$0
R =: 0$0
M =: 0$0

NB. helper defs. these are internal definitions
NB. that are only used to define the instruction set.
NB. (at some point, the stacks may just be designated
NB. regions in M, so these let me decouple the op
NB. definitions from the physical memory layout.)
dput =: {{ D =: D, y }}
rput =: {{ R =: R, y }}
dtos =: {{ {: D }}
dnos =: {{ {: }: D }}
dswp =: {{ D =: 1 A. D }}
dpop =: {{ r =. 0 if. #D do. D =: }: D [ r =. {. D end. r }}
rpop =: {{ r =. 0 if. #R do. R =: }: R [ r =. {. R end. r }}
rtos =: {{ {: R }}
mget =: {{ M =: y { M }}
mput =: {{ M =: x y } M }}
incp =: {{ P =: P + 1 }}           NB. ++p
xinc =: {{ r [ X =: 1 + r=.X }}    NB. x++
yinc =: {{ r [ Y =: 1 + r=.Y }}    NB. y++
(pset =: {{ P =: y }})`(xset =: {{ X =: y }})`(yset =: {{ Y =: y }})`(zset =: {{ Z=: y }})
(pget =: {{ P }})     `(xget =: {{ X }})     `(yget =: {{ Y }})     `(zget =: {{ Z }})

NB. monad/dyad: lifts 1/2-arg J verbs to VM
dy =: {{ dput (dpop u dpop) y }}
mo =: {{ dput u dpop y }}

NB. instruction set
NB. ---------------------------------------------------------------------
s_ops=:' li sw du ov zp dr rd'
n_ops=:' ad sb ml dv md'
b_ops=:' an or xr nt'
c_ops=:' eq ne gt lt ge le'
r_ops=:' dx dy dz xd yd zd'
f_ops=:' ok jm j0 j1 cl rt r0 r1 nx'
m_ops=:' rm wm wp rp qp rx wy'
ops =: s: s_ops,n_ops,b_ops,c_ops,r_ops,m_ops,f_ops

NB. stack instructions
li =: dput@mget@pinc             NB. literal->stack
sw =: swap                       NB. swap: xy->yx
du =: dput@dtos                  NB. dup: x->xx
ov =: dput@dnos                  NB. over: xy->xyx
zp =: dpop                       NB. zap: xy->x
dr =: rput@dpop                  NB. data -> return stack
rd =: dput@rpop                  NB. return -> data stack

NB. numeric instructions
(ad =: + dy)`(sb =: - dy)`(ml =: * dy)`(dv =: % dy)`(md =: |~dy)

NB. bitwise instructions (and or xor not)
(an =: (2b1001 b.) dy)` (or =: (2b0111 b.) dy)` (xr =: (2b0110 b.) dy)
nt =: 0&(26 b.) mo    NB. 26 b. is 'not y'. ignores left arg.

NB. comparison instructions
(eq =: =  dy)`(gt =: >  dy)`(lt =: <  dy)
(ne =: ~: dy)`(ge =: >: dy)`(le =: <: dy)

NB. register instructions
(dx =: xset@dpop)` (dy=: yset@dpop)` (dz=: zset@dpop)
(xd =: dput@xget)` (yd=: dput@yget)` (zd=: dput@zget)

NB. control flow instructions
ok =: ]                         NB. ok : no-op
jm =: pset@<:@mget@incp         NB. jump to M[P+1]-1 (because every op is followed by p++)
j0 =: jm`incp@.(0-:dpop)        NB. jump if tos==0 else skip addr
j1 =: jm`incp@.(0~:dpop)        NB. jump if tos!=0 else skip addr
cl =: jm@rput@pget              NB. call. like jump, but push P to return stack first
rt =: pset&(rpop-1:)            NB. return.
r0 =: rt^:(0-:dpop)             NB. return if tos==0
r1 =: rt^:(0~:dpop)             NB. return if tos!=0
nx =: (jm@rput@<:)`incp@.(0-:])@ rpop  NB. 'next': rpop if (rtos--)==0, else jump

NB. memory / messaging instructions
rm =: dput@mget@dpop         NB. (y rm -> x) copy memory addr y to stack
wm =: (dpop mput dpop)       NB. (x y wm -> ) write x to addr y
rx =: dput@mget@xinc         NB. (rx -> v) read from M[x] and increment x
wy =: (dpop mput yinc)       NB. (n wy->)  write to M[y] and increment y
wp =: [:                     NB. TODO: write to port
rp =: [:                     NB. TODO: read from port
qp =: [:                     NB. TODO: query port

NB. ---------------------------------------------------------------------
