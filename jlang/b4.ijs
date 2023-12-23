NB. b4 virtual machine
cocurrent 'b4'

MSZ =: 1024                     NB. initial memory size, in bytes

create =: {{
  NB. CPU
  P =: 0                        NB. program counter
  G =: 0                        NB. group/flags register (private)
  R =: 32$0                     NB. registers
  D =: 0$0                      NB. data stack
  C =: 0$0                      NB. call stack
  M =: (MSZ$0{a.)"_^:(''&-:) y  NB. user-addressable memory
  END =: #M
  0 0$0}}

reset =: create                 NB. reset to initial state

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
RMASK =: 31 NB. ctrl code mask

ord =: a.&i.
rega =: 4*(ord'@') -~ ord NB. backtick = register address
XREG =: rega'X'
YREG =: rega'Y'
ZREG =: rega'Z'

NB. "microcode"
NB. not part of the instruction set, but each instruction
NB. is composed of these building blocks.

dput =: {{ D =: D, y }}
cput =: {{ C =: C, y }}
dtos =: {{ {: D }}
dnos =: {{ {: }: D }}
dswp =: {{ D =: 1 A. D }}
dpop =: {{ r =. 0 if. #D do. D =: }: D [ r =. {: D end. r }}
cpop =: {{ r =. 0 if. #C do. C =: }: C [ r =. {: C end. r }}
bget =: {{ a. i. y { M }}                  NB. fetch a byte  (u8)
sget =: (>:@-)^:(127&<)@bget               NB. fetch a short (i8)
bset =: {{ M =: (a.{~  256  #:x) y } M }}  NB. store x:u8 at y (_1->FF)
mset =: {{ M =: (a.{~(4#256)#:x) (y+i._4) } M }}  NB. store i32 x at y
mget =: {{ (4#256)#.a.i.(y+i._4) { M }}     NB. fetch i32

incp =: {{ P =: mask P + 1 }}         NB. ++p
inc4 =: {{ 3-~P =: mask P + 4 }}      NB. p+=4,p-3

NB. get/set internal registers
(pget =: {{ P }}) ` (pset =: {{ P =: y }})

NB. there are 32 "R" registers, so rget/set take an argument
(rget =: {{ (32#:y) { R }}) ` (rset =: {{ R =: x (32#:y) } R }})

NB. monad/dyad: lifts 1/2-arg J verbs to VM
dy =: {{ dput mask (dpop u dpop) y }}
mo =: {{ dput mask u dpop y }}

NB. instruction set
NB. ---------------------------------------------------------------------
s_ops=:' lb li du sw ov zp dc cd'  NB. stack ops
n_ops=:' ad sb ml dv md sh'        NB. math ops
b_ops=:' an or xr nt'              NB. bitwise ops
c_ops=:' eq lt'                    NB. comparison ops
f_ops=:' ok jm hp h0 cl rt'        NB. flow ops
m_ops=:' rb wb ri wi'              NB. memory ops
h_ops=:' io'                       NB. hardware i/o ops
e_ops=:' rx ry wz nx'              NB. extended ops

ops =: ;: s_ops,n_ops,b_ops,c_ops,f_ops,m_ops,h_ops,e_ops

ok =: ]                          NB. no-op ... this should be op 0 though

NB. stack instructions
lb =: dput@bget@incp             NB. load byte (u8) to data stack
li =: dput@mget@inc4             NB. load i32
sw =: dswp                       NB. swap: xy->yx
du =: dput@dtos                  NB. dup: x->xx
ov =: dput@dnos                  NB. over: xy->xyx
zp =: dpop                       NB. zap: xy->x
dc =: cput@dpop                  NB. data stack -> call stack
cd =: dput@cpop                  NB. call stack -> data stack

NB. numeric instructions
(ad =: + dy)`(sb =: - dy)`(ml =: * dy)`(dv =: % dy)`(md =: |~dy)
sh =: SHL~ dy

NB. bitwise instructions (and or xor not)
(an =: (2b10001 b.) dy)` (or =: (2b10111 b.) dy)` (xr =: (2b10110 b.) dy)
nt =: NOT mo

NB. comparison instructions
(eq =: -@= dy) ` (lt =: -@< dy)



NB. register instructions
dr=: dpop rset AND@RMASK@dpop   NB. data stack -> register
rd=: dput@rget@AND@RMASK@dpop   NB. register -> data

NB. control flow instructions
NB. -1/<: is because every op is followed by p++
jm =: pset@<:@mget@inc4         NB. jump to M[P+1 2 3 4]-1
hp =: pset@<:@(pget+sget)@incp  NB. hop to P+M[P+1]-1
h0 =: hp`incp@.(0-:dpop)        NB. hop if tos==0 else skip addr
nx =: (hp@cput@<:)`incp@.(0-:])@ cpop  NB. 'next': rtos--?hop:pop
cl =: jm@cput@pget              NB. call: cput P then jump
rt =: pset&(cpop-1:)            NB. return


NB. memory / messaging instructions
ri =: dput@mget@dpop         NB. ( a-n) copy from addr y to stack (i32)
wi =: (dpop mset dpop)       NB. (na- ) write x to addr y
rb =: dput@bget@dpop         NB. ( a-n) copy from addr y to stack (u8)
wb =: (dpop bset dpop)       NB. (ba- ) write byte x to addr y

rx =: {{ (t+4) mset XREG [ dput mget t=. mget XREG }}
ry =: {{ (t+4) mset YREG [ dput mget t=. mget YREG }}
wz =: {{ (t+4) mset ZREG [ (dpop'') mset t=. mget ZREG }}

io =: [:                     NB. TODO: io

NB. register handler. call address in R with char on stack
NB. this does not have a specific opcode, but is invoked for
NB. every input sequence that designates a valid utf-8
NB. codepoint.
chev =: ev@rget@0@dput

NB. cpu emulator
NB. ---------------------------------------------------------------------
grps =: <:dfh&> cut'01 80 c0 c2 e0 f0 f5' NB. start of each group of ops
'gOk gCh gOp gU2 gU3 gU4 gUx' =: i.7

NB. eval: evaluate a single byte code. this allows for a
NB. 'calculator' mode, where the VM is passive and driven
NB. entirely by input from an external source.
eval =: {{
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
fmt =: ;:'P C D M'
log =: {{ (}:,<@,@(' ',.~hfd)@(a.&i.)@>@{:)":@". &.> fmt }}
sho =: fmt ,: log
s =: log@step

NB. launch a new vm with given memory image and trace what happens:
trace =: {{ (fmt,s 0}[:,/ log@step^:a:) 0 0$s=.log reset y }}
boot =: [: ". 'D' [[: (log@step^:a:) reset
