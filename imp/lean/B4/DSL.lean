import B4.Basic
import Lean

namespace B4

declare_syntax_cat b4_op
declare_syntax_cat b4_reg

syntax "PC" : b4_reg
syntax "DS" : b4_reg
syntax "CS" : b4_reg
syntax "ST" : b4_reg
syntax "DB" : b4_reg
syntax "RED" : b4_reg
syntax "BLU" : b4_reg
syntax "GRN" : b4_reg
syntax "HERE" : b4_reg
syntax "T" : b4_reg
syntax "X" : b4_reg
syntax "Y" : b4_reg
syntax "Z" : b4_reg
syntax "R" num : b4_reg

syntax "ad" : b4_op
syntax "sb" : b4_op
syntax "ml" : b4_op
syntax "dv" : b4_op
syntax "md" : b4_op
syntax "sh" : b4_op
syntax "an" : b4_op
syntax "or" : b4_op
syntax "xr" : b4_op
syntax "nt" : b4_op
syntax "eq" : b4_op
syntax "lt" : b4_op
syntax "du" : b4_op
syntax "sw" : b4_op
syntax "ov" : b4_op
syntax "zp" : b4_op
syntax "dc" : b4_op
syntax "cd" : b4_op
syntax "rb" : b4_op
syntax "ri" : b4_op
syntax "wb" : b4_op
syntax "wi" : b4_op
syntax "lb" num : b4_op
syntax "li" num : b4_op
syntax "rs" : b4_op
syntax "ls" : b4_op
syntax "jm" num : b4_op
syntax "hp" "-"? num : b4_op
syntax "h0" "-"? num : b4_op
syntax "cl" num : b4_op
syntax "rt" : b4_op
syntax "nx" "-"? num : b4_op
syntax "c0" : b4_op
syntax "c1" : b4_op
syntax "c2" : b4_op
syntax "n1" : b4_op
syntax "c4" : b4_op
syntax "io" : b4_op
syntax "db" : b4_op
syntax "hl" : b4_op
syntax "^" b4_reg : b4_op
syntax "@" b4_reg : b4_op
syntax "!" b4_reg : b4_op
syntax "+" b4_reg : b4_op
syntax num : b4_op

syntax "b4_reg_term%" b4_reg : term
macro_rules
  | `(b4_reg_term% PC) => `(Register.PC)
  | `(b4_reg_term% DS) => `(Register.DS)
  | `(b4_reg_term% CS) => `(Register.CS)
  | `(b4_reg_term% ST) => `(Register.ST)
  | `(b4_reg_term% DB) => `(Register.DB)
  | `(b4_reg_term% RED) => `(Register.RED)
  | `(b4_reg_term% BLU) => `(Register.BLU)
  | `(b4_reg_term% GRN) => `(Register.GRN)
  | `(b4_reg_term% HERE) => `(Register.HERE)
  | `(b4_reg_term% T) => `(Register.T)
  | `(b4_reg_term% X) => `(Register.X)
  | `(b4_reg_term% Y) => `(Register.Y)
  | `(b4_reg_term% Z) => `(Register.Z)
  | `(b4_reg_term% R $n) => `(Register.R $n)

syntax "b4_op_term%" b4_op : term
macro_rules
  | `(b4_op_term% ad) => `(Op.ad)
  | `(b4_op_term% sb) => `(Op.sb)
  | `(b4_op_term% ml) => `(Op.ml)
  | `(b4_op_term% dv) => `(Op.dv)
  | `(b4_op_term% md) => `(Op.md)
  | `(b4_op_term% sh) => `(Op.sh)
  | `(b4_op_term% an) => `(Op.an)
  | `(b4_op_term% or) => `(Op.or)
  | `(b4_op_term% xr) => `(Op.xr)
  | `(b4_op_term% nt) => `(Op.nt)
  | `(b4_op_term% eq) => `(Op.eq)
  | `(b4_op_term% lt) => `(Op.lt)
  | `(b4_op_term% du) => `(Op.du)
  | `(b4_op_term% sw) => `(Op.sw)
  | `(b4_op_term% ov) => `(Op.ov)
  | `(b4_op_term% zp) => `(Op.zp)
  | `(b4_op_term% dc) => `(Op.dc)
  | `(b4_op_term% cd) => `(Op.cd)
  | `(b4_op_term% rb) => `(Op.rb)
  | `(b4_op_term% ri) => `(Op.ri)
  | `(b4_op_term% wb) => `(Op.wb)
  | `(b4_op_term% wi) => `(Op.wi)
  | `(b4_op_term% lb $v) => `(Op.lb $v)
  | `(b4_op_term% li $v) => `(Op.li $v)
  | `(b4_op_term% rs) => `(Op.rs)
  | `(b4_op_term% ls) => `(Op.ls)
  | `(b4_op_term% jm $v) => `(Op.jm $v)
  | `(b4_op_term% hp $v) => `(Op.hp $v)
  | `(b4_op_term% hp - $v) => `(Op.hp (- $v))
  | `(b4_op_term% h0 $v) => `(Op.h0 $v)
  | `(b4_op_term% h0 - $v) => `(Op.h0 (- $v))
  | `(b4_op_term% cl $v) => `(Op.cl $v)
  | `(b4_op_term% rt) => `(Op.rt)
  | `(b4_op_term% nx $v) => `(Op.nx $v)
  | `(b4_op_term% nx - $v) => `(Op.nx (- $v))
  | `(b4_op_term% c0) => `(Op.c0)
  | `(b4_op_term% c1) => `(Op.c1)
  | `(b4_op_term% c2) => `(Op.c2)
  | `(b4_op_term% n1) => `(Op.n1)
  | `(b4_op_term% c4) => `(Op.c4)
  | `(b4_op_term% io) => `(Op.io)
  | `(b4_op_term% db) => `(Op.db)
  | `(b4_op_term% hl) => `(Op.hl)
  | `(b4_op_term% ^ $r) => `(Op.invoke (b4_reg_term% $r))
  | `(b4_op_term% @ $r) => `(Op.read (b4_reg_term% $r))
  | `(b4_op_term% ! $r) => `(Op.write (b4_reg_term% $r))
  | `(b4_op_term% + $r) => `(Op.stream (b4_reg_term% $r))
  | `(b4_op_term% $n:num) => `(Op.li $n)

syntax "b4!" "{" b4_op* "}" : term

macro_rules
  | `(b4! { $[$ops]* }) => do
    let opsTerms ← ops.mapM fun op => `(b4_op_term% $op)
    `( ([$opsTerms,*] : List Op) )

end B4
