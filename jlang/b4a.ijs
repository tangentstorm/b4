load 'b4.ijs'
cocurrent 'b4'
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

NB. assert (128 1 131 136 {a.) -: asm 'si 01 du ad'

NB. '1 dup +' should result in D=.,2

assert (,2) -: boot asm 'si 01 du ad'
assert (,3) -: boot asm 'si 01 du du ad ad'
assert (,4) -: boot asm 'si 01 du ad du ad'

new''
