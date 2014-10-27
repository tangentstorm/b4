NB. B4 virtual machine in J
NB. --------------------------------------------------------------------
NB. new'' resets the machine (r=register es=emptystack s=string)
new =:            'r;r;r;r;r;r;es;es;s;s;s;(32$0)[r=.0[s=.''''[es=.0$0'
new =: 13 :('>a:[''P T S R A B Ds Rs I O E M''=:',new,'[DBG=:0$a.')
ddrop=: 3 :('>{.''res T S Ds''=:(T;S;{.;}.)Ds')
dpush=: 3 :('''T S Ds''=:y;T;S,Ds')
NB. ops consume 1..2 cells from ram and always increments p.
cell=: 2 : 'y] v (oldp+1+i.argc){M[ argc=:m-1[ P=:m+oldp=:{.P'
syms=: chs =: ''
is =:2 :('m=.;:m';'syms=:syms,0{m'; 'chs=:chs,>1{m'; '".(>0{m),''=:v''')
sym=:3 :('if. P<#M do. r=.(P{M){syms else. r=.<''n/a'' end.'; ',:''op'';r')
sho=:3 :'({.,(sym _),}.)&.|:(;:''P T S Ds Rs I O E M''),:P;T;S;Ds;Rs;I;O;E;M'
'nop .' is (1 cell ])
'dbg %' is (1 cell sho)
'lit n' is (2 cell dpush)
'jmp j' is (2 cell (3 :'P=:{.y'))
'thn t' is (2 cell (3 :'if. T do. P=:{.y end.'))
'els e' is (2 cell (3 :'if. T=0 do. ddrop P=:y end.'))
'del d' is (1 cell ddrop)
'rch r' is (1 cell (3 :'I=:}.I [ dpush a.i.{.I'))
'wch w' is (1 cell (3 :'O=:O,a.{~ddrop _'))
'swp s' is (1 cell (3 :'dpush"0 (ddrop, ddrop) _'))
'add +' is (1 cell (3 :'dpush (ddrop + ddrop) _'))
'not ~' is (1 cell (3 :'dpush -.ddrop _'))
'and &' is (1 cell (3 :'dpush *./-.0=(ddrop,ddrop) _'))
'or  |' is (1 cell (3 :'dpush +./-.0=(ddrop,ddrop) _'))
'lt  <' is (1 cell (3 :'dpush (ddrop < ddrop) _'))
'gt  >' is (1 cell (3 :'dpush (ddrop > ddrop) _'))
'dup :' is (1 cell (3 :'(dpush,dpush) ddrop _'))
'eof $' is (1 cell (3 :'dpush 0=#I'))
NB. -- assembler --
isch =: 2=(3!:0)
c2op =: 3 : 0
  r=:0$0
  for_c.y do.
    if.c e.chs do.r=.r,chs i.c else. E=:c,'?' throw. end.
  end.
)
b4a =: 3 :('new _';'try. ; c2op^:isch L:0 y catcht. 0$0 return. end.')
NB. -- interpreter --
b0=:3 :('new _';':';'''M I''=:x;y')
b1=:3 :'if. P<#M do. syms@.(P{M) _ end.'
b2=:3 :('while. P<#M do. b1 _ end.';'O')
b4=:4 :('x b0 y';'if. -.#E do. b2 _ else. E end.')

NB. -- tests --
assert 'b?'-:".'E'[ b4a 'b is not a b4a token so error E should say "b?"'
NB. echo sho [ (b4a '$t';10;'rn';32;'%+wj';0) b0 'HELLO WORLD'
