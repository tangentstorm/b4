NB. ----------------------------------------------
NB. a simple forth-like system
NB. ----------------------------------------------

NB. registers and hardware
'A B C P T N R' =: 7 $ 0
ram =: <0 [ data =: '' [ addr =: '' [ ctrl =: '' [ ports =: 32 $ 0
mem =: 3 : '<:#ram=:ram,<y'           NB. append to ram
get =: 1 : ' u { ram'                 NB. addr get -> val
put =: 1 : 'y [ ram =: (<y) u } ram'  NB. addr put val

NB. the dictionary, with kinds, xt and namespace entries.
dict =: '' [ wkind =: '' [ wxt =: '' [ wns =: '' [ cns =: 0

find =: 3 :'wxt {~ dict i. sym y'   NB. find 'word' -> XT
sym =: [: s: ' ' , ]                NB. sym 'word' -> `word
var =: adverb : '1 u y'             NB. new var 'name'
new =: 3 : 0                        NB. (kind) new 'word' -> XT
  0 new y
:
  dict =: dict, sym y
  wns =: wns, cns [ wkind =: x [ wxt =: wxt , r =. # ram
  r return.
)

