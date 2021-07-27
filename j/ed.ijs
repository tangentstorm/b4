NB. simple line editor
load'tangentstorm/j-kvm'
coinsert 'kvm' [ coclass 'ed'

XY=: 0 0 NB. screen coordinates

init =: {{
  B =: ''  NB. the string to edit.
  C =: 0   NB. cursor position(s)
  M =: 0   NB. mark(s) (one per cursor)
  W =: 64  NB. width/max length
  E =: ' ' NB. empty element to put at the end
}}

create =: {{ B=:y [ init'' if. 32=3!:0 y do. E =: a: end. }}

NB. buffer editing commands:
ins =: {{ B=:(C+&#B) {. y (<:C=:>:(+i.@#)C) } (1+C e.~ i.#b)#b=.B,E }}"0
bsp =: {{ C=:C-1+i.#C[B=:}:b#~-.1|.C e.~i.#b=.B,E }}
del =: {{ C=:C-i.#C[B=:}:b#~-.C e.~i.#b=.B,E}}
eol =: {{ C=:C+(#B)->./C }}
bol =: {{ C=:C-<./C }}
swp =: {{ B=: a (C-1) } b (C-2) } B [ a=. (C-2) { B [ b=. (C-1) { B }}

NB. keybindings:
kc_m =: {{ break_kvm_=: 1}}
k_asc =: {{ ins y }}
kc_d =: del
kc_h =: k_bksp =: bsp
kc_e =: eol
kc_b =: {{ C=:<:C }}
kc_f =: {{ C=:>:C }}
kc_t =: swp

lined =: {{
  goxy XY [ curs 0
  bgc 8 [ fgc 15
  ceol@puts B
  bgc 1
  ({{ goxy xy [ putc y{B,' '[ goxy xy=.XY+y,0 }} :: ])"0 C
  reset''
}} loop@'ed' @init@''

init ''

assert '!.!michal!.!' -: 'B'~ [ ins'!.!' [ B=:'michal' [ C=:0 6

sho =. {{ b,:'-^' {~ C e.~i.#b=.B,E }}
sho B0=.B [ C0=.C

ed_z_=:lined_ed_
