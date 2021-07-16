NB. simple line editor

coclass 'lined'

init =: {{
  B =: ''  NB. the string to edit.
  C =: 0   NB. cursor position(s)
  M =: 0   NB. mark(s) (one per cursor)
  W =: 64  NB. width/max length
}}

create =: {{ B=:y  }}

ins =: {{ B=:(C+&#B) {. y (<:C=:>:(+i.@#)C) } (1+C e.~ i.#b)#b=.B,' ' }}"0
bsp =: {{ C=:C-1+i.#C[B=:}:b#~-.1|.C e.~i.#b=.B,' ' }}
del =: {{ C=:C-i.#C[B=:}:b#~-.C e.~i.#b=.B,' '}}
sho =: {{ <b,:'-^' {~ C e.~i.#b=.B,' ' }}
eol =: {{ C=:C+(#B)->./C }}
bol =: {{ C=:C-<./C }}

k_asc =: {{ ins y }}
kc_d =: del
kc_h =: bsp
kc_e =: eol
kc_b =: {{ C=:<:C }}
kc_f =: {{ C=:>:C }}
ed =: {{
  puts cls,goxy 0 0
  raw 0
  echo sho''
  raw ''}}kvm

init ''

assert '!.!michal!.!' -: 'B'~ [ ins'!.!' [ B=:'michal' [ C=:0 6
sho B0=.B [ C0=.C

