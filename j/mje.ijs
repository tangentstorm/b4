NB. minimal j editor
NB.
NB. This file my attempt to port my presentation tool
NB. over to my new console-based libraries, token editor
NB. components, etc. It is something of a scratchpad and
NB. not well organized at the moment, but important enough
NB. that I should probably have it under version control.

NB. main code
NB.cocurrent'mje'
coinsert 'kvm' [ load 'tangentstorm/j-kvm'
load 'ed.ijs tok.ijs data/sqlite'

NB. 'colorwrite' stuff
cwc=. [: [:^:(16=]) 'krgybmcwKRGYBMCW' i. ]
ischr=. 2 = 3!:0
fg=: ([: fgc cwc^:ischr) f.
bg=: ([: bgc cwc^:ischr) f.
db =: sqlopen_psqlite_'~/b4/sql/syndir.sdb'



NB. org-mode stuff
load 'tangentstorm/j-lex'
load '~/ver/j-talks/preztool/org.ijs'
slides =: org_slides '~/ver/j-talks/s1/e3-sandpiles-in-j/sandpiles-j.org'
cur =: 8 NB. jump to slide

put_tok =: {{
  if. 2~:#y do. 0$0 return. end.
  'code text' =. y
  select. code
  case. ''  do.        NB. ??
  case. 'S' do.        NB. space
  case. 'A' do. fg'B'  NB. assignment
  case. 'i' do. fg'w'  NB. identifier
  case. 'v' do. fg'r'  NB. verb
  case. 'a' do. fg'y'  NB. adverb
  case. 'D' do. fg'R'  NB. def braces
  case. 'c' do. fg'r'  NB. conjunction
  case. 'P' do. fg'w'  NB. parens
  case. 'p' do. fg'Y'  NB. proname
  case. 'N' do. fg'M'  NB. numeric
  case. 'L' do. fg'c'  NB. literal
  case. 'C' do. fg'K'  NB. comment
  case. 'K' do. fg'B'  NB. control word
  case. do. reset [ puts '[',code,']' [ bg'b'
  end.
  puts text }}

draw =: {{
  NB. draw the current slide
  cscr@reset''
  goxy 0 0 [ bg'y' [ fg'k'
  puts ' ',71$!.' ' head cur
  puts CR,LF [ reset''
  for_line. >jlex code cur do.
    puts ' '
    if. line ~: a: do.  put_tok"1 > line end.
    puts CR,LF
  end.
  for. i.16-#code cur do. puts CR,LF end.
  NB. limit text to 5 lines
  for_line. > ({.~5&<.@#) text cur do. puts line,CR,LF [ fg'K' end.
  puts CR,LF[ reset'' }}



NB. keyboard control
k_any =: {{
  puts '[',y,']' [ goxy 20 5
  select. 0{y
  case.'9'do. draw bak''
  case.'0'do. draw fwd''
  end.
}}
kc_l =: draw

NB. emacs keys -> cursor keys
csi =: csi_vt_

kc_p =: puts@CURSU
kc_n =: puts@CURSD
kc_b =: puts@CURSL
kc_f =: puts@CURSR
kc_a =: putc@CR

NB. kc_d =: {{ break_kvm_=:1 }}
kc_h =: {{ puts '[^H:BS]' }}
kc_d =: {{ puts '[^D:del]' }}
kc_a =: {{ puts '^A' }}

kc_t =: {{ reset'' NB. ^T -> random colored text
  for. i.500 do. putc a.{~32+?94 [ goxy 20 2 + ? 30 10 [ fg ? 256 end.
  bg@24 fg'w' }}

kc_c =: {{ break_kvm_=: 1 }}




NB. event loop
mje =: {{
  9!:29]0  NB. disable infinite loop on error
  NB. clear the display
  cscr @ bg 24
  puts 'mje' [ goxy 2 2
  [ loop_kvm_'base'
  reset''
  0$0}}

9!:27 'mje'''''
9!:29]1

rl =: {{ load'mje.ijs' }}

go =: {{
curs 0 0 [ cscr''
'cx cy'=.curxy''
if. cy < 8 do. echo^:(8-cy) '' end.
C__ted =: 0 [ B__ted =: 0$a:
C__ced =: 0 [ B__ced =: ''
do'    n?0 0 4 0 0?        !'
do' b v?,:? b n?0 0 4 0 0? !'
do' b v?,?  b n?4 4 4 4 4? !'
do' b v?,?  b n?0 0 4 0 0? !'
do' b v?,?  b n?0 0 4 0 0? !'
do' bb i?y? c?=.?          !'
raw 0 [ goxy 0 8 [ curs 1 }}
go''
NB. ej : emit j. (string -> commands)
NB. ej =: sleep@(25+[:?25[])@do@('?',,&'?') each @jcut
NB. ej '{{ (+33*])/|.0, 5381,a.i. y}}'

