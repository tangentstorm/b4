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
load'tangentstorm/j-kvm/ui'
load'worlds.ijs'

NB. todo: move this to kvm
cocurrent 'kvm'
NB. 'colorwrite' stuff
cwc=. [: [:^:(16=]) 'krgybmcwKRGYBMCW' i. ]
ischr=. 2 = 3!:0
fg=: ([: fgc cwc^:ischr) f.
bg=: ([: bgc cwc^:ischr) f.
db =: sqlopen_psqlite_'~/b4/sql/syndir.sdb'
cocurrent'base'

NB. org-mode stuff
load 'tangentstorm/j-lex'
load '~/ver/j-talks/preztool/org.ijs'

NB. org_slides defines locale variables: title=:.. and slides=:..
open =: {{
  emit_vm_ =: ]
  org_slides org_path=: 'e3/sandpiles-j.org'
  heads =: <@;"1((' '#~+:@<:) each 3 {"1 slides),.(0{"1 slides)
  index =: 0 0 $ 0
  world =: ,<'WORLD0'
  init_world_''
  for_slide. slides do. i =. slide_index
    for_line. text i do. j =. line_index
      if. -. line = a: do.
        line =. > line
        if. ': ' {.@E. line do. line =. 2}.line     NB. : marks code line
          if. '. ' {.@E. line do. line =. 2}.line   NB. : . is editor macro
            NB. TODO : execute macro
          else. exec_world_ line end.               NB. execute code in repl
        end.
        index =: index, i,j
        world =: world,<this_world_''
      end.
    end.
  end.
  NB. emit_vm_ =: emit0_vm_
  0 0$0}}


open''
echo<'done loading j-talk. press enter.'
rkey''
cur =: 8 NB. jump to slide

NB. need to handle empty tokens (blanks lines??)
put_tok=:put_tok_tok_ :: ]


NB. --  screen setup --------------------------------------------------------

H_HIST =: 24
X_HIST =: 72
Y_META =: H_HIST+2

NB. indent headings based on depth
list =: 'uiList' conew~ heads
H__list =: (ymax'')-Y_META
XY__list =: 0,Y_META

cmds =: 'uiList' conew~ a:
W__cmds =: (xmax'')-32
H__cmds =: 32
XY__cmds =: 33 0 + XY__list

repl =: 'uiList' conew~ a:
W__repl =: W__cmds
H__repl =: (ymax'')-H__cmds+H__list
XY__repl =: XY__cmds + 0,H__cmds+2

ced =: ced_tok_
ted =: ted_tok_


draw_slide =: {{
  NB. draw the current slide
  goxy 0 0 [ bg'y' [ fg'k'
  puts ' ',71$!.' ' head cur
  puts CRLF [ reset''
  if. -. a: -: code cur do.
    for_line. >jlex code cur do.
      puts ' '
      if. line ~: a: do.  put_tok L:1 "1 > line end.
      puts CRLF [ ceol''
    end.
  end.
  for. i.16-#code cur do. puts CRLF [ ceol'' end.
  puts CRLF [ reset'' }}

draw_hist =: {{
  w =. world pick~ index I. C__list, C__cmds
  NB. H_HIST-1 to leave one line at bottom for next repl input
  lines =. (-H_HIST-1) {. ('HISTL1_',w,'_')~ {. ehist_world_
  for_line. lines do.
    goxy X_HIST, line_index
    puts (RESET,CEOL),~ 7 u: > line
  end.
  NB. draw last line
  goxy xy =. X_HIST, #lines
  puts (RESET,CEOL)
  if. ':' {.@E. val =. >val__cmds'' do.
    XY__ted =: 3 0 + xy  NB. 3-space prompt
    B__ted =: jcut_jlex_ 2}.val
    draw_toked_tok_''
  end. }}


draw_app =: {{
  draw_slide''
  render__list''
  render__cmds''
  draw_hist''
  render__repl''  }}


NB. keyboard control
goto =: {{
 L__cmds =: text cur =: y
 S__cmds =: C__cmds =: 0 }}

k_any =: {{
  select. 0{y
  case.'9'do. draw_app goto bak__list''
  case.'0'do. draw_app goto fwd__list''
  case.'('do. draw_app bak__cmds''
  case.')'do. draw_app fwd__cmds''
  end. }}

kc_l =: draw_app@cscr@''bg@24@curs@0

kc_p =: puts@CURSU
kc_n =: puts@CURSD
kc_b =: puts@CURSL
kc_f =: puts@CURSR
kc_a =: putc@CR

NB. kc_d =: {{ break_kvm_=:1 }}
kc_h =: {{ puts '[^H:BS]' }}
kc_d =: {{ puts '[^D:del]' }}
kc_a =: {{ puts '^A' }}
kc_o =: draw_app@{{ L__list =: heads }}@open
kc_s =: {{ (org_text'') fwrites org_path }}

kc_t =: {{ reset'' NB. ^T -> random colored text
  for. i.500 do. putc a.{~32+?94 [ goxy 20 2 + ? 30 10 [ fg ? 256 end.
  bg@24 fg'w' }}

kc_c =: {{ curs@1 reset@'' break_kvm_=: 1 }}




NB. event loop
mje =: {{
  9!:29]0  NB. disable infinite loop on error
  NB. clear the display
  cscr @ bg 24 [ curs 0
  draw_app''
  [ loop_kvm_'base'
  reset''
  0$0}}

9!:27 'mje'''''
9!:29]1

rl =: {{ load'mje.ijs' }}

k_p =: {{
  if. at0__cmds'' do. draw_app goto bak__list''
  else. draw_hist@'' render__cmds bak__cmds'' end. }}

k_n =: {{
  if. atz__cmds'' do. draw_app goto fwd__list''
  else. draw_hist@'' render__cmds fwd__cmds'' end. }}

k_N =: {{
  if. -. a: = cmd =. val__cmds'' do.
    cmd =. >cmd
    if. ':' = 0{cmd do.  do_tok_>'n?','?!',~}.cmd end.
  end.
  k_n'' }}

