NB. minimal j editor
NB.
NB. This file my attempt to port my presentation tool
NB. over to my new console-based libraries, token editor
NB. components, etc. It is something of a scratchpad and
NB. not well organized at the moment, but important enough
NB. that I should probably have it under version control.

dbg 1

NB. main code
NB.cocurrent'mje'
coinsert 'kvm' [ load 'tangentstorm/j-kvm'
load 'tok.ijs data/sqlite'
load'tangentstorm/j-kvm/ui'
load'code-edit.ijs'
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
          else.
            ehlen =. #ehist_world_
            exec_world_ line              NB. execute code in repl
            ehist_world_ =: (<'   ',vtcolor_tok_ line) ehlen } ehist_world_
          end.
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



NB. --  screen setup --------------------------------------------------------

H_HIST =: 24
X_HIST =: 72
W_HIST =: X_HIST -~ xmax''
Y_META =: H_HIST+2

NB. indent headings based on depth
list =: 'UiList' conew~ heads
H__list =: (ymax'')-Y_META
XY__list =: 0,Y_META

cmds =: 'UiList' conew~ a:
W__cmds =: (xmax'')-32
H__cmds =: 32
XY__cmds =: 33 0 + XY__list

ted =: 'TokEd' conew~ ''
H__ted =: 1
W__ted =: W_HIST-3

hist =: 'UiWidget' conew~ ''
H__hist =: H_HIST
W__hist =: W_HIST
XY__hist=: X_HIST,0

editor =: 'UiWidget' conew~ ''
XY__editor =: 0 0
H__editor =: H_HIST
W__editor =: X_HIST-2


render__editor =: {{
  cc =. code_base_ cur_base_
  NB. draw the code editor
  cscr'' [ bgx 16b101010 [ reset''
  if. -. a: -: cc do.
    for_line. >jlex cc do.
      goxy 0,line_index
      puts ' ' NB. little bit of whitespace on the left
      if. line ~: a: do.  (put_tok_TokEd_ :: ]) L:1 "1 > line end.
    end.
  end. }}

hist_lines =: {{
  w =. world pick~ index I. C__list, C__cmds
  NB. H_HIST-1 to leave one line at bottom for next repl input
  (-H_HIST-1) {. ('HISTL1_',w,'_')~ {. ehist_world_ }}

draw_hist =: {{
  NB. the repl output includes vt escape codes because of j-kvm/vm,
  NB. and I do not yet have a parser for these.
  for_line. hist_lines'' do.
    goxy X_HIST, line_index
    puts (RESET,CEOL), > line
  end. }}

render__hist =: {{
  ted =. ted_base_
  cmds =. cmds_base_
  NB. draw token editor on the last line
  XY__ted =: 3 0 + X_HIST_base_, #hist_lines_base_'' NB. 3-space prompt
  if. ':' {.@E. val =. >val__cmds'' do. B__ted =: jcut_jlex_ 2}.val
  else. B__ted =: a: end. }}

app =: (editor,list,cmds,hist,ted) conew 'UiApp'

draw_app =: {{
  render__app''
  draw_hist'' }}


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

kc_l =: draw_app@smudge__app

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
  draw_app goto 0
  [ loop_kvm_'base'
  reset''
  0$0}}

9!:27 'mje'''''
9!:29]1

rl =: {{ load'mje.ijs' }}

k_p =: {{
  if. (at0__cmds > at0__list)'' do.
    goto bak__list''
    goz__cmds''
    draw_app''
  else. draw_hist@'' render__app bak__cmds'' end. }}

k_n =: {{
  if. atz__cmds'' do. draw_app goto fwd__list''
  else. draw_hist@'' render__app fwd__cmds'' end. }}

k_N =: {{
  if. -. a: = cmd =. val__cmds'' do.
    cmd =. >cmd
    if. ':' = 0{cmd do.  do_tok_>'n?','?!',~}.cmd end.
  end.
  k_n'' }}

