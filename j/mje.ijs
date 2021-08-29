NB. minimal j editor
NB.
NB. This file my attempt to port my presentation tool
NB. over to my new console-based libraries, token editor
NB. components, etc. It is something of a scratchpad and
NB. not well organized at the moment, but important enough
NB. that I should probably have it under version control.
(<'z') copath 'base'

copush =: {{ 18!:4 y [ BASE__y =: coname'' [ y=.<y }}
copop_z_ =: {{ y [ 18!:4 BASE [ y=.coname'' }}

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
cwfg=: ([: fgc cwc^:ischr) f.
cwbg=: ([: bgc cwc^:ischr) f.
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
  ihist =: iline =: ''
  init_world_''
  for_slide. slides do. i =. slide_index
    for_line. text i do. j =. line_index
      if. -. line = a: do.
        line =. > line
        if. ': ' {.@E. line do. line =. 2}.line     NB. : marks code line
          if. '. ' {.@E. line do. line =. 2}.line   NB. : . is editor macro
            NB. TODO : execute macro
          else.
            ihist =: ihist,<line
            iline =: line_index
            ehlen =. #ehist_world_     NB. length of the history
            exec_world_ line           NB. execute code in repl
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

NB. ted is the token editor in the 'on-camera' repl
ted =: 'TokEd' conew~ ''
H__ted =: 1
W__ted =: W_HIST-3

NB. except for now, i will just use a normal editor for editing the repl.
NB. this is just to avoid blocking video production until the token
NB. editor is working.
red =: 'UiEditWidget' conew~ ''
V__red =: 0
H__red =: 1
W__red =: W_HIST-3

NB. led is the line editor for editing the text
led =: 'UiEditWidget' conew~ ''
XY__led =: XY__cmds
W__led =: W__cmds
V__led =: 0

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
  cscr'' [ bg 16b101010 [ reset''
  if. -. a: -: cc do.
    for_line. >jlex cc do.
      goxy 0,line_index
      puts ' ' NB. little bit of whitespace on the left
      if. line ~: a: do.  (put_tok_TokEd_ :: ]) L:1 "1 > line end.
    end.
  end. R =: 0 }}

hist_lines =: {{
  w =. world pick~ index I. C__list, C__cmds
  NB. H_HIST-1 to leave one line at bottom for next repl input
  (-H_HIST-1) {. ('HISTL1_',w,'_')~ {. ehist_world_ }}

draw_hist =: {{
  NB. the repl output includes vt escape codes because of j-kvm/vm,
  NB. and I do not yet have a parser for these.
  for_line. hist_lines'' do.
    goxy X_HIST, line_index
    puts RESET,(>line),RESET,CEOL
  end. }}

render__hist =: {{
  red =. red_base_ [ ted =. ted_base_
  cmds =. cmds_base_
  NB. draw token editor on the last line
  XY__red =: XY__ted =: 3 0 + X_HIST_base_, #hist_lines_base_'' NB. 3-space prompt
  if. ':' {.@E. val =. >val__cmds'' do. B__ted =: jcut_jlex_ 2}.val
  else. B__ted =: a: end. }}

app =: (editor,list,cmds,hist,ted,red,led) conew 'UiApp'

draw_app =: {{
  render__app''
  draw_hist'' }}


NB. keyboard control
goto =: {{
 R__editor =: 1
 L__cmds =: text cur =: y
 S__cmds =: C__cmds =: 0 }}

put_text =: {{ 0 0 $ slides =: (<L__cmds) (<cur,1) } slides  }}

COPATH =: copath coname''
keymode =: {{ (~. (}: y;copath y),COPATH__BASE) copath BASE }}


(copush [ coinsert) 'outkeys'
NB. -----------------------------------------------------------
coinsert BASE
k_any =: {{
  select. 0{y
  case.'9'do. goto bak__list''
  case.'0'do. goto fwd__list''
  case.'('do. bak__cmds''
  case.')'do. fwd__cmds''
  end. }}

kc_l =: smudge__app
kc_s =: {{ (org_text'') fwrites org_path }}
kc_c =: {{ curs@1 reset@'' break_kvm_=: 1 }}
kc_o =: {{ L__list =: heads }}@open

edline =: {{
  R__led =: V__led =: 1 [ XY__led =: XY__cmds + 0,C__cmds
  C__led =: 0 [ B__led =: '',>val__cmds__BASE''
  ed_edkeys_ =: led
  keymode__BASE 'edkeys'}}

edrepl =: {{
  V__red =: R__red =: 1 NB.[ MODE__ted =: 'i'
  C__red =: 0 [ B__red =: 2}.>val__cmds__BASE''
  keymode__BASE 'replkeys' }}

insline =: edline@'' put_text@'' @ ins__cmds@''
k_O =: insline
k_o =: insline@fwd__cmds
k_e =: edline
k_E =: edrepl
k_d =: put_text@'' @ del__cmds

k_k =: k_p =: {{
  if. (at0__cmds > at0__list)'' do.
    goto bak__list''
    goz__cmds''
  else. bak__cmds'' end. }}

k_j =: k_n =: {{
  if. atz__cmds'' do. goto fwd__list''
  else. fwd__cmds'' end. }}

k_N =: {{
  if. -. a: = cmd =. val__cmds'' do.
    cmd =. >cmd
    if. ':' = 0{cmd do.  do_tok_>'n?','?!',~}.cmd end.
  end.
  k_n'' }}

copop''

copush 'edkeys'
NB. -----------------------------------------------------------
led =: led__BASE [ cmds =: cmds__BASE

kc_m =: stop__led =: {{
  keymode__BASE 'outkeys'
  V__led =: 0 [ R__led =: R__cmds =: 1
  L__cmds =: (<B__led) C__cmds } L__cmds
  put_text'' }}

k_asc =: {{ R__led =: 1 [ ins__led y }}
kc_d =: del__led
kc_h =: k_bksp =: bsp__led
kc_e =: eol__led
kc_b =: bak__led
kc_f =: fwd__led
kc_t =: swp__led

copop''



copush'replkeys'
NB. -----------------------------------------------------------
red =: red__BASE

kc_m =: {{
  keymode__BASE 'outkeys'
  V__red =: 0 [ R__red =: R__hist =: 1
  NB. TODO:
}}

k_asc =: {{ R__red =: 1 [ ins__red y }}
kc_d =: del__red
kc_h =: k_bksp =: bsp__red
kc_e =: eol__red
kc_b =: bak__red
kc_f =: fwd__red
kc_t =: swp__red

copop''



NB. event loop
mje =: {{
  9!:29]0  NB. disable infinite loop on error
  NB. clear the display
  cscr @ bg 24 [ curs 0
  draw_app goto 0
  draw_app loop_kvm_'base'
  reset''
  0$0}}

9!:27 'mje'''''
9!:29]1

rl =: {{ load'mje.ijs' }}
