#!/usr/bin/env j
load 'b4.ijs'

vm =: 'b4' conew~''
readln =: [: (1!:01) 1:
doNext =: [: (9!:29) 1: [ 9!:27
ord =: a.&i.
chr =: {&a.
runop =: ".@(,&'__vm _')
isop =: {{ ops__vm e.~ <y }}
OPCODE =: 16b80 NB. bytecode of first internal instruction
OPLAST =: OPCODE + # ops__vm
dput =: dput__vm
rega =: 4*(ord'@') -~ ord NB. backtick = register address
ihx =: {{ -^:('-'={.y)  dfh y }}  NB. int from hex
addr =: ihx NB. TODO err handling
step =: step__vm
isHEX =: [: *./@(e.&'0123456789ABCDEF') }.^:('-'={.) NB. upper case hex num?
puthex =: {{ if. isHEX y do. 1 [ dput ihx y return. end. 0 }}
hexstr =: toupper@{{ if. y<0 do. '-',hfd -y else. hfd y end. }}
stackstr =: ' ' joinstring toupper @ hexstr &.>
asm =: {{
  if. (#ops__vm) > ix=.(<y) i.~ ops__vm do. OPCODE + ix
  elseif. (2=#y) *.''''=0{y do.          ord 1{y
  elseif. (2=#y) *. '^'=0{y do.      rgn__vm 1{y
  elseif. (2=#y) *. '@'=0{y do. 32 + rgn__vm 1{y
  elseif. (2=#y) *. '!'=0{y do. 64 + rgn__vm 1{y
  elseif. (2=#y) *. '+'=0{y do. 96 + rgn__vm 1{y
  else. ihx y
  end. }}
HERE=:rgn__vm'_'
putmem =: {{ NB. decode str like 'addr bytes' and store in ram
  'addr ops' =. ({.;<@}.) ' 'splitstring y
  if. 1=#s=.>addr do.
    addr=.rget__vm HERE
    addr rset__vm r=.rgn__vm 0{s
    (addr+#ops) rset__vm HERE
  else. addr=.ihx>addr end.
  bytes =. asm &> ops
  M__vm =: (bytes{a.) (addr+i.#bytes) } M__vm }}
dis =: {{ NB. disassemble bytes to b4 assembly language
  if. y = 0 do. '..'
  elseif. (>:&16b01 *. <:&16b1f) y do. '^', chr     y+ord'@'
  elseif. (>:&16b20 *. <:&16b3f) y do. '@', chr (y-32)+ord'@'
  elseif. (>:&16b40 *. <:&16b5f) y do. '!', chr (y-64)+ord'@'
  elseif. (>:&16b60 *. <:&16b7f) y do. '+', chr (y-96)+ord'@'
  elseif. (>:&OPCODE *. <:&OPLAST) y do. >(y-OPCODE){ops__vm
  else. hexstr y end. }}
memstr =: {{ , ' ',.~ dis"0 a.i. (y+i.16) { M__vm  }}
showmem =: echo@memstr

main =: {{
  (rgn__vm'_') rset__vm~ P__vm =: 16b100 [ done=:0
  while. 1 do.
    line=:readln''
    if. ''-:line do.
    elseif.':'=0{line do. putmem }.line
    else. for_tok. ' 'splitstring line do.
      select. tok=.>tok
      case. ''   do.
      case. '%q' do. done=:1
      case. '%s' do. step''
      case. '%C' do. create__vm'' NB. clear everything
      case. '?d' do. echo 'ds: [', (stackstr D__vm), ']'
      case. '?c' do. echo 'cs: [', (stackstr C__vm), ']'
      case. '?i' do. echo 'ip: ', hexstr P__vm
      case. '%j' do.
        echo 'Exiting into J. Type "main _" to return' return.
      case. do.
        if. isop tok do. runop tok continue. end.
        select. 0{tok
        case. '^' do. imr__vm rgn__vm 1{tok
        case. '!' do. wr__vm rgn__vm 1{tok
        case. '@' do. rr__vm rgn__vm 1{tok
        case. '+' do. ir__vm rgn__vm 1{tok
        case. ''''do. NB. quote ascii char to push it onto stack
          if. 1=#tok do. dput 32 NB. space
          else. dput ord 1{tok end.
        case. '`' do. dput rega 1{tok
        case. '?' do.
          if. 2=#tok do. echo _8 {. (8$'0'),hfd rget__vm rgn__vm 1{tok
          else. showmem addr }.tok end.
        case. do.
          if. puthex tok do.
          else. exit [ echo 'unrecognized token "',tok,'"' end.
        end. NB. select 0{tok
      end. NB. select tok
    end.end. NB. else/for_tok.
    if. #O__vm do. O__vm=:'' [ echo O__vm end.
    if. done do. exit'' end.
  end.}}

doNext 'main _'
