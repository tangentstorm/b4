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
addr =: dfh NB. TODO err handling
step =: step__vm
isHEX =: *./@(e.&'0123456789ABCDEF') NB. upper case hex num?
puthex =: {{ if. isHEX y do. 1 [ dput dfh y return. end. 0 }}
hexstr =: {{ if. y<0 do. '$-',hfd -y else. '$',hfd y end. }}
stackstr =: ' ' joinstring toupper @ hexstr &.>
decode =: {{
  if. (#ops__vm) > ix=.(<y) i.~ ops__vm do. OPCODE + ix
  else. dfh y
  end. }}
putmem =: {{ NB. decode str with '!addr bytes' and store in ram
  'addr bytes' =. ({.;}.) decode &> ' 'splitstring y
  M__vm =: (bytes{a.) (addr+i.#bytes) } M__vm }}
encode =: {{ NB. encode bytes as b4 assembly language
  if. y = 0 do. '..'
  elseif. (<:&16b01 *. <:&16b1f) y do. '^', chr(y+ord'@')
  elseif. (<:&OPCODE *. <:&OPLAST) y do. >(y-OPCODE){ops__vm
  else. toupper hfd y end. }}
memstr =: {{ , ' ',.~ encode"0 a.i. (y+i.16) { M__vm  }}
showmem =: echo@memstr

main =: {{
  P__vm =: 16b100
  while. 1 do.
    line=:readln''
    if. '!'=0{line do. putmem }.line
    else. for_tok. ' 'splitstring line do.
      select. tok=.>tok
      case. ''   do.
      case. '%q' do. exit''
      case. '%s' do. step''
      case. '?d' do. echo 'ds: [', (stackstr D__vm), ']'
      case. '?c' do. echo 'cs: [', (stackstr C__vm), ']'
      case. '?i' do. echo 'ip: $', hfd P__vm
      case. '%j' do.
        echo 'Exiting into J. Type "main _" to return' return.
      case. do.
        if. isop tok do. runop tok continue. end.
        select. 0{tok
        case. ''''do. NB. quote ascii char to push it onto stack
          if. 1=#tok do. dput 32 NB. space
          else. dput ord 1{tok end.
        case. '`' do. dput rega 1{tok
        case. '@' do. showmem addr }.tok
        case. do.
          if. puthex tok do.
          else. exit [ echo 'unrecognized token "',tok,'"'
          end.
        end.
      end.
    end. end.
  end.}}

doNext 'main _'
