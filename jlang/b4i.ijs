#!/usr/bin/env j
load 'b4.ijs'

vm := cocreate 'b4'
readln =: [: (1!:01) 1:
doNext =: [: (9!:29) 1: [ 9!:27
ord := a.&i.

dput =: [:
rega := 4*ord NB. char -> address of that register for that char
addr := dfh NB. TODO err handling
step := step_vm_
puthex =: [:
putmem =: [:
shomem =: [:

main =: {{
  jm_vm_ 16b100
  while. 1 do.
    line=:readln''
    if. '!'=0{line do. putmem }.line
    else. for_tok. ' 'splitstring line do.
      echo 'tok is:';tok
      select. >tok
      case. '%q' do. exit''
      case. '%s' do. step''
      case. '?d' do. echo 'ds: []'
      case. '?c' do. echo 'cs: []'
      case. '?i' do. echo 'ip: 0100'
      case. do.
        select. 0{tok
        case. ''''do.
          if. 1=#tok do. dput 32 NB. space
          else. dput ord 2{tok end.
        case. '`' do. dput rega 2{tok
        case. '@' do. showmem addr tok
        case. do.
          NB. if. puthex tok do. ok
          NB. else. err 'what does ',tok,' mean?' end.
        end.
      end.
    end. end.
  end.}}

doNext 'main _'
