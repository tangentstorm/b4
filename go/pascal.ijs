#!/usr/bin/env j
NB. pascal compiler/generator for j
require 'task'
type =: 3!:0
'tnil tbit tstr tint tnum tcpx'=: 0 1 2 4 8 16
'tbox tbig trat tsym tuni'=: 32 64 128 65536 131072
'tname' =: _1
rule =: noun

NB. ----------------------------------------------
NB. patterns to describe pascal syntax
NB. ----------------------------------------------
program =: rule : 0
  '{$mode delphi}{$i xpc.inc}'
  'program ' $0 ';'
  'uses ' $1 ';'
    $2
  'begin'
    $3
  'end.'
)

procedure =: rule : 0
  'procedure ' $0 $1 ';'
     $2
  'begin'
     $3
  'end;'
)

tokT =: monad : 0
  NB. ----------------------------------------------
  NB. return the type of a j token (as lexed by ;:)
  NB. ----------------------------------------------
  h =: {. y
  if. h e. '0123456789_' do. tint
  elseif. h e. 'abcdefghijklmnopqrstuvwxyz' do. tname
  elseif. 1 do.
    select. a.i.h
      case. 39 do. tstr   NB. quote
      case. 10 do. tnil   NB. linefeed
    end.
  end.
)


gen =: dyad : 0
  NB. ----------------------------------------------
  NB. generate text from template x with data from y
  NB. ----------------------------------------------
  r =. ''              NB. ]] echo ;: x return.
  tokens =: ;: x       NB. ]] echo (lexT > tok);>tok=.>token
  i =. 0               NB. ]] for_yi. y do. echo yi_index; >yi end. return.
  while. i < # tokens do.
    tok =. > i { tokens
    select. tokT tok
      case. tnil do. r=.r, LF
      case. tstr do. r=.r, }.}: tok
      case. tint do.
         if. (# y) > ".tok   do. r=.r, > (".tok) { y
         else. echo 'no argument ', tok, ' given!' throw.  end.
    end.
    i =. >: i
  end.
  r return.
)


read =: monad : 0
  NB. ----------------------------------------------
  NB. read y noun defs from the script and box them.
  NB. ----------------------------------------------
  r=.'' for. i.y do. r=.r ;~ (0 : 0) end. }.|. r return.
)


NB. -----------------------------------
NB. construct a simple pascal program
NB. -----------------------------------
say =: procedure gen 'say'; '(msg:string)'; ''; read 1
  ccenterxy(
    kvm.xmax div 2, kvm.ymax div 2 - 5,
    '|b-|B=|K[ |W' + msg + ' |K]|B=|b-');
  gotoxy(0,kvm.ymax-8);
)

hello =: program gen 'helloj'; 'kvm,cw'; say; read 1
  clrscr; say('hello world');
)


hello fwrite 'gen/helloj.pas'
shell 'fpc gen/helloj.pas'
echo  'generated and compiled gen/helloj'
exit''
