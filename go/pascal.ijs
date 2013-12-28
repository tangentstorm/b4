#!/usr/bin/env j
NB. pascal compiler/generator for j
require 'task'
type =: 3!:0
'tnil tbit tstr tint tnum tcpx'=: 0 1 2 4 8 16
'tbox tbig trat tsym tuni'=: 32 64 128 65536 131072
'tname' =: _1

NB. ----------------------------------------------
NB. patterns to describe pascal syntax
NB. ----------------------------------------------
program =: 0 : 0
 '{$mode delphi}{$i xpc.inc}'
  'program ' 0 ';'
  'uses ' 1 ';'
  2
  'begin'
  3
  'end.'
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
  for_token. ;: x do.  NB. ]] for_yi. y do. echo yi_index; >yi end. return.
    tok =. > token     NB. ]] echo (lexT > tok);>tok=.>token
    select. tokT tok
      case. tnil do. r=.r, LF
      case. tstr do. r=.r, }.}: tok
      case. tint do.
         r=.r, > (".tok) { y
         NB.catch. echo 'argument ', tok, ' not found!' end.
    end.
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
hello =: program gen 'helloj'; 'kvm,cw'; read 2
procedure say(msg:string);
  begin
    ccenterxy(
      kvm.xmax div 2, kvm.ymax div 2 - 5,
      '|b-|B=|K[ |W' + msg + ' |K]|B=|b-');
    gotoxy(0,kvm.ymax-8);
  end;
)
clrscr; say('hello world');
)


hello fwrite 'gen/helloj.pas'
shell 'fpc gen/helloj.pas'
echo  'generated and compiled gen/helloj'
exit''
