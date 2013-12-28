#!/usr/bin/env j
NB. pascal compiler/generator for j
require 'task'
type =: 3!:0
'tnil tbit tstr tint tnum tcpx'=: 0 1 2 4 8 16
'tbox tbig trat tsym tuni'=: 32 64 128 65536 131072

NB. ----------------------------------------------
NB. patterns to describe pascal syntax
NB. ----------------------------------------------
program =: 0 : 0
  '{$mode delphi}{$i xpc.inc}'
  'program ' 0 ';'
  1
)

tokT =: monad : 0
  NB. ----------------------------------------------
  NB. return the type of a j token (as lexed by ;:)
  NB. ----------------------------------------------
  h =: {. y
  if. h e. '0123456789_' do. tint
  else.
    select. h
      case. '''' do. tstr
      case. ''   do. tnil   NB. linefeed
    end.
  end.
)

gen =: dyad : 0
  NB. ----------------------------------------------
  NB. generate text from template x with data from y
  NB. ----------------------------------------------
  r =. ''                   NB. ]] echo ;: x return.
  for_token. ;: x do.
    tok =. > token          NB. ]] echo (lexT > tok);>tok=.>token
    select. tokT tok
      case. tnil do. r=.r, LF
      case. tstr do. r=.r, }.}: tok
      case. tint do.
         try. r=.r, > (".tok) { y
         catch.
           echo 'argument ', tok, ' not found!'
           exit''
         end.
    end.
  end.
  r return.
)

NB. -----------------------------------
NB. construct a simple pascal program
NB. -----------------------------------
hello =: program gen 'helloj'; (0 : 0)
uses kvm,cw;

procedure say(msg:string);
  begin
    ccenterxy(
      kvm.xmax div 2, kvm.ymax div 2 - 5,
      '|b-|B=|K[ |W' + msg + ' |K]|B=|b-');
    gotoxy(0,kvm.ymax-8);
  end;

begin
  clrscr;
  say('hello world');
end.
)


hello fwrite 'gen/helloj.pas'
shell 'fpc gen/helloj.pas'
echo  'generated and compiled gen/helloj'
exit''
