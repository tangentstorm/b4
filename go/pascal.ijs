#!/usr/bin/env j
NB. pascal compiler/generator for j
require 'task'

hello =: noun : 0
{$mode delphi}{$i xpc.inc}
program helloj;
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
