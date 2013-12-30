#!/usr/bin/env j
NB. pascal compiler/generator for j
require 'task'
require 'debug'
(loglev =: 5:) [ dbr 0 [ dbss 'gen *:*'
trace =: 4 : 'if. (loglev _) <: x do. echo y end.' ]

ntyp =: 3!:0
'tNil tBit tStr tInt tNum tCpx'=: 0 1 2 4 8 16
'tBox tBig tRat tSym tUni'=: 32 64 128 65536 131072
'kName kList kArgv' =: ->:i.3
rule =: noun



NB. ----------------------------------------------
NB. patterns to describe pascal syntax
NB. ----------------------------------------------
program =: rule : 0
  '{$mode delphi}{$i xpc.inc}'
  'program ' $0 ';'
  'uses ' $1 ';'
     @2
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
  NB. return the type of a template language token
  NB. ----------------------------------------------
  h =: {. y
  if. h e. '0123456789_' do. tInt
  elseif. h e. 'abcdefghijklmnopqrstuvwxyz' do. kName
  elseif. _ do.
    select. h
      case. 39{a. do. tStr   NB. quote
      case. 10{a. do. tNil   NB. linefeed
      case. '@' do. kList
      case. '$' do. kArgv
    end.
  end.
)
assert kList = tokT '@'

NB. ----------------------------------------------
NB. nodes
NB. ----------------------------------------------
NB. A node is a boxed array with two cells.
NB. The left cell contains the tag (a string)

NB. 'tag' node 'arg1';'arg2','...' -> Node
node =: < @: , &: <

NB. ntag :: Node -> Str       TODO: symbols
ntag =: > {.


NB. ----------------------------------------------
NB. a global queue for holding generated output
NB. ----------------------------------------------
Q =: a:    NB. initialize the empty global queue.
q =: 3 : 0 NB. q y adds y to the queue
  if. Q = a: do. Q =: < y else. Q =: Q , < y end.
  # Q return.
)
d =: 3 : 0 NB. d drains y items from the queue.
  r =. y {. Q
  Q =: (y - # Q) {. Q
  if. 0 = # Q do. Q =: a: end.
  r return.
)

get =: dyad : 0  NB. x get y -> x[y]
  1 trace 'loading item ', ":y
  if. (0 <: y) *. y < # x do. > y { x return.
  else. echo 'index error: ', (":y), '!' throw. end.
)

gen =: dyad : 0
  NB. ----------------------------------------------
  NB. generate text from template x with data from y
  NB. ----------------------------------------------
  i =. 0 [ state =. 0 [ argp =. 0 [ toks =. ;: x~
  while. i < # toks do.
    typ =. tokT tok =. > i { toks [ arg =. '' [ next =. state
    2 trace 'i=',(":i),' | tok=', tok,' | state=',(":state)
    NB. -- ints in template set arg to item in y ---
    if. typ = tInt do. arg =. y get argp =. ".tok end.
    NB. -- state machine ---------------------------
    select. state
    case. 0 do. NB. ---- handle simple tokens ------
      select. typ
      case. kList do. next =. 1
      case. tNil do. q LF
      case. tStr do. q }.}: tok
      case. tInt do. q arg
      end.
    case. 1 do. NB. ---- handle lists of nodes -----
      for_box. arg do. q (gen &: >)/ >box end.
      next =. 0
    end.
    i =. i + 1 [ state =. next
  end.
  d _ return.
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
say =: 'procedure' node 'say'; '(msg:string)'; ''; read 1
  ccenterxy(
    kvm.xmax div 2, kvm.ymax div 2 - 5,
    '|b-|B=|K[ |W' + msg + ' |K]|B=|b-');
  gotoxy(0,kvm.ymax-8);
)

sayit =: 'procedure' node 'sayit'; ''; ''; read 1
  // testing multiple procedure defs (also one with no arguments)
  say('hello world');
)

NB. exit '' ]] echo 0: L:0 say, sayit

hello =: 'program' gen 'helloj'; 'kvm,cw'; (say , sayit); read 1
  clrscr; sayit
)


hello fwrite 'gen/helloj.pas'
shell 'fpc gen/helloj.pas'
echo  'generated and compiled gen/helloj'
exit''
