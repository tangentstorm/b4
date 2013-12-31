#!/usr/bin/env j
NB. pascal compiler/generator for j
require 'task'
require 'debug'
(loglev =: 5:) [ dbr 0 [ dbss 'gen *:*'
trace =: 4 : 'if. (loglev _) <: x do. echo y end.' ]

ntyp =: 3!:0
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

NB. ----------------------------------------------
NB. tokenizer
NB. ----------------------------------------------
cocurrent'tokens'
'kName kList kArgv' =: ->:i.3
'tNil tBit tStr tInt tNum tCpx'=: 0 1 2 4 8 16
'tBox tBig tRat tSym tUni'=: 32 64 128 65536 131072
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
coinsert 'tokens' [ cocurrent'base'

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
  if. Q = a: do. Q =: y else. Q =: Q , y end.
  # Q return.
)
d =: 3 : 0 NB. d drains y items from the queue.
  r =. y {. Q
  if. y >: # Q do. Q =: a:
  else. Q =: (y - # Q) {. Q end.
  r return.
)

NB. -- state machine ---------------------------
NB. This part parses the templates and generates
NB. a sequence of opcode;item records.
NB. --------------------------------------------
coclass 'CodeGen'
coinsert 'tokens'
destroy =: codestroy

create  =: verb : 0  NB. cg =: (toks;args) conew 'CodeGen'
  toks =: >> {. y
  args =: }. y
  argp =: 0 [ tokp =: 0 [ tok =: '' [ state =: 0 [ next =: 0
)

status =: verb : 0
  'tokp=',(":tokp),' | tok=', tok,' | state=',(":state)
)

more =: verb : 0
  tokp < # toks
)

get =: dyad : 0  NB. x get y -> x[y]
  if. (0 <: y) *. y < # x do. > y { x return.
  else. echo 'index error: ', (":y), '!' throw. end.
)

step =: verb : 0
  arg =: ''
  next =: state
  typ =: tokT tok =: > tokp { toks
  NB. -- ints in template set arg to item in y ---
  if. typ = tInt do. arg =: args get ".tok end.
)

end_step =: verb : 0
  tokp =: tokp + 1 [ state =: next
)
cocurrent 'base'

gen =: dyad : 0
  NB. ----------------------------------------------
  NB. generate text from template x with data from y
  NB. ----------------------------------------------
  cg =. ((< ;: x~);y) conew 'CodeGen'
  while. more__cg'' do.
    step__cg''
    2 trace status__cg''
    NB. -- state machine ---------------------------
    select. state__cg
    case. 0 do. NB. ---- handle simple tokens ------
      select. typ__cg
      case. kList do. next__cg =: 1
      case. tNil do. q LF
      case. tStr do. q }.}: tok__cg
      case. tInt do. q arg__cg
      end.
    case. 1 do. NB. ---- handle lists of nodes -----
      for_box. arg__cg do. q (gen &: >)/ >box end.
      next__cg =: 0
    end.
    end_step__cg''
  end.
  destroy__cg''
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

path =: 'gen/helloj.pas'
ferase path
hello fwrite path
shell 'fpc ',path
echo  'generated and compiled ',path
exit''
