#!/usr/bin/env j
NB. pascal compiler/generator for j
require 'task'
require 'debug'
coinsert 'tokens trace' NB. defined herein

cocurrent'trace'
(loglev =: 5:) [ dbr 0 [ dbss 'gen *:*'
trace =: 4 : 'if. (loglev _) <: x do. echo y end.' ]

cocurrent'grammar'
rule =: noun

NB. ----------------------------------------------
NB. patterns to describe pascal syntax
NB. ----------------------------------------------
cocurrent 'pascal'
coinsert 'grammar'
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
'kName kSub kArg kInt' =: ->:i.4
'tNil tBit tStr tInt tNum tCpx'=: 0 1 2 4 8 16
'tBox tBig tRat tSym tUni'=: 32 64 128 65536 131072
tokT =: monad : 0
  NB. ----------------------------------------------
  NB. return the type of a template language token
  NB. ----------------------------------------------
  h =: {. y
  if. h e. '0123456789_' do. kInt
  elseif. h e. 'abcdefghijklmnopqrstuvwxyz' do. kName
  elseif. _ do.
    select. h
      case. 39{a. do. tStr   NB. quote
      case. 10{a. do. tNil   NB. linefeed
      case. '@' do. kSub
      case. '$' do. kArg
    end.
  end.
)
assert kSub = tokT '@'
cocurrent'base'

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
NB. a queue class for holding generated output
NB. ----------------------------------------------
coclass 'Queue'
destroy =: codestroy
create =: 3 : 0
  Q =: a:    NB. initialize the empty queue.
)
addto =: 3 : 0 NB. q y adds y to the queue
  try.
    if. Q = a: do. Q =: y else. Q =: Q , y end.
  catch.
    echo 'warning: failed to add this to queue:'; y
  end.
  # Q return.
)
drain =: 3 : 0 NB. d drains y items from the queue.
  r =. y {. Q
  if. y >: # Q do. Q =: a:
  else. Q =: (y - # Q) {. Q end.
  r return.
)


NB. -- templates -------------------------------
NB. parser class for templates
NB. --------------------------------------------
coclass'Template'
coinsert 'tokens trace'
destroy =: codestroy
create =: verb : 0
  toks =: ;: y
  tokp =: 0 [ tok =: ''
)
status =: verb : 0
  'tokp=',(":tokp),' | tok=', tok
)
more =: verb : 0
  tokp < # toks
)
step =: verb : 0
  typ =: tokT tok =: > tokp { toks
  if. typ e. kArg, kSub do.
    tokp =: tokp + 1
    tok =: > tokp { toks
  end.
  2 trace status''
  tokp =: tokp + 1
)
cocurrent'base'



NB. -- codegen----------------------------------
NB. this class merges a tree of nodes with a set
NB. of templates to generate the finished code.
NB. --------------------------------------------
coclass 'CodeGen'
coinsert 'tokens trace pascal'

destroy =: codestroy
  create  =: verb : 0  NB. cg =: (tpname;args) conew 'CodeGen'
  tp =: '' [ tstack =: a:
  args =: ''[ astack =: a:
  q =: ''conew'Queue'
)
push_state =: 3 : 0
  astack =: args; astack
  tstack =: tp ; tstack
)
pop_state =: 3 : 0
  args =: >{. astack
  tp   =: >{. tstack
  astack =: }. astack
  tstack =: }. tstack
)
get =: dyad : 0  NB. x get y -> x[y]
  if. (0 <: y) *. y < # x do. > y { x return.
  else. echo 'index error: ', (":y), '!' throw. end.
)
step =: verb : 0
  step__tp''
  if. typ__tp e. kArg, kSub do. arg =: args get ".tok__tp
  else. arg =: '' end.
  select. typ__tp
    case. tNil do. emit LF
    case. tStr do. emit }.}: tok__tp
    case. kArg do. emit arg
    case. kSub do. for_box. arg do. (gen &: >)/ >box end.
  end.
)
gen =: dyad : 0
  push_state''
  args =: y
  tp =: (x,'_pascal_')~ conew 'Template'
  while. more__tp'' do. step'' end.
  destroy__tp''
  pop_state''
)
emit  =: verb : 'addto__q y'
result =: verb : 'drain__q _'
cocurrent 'base'

gen =: dyad : 0
  NB. ----------------------------------------------
  NB. generate text from template x with data from y
  NB. ----------------------------------------------
  cg =. '' conew 'CodeGen'
  x gen__cg y
  r =. result__cg''
  destroy__cg''
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
