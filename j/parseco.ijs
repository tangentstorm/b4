clear''
NB. Parser Combinators for J
NB.
NB. The semantics here are heavily inspired by
NB. Allesandro Warth's ometa system:
NB.
NB.   http://tinlizzie.org/ometa/
NB.
NB. but implemented as parser combinators rather than a standalone language.
NB.
LICENSE =: (0 : 0)
Copyright (c) 2021 Michal J Wallace <http://tangentstorm.com/>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
)

NB. s0 : s. initial parse state
s0 =: 0 ; '' ; 6#a:

NB. these names are the indices into the tuple:
'ix ch tb nt na nb wk ib' =: i.#s0

NB. type s = (ix;ch;tb;nb;nt;na;wb)
NB.   ix = current index into the input
NB.   ch = current character, or '' after ix>#S
NB.   tb = token buffer (grows as we match each char)
NB.   nt = node tag
NB.   na = node attributes
NB.   nb = node buffer (grows as we build rules)
NB.   wk = work stack (grows with recursive descent)
NB.   ib = input buffer
NB. type fs = (bit;<s).  flag + parse state
NB.   the flag indicates whether the rule that
NB.   was just invoked matched the input or not.

NB. accessor methods (combine with the names above: 'ix at state')
at =: >@{                    NB. fetch and unbox at index x from boxes y
aa =: {{ (u&.(>@]) n{ y) n} y }} NB. apply u at ix n in array of boxes y


S =: ib&at NB. retrieve the input string

NB. nx :: state->state = move to next character (ch-:'' if past end)
nx =: {{'nx'] (i; <i{ ::'' S y) 0 1 } ,&(ch at y) aa tb y [ i=: 1 + ix at y }}

NB. match: string -> s (initial parser state)
NB. everything is stored explicitly inside
NB. the state tuple, to make it easy to backtrack.
match =: {{ (<y) ib } (<{.y) ch} s0 }}

NB. u parse: string -> parse tree | error
NB. applies rule u to (match y) and returns node buffer on success.
parse =: {{ if.f['f s'=.u match y do. >nb{s else. ,.'parse failed';<s end. }}

NB. u lex: string -> tokens | error
NB. applies rule u to (match y) and returns token buffer on success.
lex =: {{ if.f['f s'=.u match y do. >tb{s else. ,.'parse failed';<s end. }}

NB. parser combinators
NB. --------------------------------------------------
NB. these are all adverbs (or conjunctions in the case of 'sep')
NB. that take an argument describing what to match and produce
NB. a verb from s->fs

NB. nil: s->fs. always match, but consume nothing.
nil =: 1;<

NB. any: s->fs. matches one input item, unless out of bounds.
any =: {{'any'] f;<nx^:f y [ f =. (#S y)>ix at y }}

NB. u neg: s->fs. consume nothing.
NB. This primitive allows you to implement lookahead.
neg =: {{'neg'] (-.f);< y [ 'f s' =. u y }}

NB. u end: s->fs. matches at end of input.
end =: any neg

NB. r try : s->fs. generic error trap. mostly this handles
NB. the case where we're reading past the end of the input.
try =: :: (0;<)

NB. m chr: s->fs. match literal atom m and advance the index
chr =: {{'chr'] p;<nx^:p y [ p =. m -: ch at y }} try

NB. m one: s->fs. match one item from m and advance the index.
one =: {{'one'] p;<nx^:p y [ p =. m e.~ ch at y }} try

NB. m seq: s->fs. match each rule in sequence m
seq =: {{'seq'] s=:y
  for_r. m do.
    if. -.f['f s'=. r`:6 s do. 0;<y return. end.
  end. 1;<s }}

NB. m alt: s->fs. try each rule in m until one matches.
NB. This is "Prioritized Choice" from PEG parsers.
NB. It removes some ambiguity, but means you have to think
NB. carefully about how to order your rules. For example,
NB. if your language allows simple strings of letters to be
NB. used as names but also reserves some strings of letters
NB. as keywords, then you must specify the keywords first.
alt =: {{'alt'] s=:y
  for_r. m do.
    if. f['f s'=. r`:6 s do. 1;<s return. end.
  end. 0;<y }}

NB. m lit: s->fs like seq for literals only.
NB. this just matches the whole sequence directly vs S.
NB. ,m is so we can match a single character.
lit =: {{'lit'] f;<nx^:(f*#m) y [ f=.m-:(S y){~(ix at y)+i.#m=.,m }} try

NB. eat =: {{ if. f['f s'=.u y do. s=. a: tb} ,&(tb{s) aa nb s end. f;<s }}
NB. zap =: {{ t =. tb{y if. f['f s'=.u y do. s=. t tb} s end. f;<s }}

NB. u ifu v: s->fs. if u matches, return 1;<(s_old) v (s_new)
ifu =: {{'ifu' if.f['f s'=.u y do. s=.y v s end. f;<s }}

NB. u tok: s->fs move current token to nb if u matches, else fail
tok =: ifu {{x] a: tb} ,&(tb{y) aa nb y }}
tok =: ifu {{'tok']x] a: tb} (tb at y) emit y }}

NB. m sym: s->fs alias for 'm lit tok'
sym =: lit tok

NB. u zap: s->fs match if u matches, but drop any generated nodes
NB. the only effect that persists is the current char and index.
zap =: ifu {{'zap'] ((ch,ix){y) (ch,ix) } x }}

NB. u opt: s->fs. optionally match rule u. succeed either way
opt =: {{ 1 ; }. u y }}
opt =: `nil alt

NB. u rep: s->fs. match 1+ repetitions of u
rep =: {{'rep'] f=.0 [ s=.y while. fi['fi s'=.u s do. f=.1 end. f;<s }}

NB. u orp: s->fs. optionally repeat (match 0+ repetitions of u)}}
orp =: rep opt

NB. u not: s->fs. match anything but u.
NB. fail if u matches or end of input, otherwise consume 1 input.
not =:{{'not'
  if. (#S y) <: ix at y do. 0;<y
  elseif. f['f s'=.{. u y do. 0;<y
  else. 1;<nx y end. }}
not =: {{('not'] (u neg)`any) seq }}


NB. u sep v: s->fs. match 1 or more u, separated by v
sep =: {{ (u f.)`((v f.)`(u f.) seq orp) seq }}

NB. plain functions for tree building
NB. ---------------------------------

NB. ntup: the state indices to copy (in order) for current node
ntup =: nt,na,nb

NB. x node: s->s. starts a new node in the parse tree with tag x
NB. copies current node tuple to work stack
node =: {{ (<x) nt } a: ntup } ,&(<ntup{y) aa wk y }}

NB. x emit: s->s push item x into the current node buffer
emit =: {{ ,&(<x) aa nb y }}

NB. m attr n: s->fs. append (m=key;n=value) pair to the attribute dictionary.
NB. initialize dict if needed
attr =: {{ if. a:-:na{s do. s=. (<0 2$a:) na} s end. ,&(m;n) aa na y }}

NB. done: s->s. closes current node and makes it an item of previous node-in progress.
done =: {{
  new =. ntup { y       NB. temp storage for the node we're closing.
  'old s' =. wk tk y    NB. pop the previous context
  s =. (>old) ntup } s  NB. insert it into the state
  new emit s }}         NB. and append new node to the node buffer.

NB. x tk: s->(item;<s). pop the last item from buffer x in state y.
tk =: {{ item ; < }: aa x y [ item =. {:>x{y }}


NB. combinators for tree building.
NB. ------------------------------

NB. u elm n : s->fs. create node element tagged with n if u matches
elm =: {{'elm' if.f['f s'=.u n node y do. 1;<done s else. 0;<y end. }}

NB. u atr n : s->fs. if u matched, move last item to node attribute n.
atr =: {{'atr' if.f['f s'=. u y do. 1;<n attr it s [ 'it s'=. nb tk s else. 0;<y end. }}

NB. u tag: s->fs. move the last token in node buffer to be the node's tag.
NB. helpful for rewriting infix notation, eg  (a head(+) b) -> (+ (a b))
tag =: {{'tag' if.f['f s'=. u y do. 1;<tok nt } s['tok s' =. nb take y else. 0;<y end. }}



NB. character sets
alpha =: a.{~ , (i.26) +/ a.i.'Aa'
digit =: '0123456789'
hexit =: digit,'AaBbCcDdEeFf'
other =: (32}.127{.a.)-.alpha,digit
paren =: '()'
brack =: '[]'
curly =: '{}'
space =: 32{.a. NB. that's all ascii ctrl chars.

NB. Some predefined tokens
NL =: (CR lit opt)`(LF lit) seq
ALPHA  =: alpha one
UNDER  =: '_' lit
DIGIT  =: digit one
NUMBER =: digit rep
IDENT  =: (ALPHA`(ALPHA`DIGIT`UNDER alt orp) seq)
HEXIT  =: hexit one
LPAREN =: '(' lit
RPAREN =: ')' lit
LBRACK =: '[' lit
RBRACK =: ']' lit
LCURLY =: '{' lit
RCURLY =: '}' lit
WS =: (TAB,' ') one

NB. generic line splitter
lines =: {{ ,.> nb at s [ 'f s' =. (NL not rep) tok sep (NL zap) match y }}


NB. j syntax rules
NB. -------------------------------

NB. fragments used by the tokens:
j_op  =: (brack,curly,other-.'_') one
j_num =: ('_' lit opt)`(DIGIT rep)`(('.'lit)`(DIGIT orp) seq opt) seq
squo =: ''''
squl =: squo lit
j_esc =: (2#squo) lit

NB. full tokens
J_LDEF =: '{{'lit
J_RDEF =: '}}'lit
J_NB  =: (('NB','.')lit)`(NL not rep) seq

J_STR =: squl`(j_esc`(squl not) alt orp)`squl seq
J_OPER =: (j_op`DIGIT`ALPHA alt)`('.:' one rep) seq
J_OP   =: j_op
J_NUMS =: j_num`('j'lit`j_num seq opt)seq sep (WS rep)
J_TOKEN =: NL`J_LDEF`J_RDEF`LPAREN`RPAREN`J_NB`J_STR`J_OPER`J_NUMS`J_OP`IDENT alt
J_LEXER =: (WS zap)`(J_TOKEN tok) alt rep

J_STR match h=.'''hello'',abc'


jsrc =: 0 : 0
avg =: +/ % #  NB. average is sum div len
avg +/\>:i.10  NB. average of first 10 triangle nums (22)
name =. 'Sally O''Malley'
)

[ expect =: (;:jsrc)
[ actual =: J_LEXER parse jsrc
assert expect -: actual

examples =: {{
  nx^:(<#S) s0 =: match S=.'hello (world 123)'
  'a'lit rep match 'aaa'
  ab =: ('a'lit)`('b'lit)
  ab seq match 'abcat'
  'c'lit opt match 'cat'   NB. 1
  'c'lit opt match 'bat'   NB. 1
  'c'lit match 'bat'       NB. 0
  'a'lit rep match 'aaaahello'
  'a' lit sep (','lit) match 'a,a,a.b'
  ('hello'lit)`(', 'lit zap)`('world'lit) seq match 'hello, world'
  (NL not) rep match 'hello'
  hi =: ('hello'sym)`(', 'lit zap)`('world'sym atr 'who') seq
  [ 'f s' =: hi match 'hello, world'
  'f s' =: hi elm 'hi' match 'hello, world'
 }}

examples'' NB. no output, but will complain if anything broke

nil`any`end `:0@ match each '';'x'

                                                          s0
                                                  'n'node s0
                                         'e' emit 'n'node s0
                              'n2' node  'e' emit 'n'node s0
                    'e2' emit 'n2' node  'e' emit 'n'node s0
               done 'e2' emit 'n2' node  'e' emit 'n'node s0
     'e3' emit done 'e2' emit 'n2' node  'e' emit 'n'node s0
done 'e3' emit done 'e2' emit 'n2' node  'e' emit 'n'node s0


NB. simple pascal -like block
BEGIN  =: 'begin' sym
END =: 'end' sym
nends =: END not orp tok
block =: (BEGIN`nends`END) seq
block match 'begin hello; 2+2; end'

TRACE =: 0
NB. m trace v: s->st. provides a trace of parse rule v if TRACE~:0
trace =: {{
  if. TRACE do. r [ smoutput m; r=.v Y=:y [ smoutput '>> ',m
  else. v y end. }}

NB. decompiler
ar =: 5!:1@<
br =: 5!:2@<
tr =: 5!:4@<
ops =: ;:'nil any lit one seq alt tok sym zap opt rep orp not sep elm atr tag run'
all =: ops,;:'try ifu'
ALL =: toupper each all
(ALL) =: br each all

NB. s-expressions (lisp-like syntax)
lp =: 'lp' trace (LPAREN zap)
rp =: 'rp' trace (RPAREN zap)
id =: 'id' trace (lp`rp`WS alt not rep tok)

NB. se: s-expression parser
se =: 'se' trace (lp`(id`se`(WS zap) alt orp)`rp seq elm '' sep (WS zap))

NB. ll: lisp lexer
ll =: (LPAREN tok)`(RPAREN tok)`(WS zap)`id alt rep

lisp =: '(one two (a b c) three) (banana)'
ll parse lisp
se parse lisp


NB. tree matching

NB. u all: s->fs. matchs if u matches the entire remaining input.
all =: {{ (u f.)`end seq }}

NB. u box: s->fs. matches if current value is
box =: {{
  if. 32 = 3!:0 c =. ch at y
  do. smoutput 'entering box C:' [ C =: > c
      smoutput c
      f;<nx^:f s [ 'f s'=.u all match > c else. 0;<y end. }}
