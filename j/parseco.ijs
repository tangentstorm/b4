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
s0 =: _ ; 0 ; '' ; 6#a:

NB. these names are the indices into the tuple:
'MB IX CH TB NT NA NB WK IB' =: i.#s0

NB. type s = (ix;ch;tb;nb;nt;na;wb)
NB.   mb = match bit
NB.   ix = current index into the input
NB.   ch = current character, or '' after ix>#S
NB.   tb = token buffer (grows as we match each char)
NB.   nt = node tag
NB.   na = node attributes
NB.   nb = node buffer (grows as we build rules)
NB.   wk = work stack (grows with recursive descent)
NB.   ib = input buffer

NB. accessor verbs: (v y) gets item from state,  (x v y) sets it.
AT =: {{ m&(>@{) : (<@[ m} ]) }}
(ix=:IX AT) (ch=:CH AT) (ib=:IB AT) (tb=:TB AT)
(nt=:NT AT) (na=:NA AT) (nb=:NB AT) (wk=:WK AT)
(I =:1&mb) (O =:0&mb) (mb=:MB AT)

NB. simple test framework
T =: [:`]@.mb NB. T = assert match
F =: ]`[:@.mb NB. F = assert doesn't match

NB. u AA v. s->s. apply u at v in y. (where v = (m AT))
AA =: {{ (u v y) v y }}

NB. m AP v. s->s. append m to v=(buffer AT) in y
AP =: {{ ,&m AA v y }}


NB. nx :: state->state = move to next character (ch-:'' if past end)
nx =: {{'nx'] i ix (i{ ::'' ib y) ch (ch y) AP tb y [ i=: 1 + ix y }}

NB. on: string -> s (initial parser state)
NB. everything is stored explicitly inside
NB. the state tuple, to make it easy to backtrack.
on =: {{ ({.y) ch y ib s0 }}
match =: on

NB. u parse: string -> parse tree | error
NB. applies rule u to (on y) and returns node buffer on success.
parse =: {{ if.mb s=.u on y do. nb s else. ,.'parse failed';<s end. }}

NB. u scan: string -> tokens | error
NB. applies rule u to (on y) and returns token buffer on success.
scan =: {{ if.mb s=.u on y do. tb s else. ,.'scan failed';<s end. }}

NB. parser combinators
NB. --------------------------------------------------
NB. these are all adverbs (or conjunctions in the case of 'sep')
NB. that take an argument describing what to match and produce
NB. a verb from s->s

NB. nil: s->s. always match, but consume nothing.
nil =: I

NB. any: s->s. matches one input item, unless out of bounds.
any =: {{ f mb nx^:f y [ f =. (#ib y)>ix y }}

any on 'hello'

NB. u neg: s->s. invert match flag from u and restore everything else
NB. from the original state after running u. This primitive allows
NB. you to implement negative lookahead (or apply it twice to implement
NB. positive lookahead without consuming).
neg =: {{'neg'] y mb~ -. mb u y }}

any neg on 'hello'


NB. u end: s->s. matches at end of input.
end =: any neg

end on 'x'
end on ''

NB. r try : s->s. generic error trap. mostly this handles
NB. the case where we're reading past the end of the input.
try =: :: O

NB. m chr: s->s. match literal atom m and advance the index
chr =: {{'chr'] p mb nx^:p y [ p =. m -: ch y }} try
'a' chr on 'xyz'
'a' chr on 'abc'

NB. m one: s->s. match one item from m and advance the index.
one =: {{'one'] p mb nx^:p y [ p =. m e.~ ch y }} try
one =: {{ y fw m e.~ ch y }} try

'abc' one on 'xyz'
'abc' one on 'cab'


NB. m seq: s->s. match each rule in sequence m
seq =: {{'seq'] s=:y
  for_r. m do.
    if. -.mb s=. r`:6 s do. O y return. end.
  end. I s }}

T ('a'chr)`('b'chr)`('c'chr) seq on 'abc'


NB. m alt: s->s. try each rule in m until one matches.
NB. This is "Prioritized Choice" from PEG parsers.
NB. It removes some ambiguity, but means you have to think
NB. carefully about how to order your rules. For example,
NB. if your language allows simple strings of letters to be
NB. used as names but also reserves some strings of letters
NB. as keywords, then you must specify the keywords first.
alt =: {{'alt'] s=:y
  for_r. m do.
    if. mb  s=. r`:6 s do. I s return. end.
  end. O y }}

F ('a'chr)`('b'chr)`('c'chr) alt on 'xyz'
T ('a'chr)`('b'chr)`('c'chr) alt on 'abc'


NB. m lit: s->s like seq for literals only.
NB. this just matches the whole sequence directly vs S.
NB. ,m is so we can match a single character.
lit =: {{'lit'] f mb nx^:(f*#m) y [ f=.m-:(ib y){~(ix y)+i.#m=.,m }} try


fw =: {{ (*y) mb nx^:y x }}

(X =: @[) (Y =: @])
NB. lookahead
la =: {{ (ib y) {~ (ix y) + (i. x) }}
la =: ib@] {~ ix@] + i.@[
la =: ib Y {~ ix Y + i. X
lit =: {{ y fw (#m) * m-: (#m=.,m) la y }} try

T 'ab' lit on 'abc'



NB. u ifu v: s->s. if u matches, return 1;<(s_old) v (s_new)
ifu =: {{ if.f=.mb s=.u y do. s=.y v s end. f mb s }}
ifu =: {{ f mb y v^:f s [ f=.mb s=.u y }}

NB. u tok: s->s move current token to NB if u matches, else fail
tok =: ifu({{ a: TB} (TB{y) AP nb y }}@])

T 'ab' lit tok on 'abc'


NB. m sym: s->s alias for 'm lit tok'
sym =: lit tok

T 'ab' sym on 'abc'


NB. u zap: s->s match if u matches, but drop any generated nodes
NB. the only effect that persists is the current char and index.
zap =: ifu {{'zap'] (ch y) ch (ix y) ix x }}
zap =: ifu(ch@] ch ix@] ix [)

NB. u opt: s->s. optionally match rule u. succeed either way
opt =: {{ I u y }}
opt =: `nil alt

T '3' lit opt on '1'
T '3' lit opt on '3'
T 'a'lit`('b'lit opt)`('c'lit) seq on 'abc'
T 'a'lit`('b'lit opt)`('c'lit) seq on 'acb'

NB. u rep: s->s. match 1+ repetitions of u
rep =: {{ f=.0 while. mb y =. u y do. f=.1 end. f mb y }}
rep =: {{ s=.y while. mb s=.u s do.end. y (<&ix mb ])s }}
rep =: {{ y (<&ix mb ]) u^:mb^:_ I y }}

NB. while =: {{ u ^: v ^:_ y }}
NB. rep =: {{ y (<&ix mb ]) u while mb I y }}

T ('a'lit rep) on 'aab'
F ('a'lit rep) on 'bba'
T ('a'lit rep)`('b'lit) seq on 'aaab'


NB. u orp: s->s. optionally repeat (match 0+ repetitions of u)}}
orp =: rep opt

NB. u not: s->s. match anything but u.
NB. fail if u matches or end of input, otherwise consume 1 input.
not =:{{
  if. (#ib y) <: ix y do. O y
  elseif.mb u y do. O y
  else. I nx y end. }}
not =: {{ (u neg)`any seq }}

T 'x' lit not on 'a'


NB. u sep v: s->s. match 1 or more u, separated by v
sep =: {{ u`(v`u seq orp) seq f. }}

NB. plain functions for tree building
NB. ---------------------------------

NB. ntup: the state indices to copy (in order) for current node
ntup =: NT,NA,NB

NB. x node: s->s. starts a new node in the parse tree with tag x
NB. copies current node tuple to work stack
node =: {{ x nt a: ntup } (<ntup{y) AP wk y }}

NB. x emit: s->s push item x into the current node buffer
emit =: {{ (<x) AP nb y }}

tok =: ifu {{x] '' tb (tb y) emit y }}
tok =: ifu ('' tb tb@] emit ])

NB. m attr n: s->s. append (m=key;n=value) pair to the attribute dictionary.
NB. initialize dict if needed
attr =: {{ if. a:-:NA{y do. y=. (0 2$a:) na s end. (m;n) AP na y }}
attr =: {{ (m;n) AP na ((0 2$a:)&na)^:(''-:na) y }}

NB. done: s->s. closes current node and makes it an item of previous node-in progress.
done =: {{
  new =. ntup { y       NB. temp storage for the node we're closing.
  'old s' =. wk tk y    NB. pop the previous context
  s =. (>old) ntup } s  NB. insert it into the state
  new emit s }}         NB. and append new node to the node buffer.
done =: {{ (ntup{y) emit (>old) ntup} s [ 'old s'=.wk tk y }}

NB. x tk: s->(item;<s). pop the last item from buffer x in state y.
tk =: {{ item ; < }: AA u y [ item =. ({: u y) }}
tk =: {{ ({:u y) ;< (}: AA u) y }}
NB. ^looks like a fork without the parens around (AA u) (but it's not)

NB. combinators for tree building.
NB. ------------------------------

NB. u elm n : s->s. create node element tagged with n if u matches
elm =: {{ if.mb  s=.u n node y do. I done s else. O y end. }}
elm =: {{ f mb y[`(done@])@.f s [ f=.mb s=.u n node y }}

NB. u atr n : s->s. if u matched, move last item to node attribute n.
atr =: {{ if.mb  s=. u y do. I n attr it s [ 'it s'=. nb tk s else. O y end. }}

NB. u tag: s->s. move the last token in node buffer to be the node's tag.
NB. helpful for rewriting infix notation, eg  (a head(+) b) -> (+ (a b))
tag =: {{'tag' if.mb  s=. u y do. I tok NT } s['tok s' =. nb tk y else. O y end. }}



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
WSz =: WS orp zap


NB. generic line splitter
lines =: {{ ,.> NB at s =. (NL not rep) tok sep (NL zap) on y }}


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

J_STR on h=.'''hello'',abc'

NB. c-style strings
STR_ESC =: ('\'lit)`any seq
DQ =: '"'lit
STR =: DQ`(STR_ESC`(DQ not) alt orp)`DQ seq tok


jsrc =: 0 : 0
avg =: +/ % #  NB. average is sum div len
avg +/\>:i.10  NB. average of first 10 triangle nums (22)
name =. 'Sally O''Malley'
)

[ expect =: (;:jsrc)
[ actual =: J_LEXER parse jsrc
assert expect -: actual

examples =: {{
  nx^:(<#S) s0 =: on S=.'hello (world 123)'
  'a'lit rep on 'aaa'
  ab =: ('a'lit)`('b'lit)
  ab seq on 'abcat'
  'c'lit opt on 'cat'   NB. 1
  'c'lit opt on 'bat'   NB. 1
  'c'lit on 'bat'       NB. 0
  'a'lit rep on 'aaaahello'
  'a' lit sep (','lit) on 'a,a,a.b'
  ('hello'lit)`(', 'lit zap)`('world'lit) seq on 'hello, world'
  (NL not) rep on 'hello'
  hi =: ('hello'sym)`(', 'lit zap)`('world'sym atr 'who') seq
  [  s =: hi on 'hello, world'
   s =: hi elm 'hi' on 'hello, world'
 }}

examples'' NB. no output, but will complain if anything broke

nil`any`end `:0@ on each '';'x'

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
block on 'begin hello; 2+2; end'

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
LP =: 'LP' trace (LPAREN zap)
RP =: 'RP' trace (RPAREN zap)
ID =: 'ID' trace (LP`RP`WS`DQ alt not rep tok)

NB. se: s-expression parser
se =: 'se' trace (WSz`LP`(se`ID`STR`WSz alt orp)`RP seq elm '' sep WSz)

NB. ll: lisp lexer
ll =: WSz`((LPAREN tok)`(RPAREN tok)`WS`ID`(STR tok) alt)seq orp

lisp =: '(one two (a b c) three) (banana)'
ll parse lisp
se parse lisp


NB. tree matching

NB. u all: s->s. matchs if u matches the entire remaining input.
all =: {{ (u f.)`end seq }}

NB. u box: s->s. matches if current value is
box =: {{
  if. 32 = 3!:0 c =. ch y
  do. smoutput 'entering box C:' [ C =: > c
      smoutput c
      f mb nx^:f s [f=.mb s=.u all on > c else. O y end. }}
