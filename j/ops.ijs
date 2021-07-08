
NB. state accessors:
'ix ch tb nt na nb wk ib' =: i.#s0

NB. accessor methods (combine with the names above: 'ix at state')
at =: >@{                    NB. fetch and unbox at index x from boxes y
aa =: {{ (u&.(>@]) n{ y) n} y }} NB. apply u at ix n in array of boxes y


S =: ib&at NB. retrieve the input string

NB. nx :: state->state = move to next character (ch-:'' if past end)
NB. x tk: s->(item;<s). pop the last item from buffer x in state y.

NB. match: string -> s (initial parser state)
NB. everything is stored explicitly inside
NB. the state tuple, to make it easy to backtrack.
match =: {{ (<y) ib } (<{.y) ch} s0 }}

NB. u parse: string -> parse tree | error
NB. applies rule u to (match y) and returns node buffer on success.
parse =: {{ if.f['f s'=.u match y do. >nb{s else. ,.'parse failed';<s end. }}

NB. parser combinators
NB. --------------------------------------------------
NB. nil: s->fs. always match, but consume nothing.
NB. any: s->fs. matches one input item, unless out of bounds.
NB. u neg: s->fs. invert match flag from u and restore everything else.
NB. u end: s->fs. matches at end of input.
NB. m chr: s->fs. match literal atom m and advance the index
NB. m one: s->fs. match one item from m and advance the index.
NB. m seq: s->fs. match each rule in sequence m
NB. m alt: s->fs. try each rule in m until one matches.
NB. m lit: s->fs like seq for literals only.
NB. u ifu v: s->fs. invoke arbitrary function v on old/new states if u matches.
NB. u tok: s->fs move current token to nb if u matches, else fail
NB. m sym: s->fs alias for 'm lit tok'
NB. u zap: s->fs match if u matches, but drop any generated nodes
NB. u opt: s->fs. optionally match rule u. succeed either way
NB. u rep: s->fs. match 1+ repetitions of u
NB. u orp: s->fs. optionally repeat (match 0+ repetitions of u)}}
NB. u not: s->fs. match anything but u.
NB. u sep v: s->fs. match 1 or more u, separated by v

NB. plain functions for tree building
NB. ---------------------------------
NB. x node: s->s. starts a new node in the parse tree with tag x
NB. x emit: s->s push item x into the current node buffer
NB. m attr n: s->fs. append (m=key;n=value) pair to the attribute dictionary.
NB. done: s->s. closes current node and makes it an item of previous node-in progress.

NB. combinators for tree building.
NB. ------------------------------
NB. u elm n : s->fs. create node element tagged with n if u matches
NB. u atr n : s->fs. if u matched, move last item to node attribute n.
NB. u tag: s->fs. move the last token in node buffer to be the node's tag.

NB. predefined character sets
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

NB. tree matching
NB. ---------------------------------
NB. u all: s->fs. matchs if u matches the entire remaining input.
NB. u box: s->fs. matches if current value is a box and u matches it entirely.
