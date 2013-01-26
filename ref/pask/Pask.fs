######################################################
##
##  Pask
##
##  A compiler for a subset of the Pascal programming
##  language, demonstrating a recursive-descent parser
##  and MakoVM bytecode emission.
##
##  John Earnest
##
######################################################

:include <Print.fs>

# comments go in {}, but I will have a preprocessor
# for including files and such- the preprocessor
# could go ahead and strip comments too.

# Practically speaking these will probably
# need to be much bigger. For testing purposes
# I'm rather curious how much we can do with 8k:
:const dict-size 4096
:const code-size 4096

######################################################
##
##  Utilities:
##
######################################################

: revector  1 + !            ; ( 'new-word 'word -- )
: inc       dup @ 1 + swap ! ; ( addr -- )
: dec       dup @ 1 - swap ! ; ( addr -- )

: >move ( src dst len -- )
	1 - for
		over i + @ over i + !
	next 2drop
;

: -text ( a b -- flag )
	loop
		over @ over @
		2dup - if - >r 2drop r> exit then
		and -if 2drop 0 exit then
		1 + swap 1 + swap
	again
;

: text-size ( addr -- len )
	0 loop
		over @ -if swap drop break then
		1 + swap 1 + swap
	again
;

######################################################
##
##  Error handler:
##
######################################################

:data line 1
:var  char

: advance ( char -- )
	10 = if
		line inc
		0 char !
	else
		char inc
	then
;

: fail ( string -- )
	type
	" (line " type line @ .num
	", char " type char @ .num
	")" typeln
	halt
;

######################################################
##
##  Input buffering and core parser:
##
######################################################

# the default parser reads from stdin:
:vector read  CO @ dup advance ; ( -- char )

# or we can revector it to read from a string:
:var text-src
: read-text ( -- char )
	text-src @ @ dup if
		text-src inc
	else
		drop -1 # EOF
	then
	dup advance
;
: >read ( string -- )
	text-src !
	' read-text ' read revector
;

:array q 10 0
:var a # head
:var b # tail
:var s # size

: q+   dup @ 1 + 10 mod swap ! ; ( addr -- )
: >q   a @ q + !  a q+  s inc  ; ( char -- )
: q>   b @ q + @  b q+  s dec  ; ( -- char )
: clear-q   0 a ! 0 b ! 0 s !  ; ( -- )

: xq ( index -- char )
	s @ over <= if
		dup s @ - for
			read >q
		next
	then
	b @ + 10 mod q + @
;

: pull   s @ 1 < if read >q then ; ( -- )
: skip   pull q> drop            ; ( -- )
: curr   pull b @ q + @          ; ( -- char )
: getc   pull q>                 ; ( -- char )

: white?   dup 9 = over 10 = or swap 32 = or ; ( char -- flag )
: letter?  32 or dup 96 > swap 123 < and     ; ( char -- flag )
: digit?         dup 47 > swap  58 < and     ; ( char -- flag )
: d>num    48 -                              ; ( char -- n )

: space?   curr white?  ; ( -- flag )
: name?    curr letter? ; ( -- flag )
: numeral? curr digit?  ; ( -- flag )
: eof?     curr -1 =    ; ( -- flag )
: newline? curr 10 =    ; ( -- flag )

: trim ( -- )
	space? if
		loop skip space? while
	then
;

######################################################
##
##  Scoped Dictionary and Allocator:
##
##  In essence we need dictionaries to keep track
##  of the following, based on my current conception
##  of the problem space:
##
##  - Names (identifiers)
##  - Types
##  - Base-level semantic elements such as
##    functions, procedures, constants and vars.
##
######################################################

:array dict dict-size 0

:data here dict
: alloc ( size -- base )
	here @ swap over + here !
	dict dict-size + here @ <= if
		"Compiler heap exhausted!" fail
	then
;

# name dictionary:
:var names
: .name-prev     ;
: .name-val  1 + ;
:const name-size 2

: name+ ( string -- id )
	names @ loop
		dup -if
			drop
			dup name-size alloc       # allocate a new entry
			names @ over .name-prev ! # link to previous name
			names !                   # update head
			dup text-size 1 + alloc   # allocate space for string
			dup names @ .name-val !   # link to string
			over text-size 1 + >move  # copy string
			drop names @ exit
		then
		over over .name-val @ -text -if
			swap drop exit
		then
		.name-prev @
	again
;

# semantic dictionary:
:var head
: .prev      ; # previous dict entry
: .kind  2 + ; # const, var, etc
: .name  1 + ; # identifier associated with this node
: .val   3 + ; # value, entrypoint for procedure/function
: .type  4 + ; # pascal type- int, bool, etc
: .ret   5 + ; # return type- functions only.
:const entry-size 6

:const KIND_CONST 1
:const KIND_TYPE  2
:const KIND_VAR   3
:const KIND_PROC  4
:const KIND_FUNC  5

: name>dict ( id -- addr )
	head @ loop
		dup -if
			"Dictionary entry not found." fail
		then
		over over .name @ = if
			swap drop exit
		then
		.prev @
	again
;

: dict+ ( kind -- addr )
	head @
	entry-size alloc dup head !
	swap over .prev !
	swap over .kind !
;

# type dictionary:
:var types 0
: .type-prev      ; # previous type entry in the chain
: .type-count 1 + ; # -1 if scalar, size for array
: .type-first 2 + ; # index of first array element
: .type-list  3 + ; # field list for this record

: .field-val      ; # either a prim id OR addr of another node
: .field-next 1 + ; # 0 or next field entry

:const TYPE_INT   2
:const TYPE_CHAR  3
:const TYPE_BOOL  4

: name>type ( id -- addr )
	name>dict
	dup .kind @ KIND_TYPE != if
		"Type name expected." fail
	then
	.type @
;

: type+ ( -- id )
	"TYPE+ NOT IMPLEMENTED!" typeln halt
;

: assert-types ( id1 id2 -- )
	= -if
		"Type mismatch." fail
	then
;

: assert-boolean ( type -- )
	TYPE_BOOL = -if
		"Boolean expression expected." fail
	then
;

: assert-int ( type -- )
	TYPE_INT = -if
		"Integer expression expected." fail
	then
;

: assert-2boolean ( type -- )
	assert-boolean
	assert-boolean
;

: assert-2int ( type -- )
	assert-int
	assert-int
;

# scope stack:
:array scope 75 0
:data scope-ptr scope
: push-scope ( -- )
	scope-ptr @
	 here @ over     !
	 head @ over 1 + !
	names @ over 2 + !
	types @ over 3 + !
	4 + scope-ptr !
;

: pop-scope ( -- )
	scope-ptr @ 3 -
	dup     @  here !
	dup 1 + @  head !
	dup 2 + @ names !
	dup 3 + @ types !
	scope-ptr !
;

######################################################
##
##  High-level parsing words:
##
######################################################

: match? ( string -- flag )
	dup 0 loop
		( string char* queue-index )
		over @ -if
			2drop text-size 
			1 - for skip next
			true trim exit
		then
		over @ over xq != if
			2drop drop false exit
		then
		1 + swap 1 + swap
	again
;

: expect ( string -- )
	dup match? if
		drop
	else
		"Expected '" type type "'." fail
	then
;

: number> ( -- n )
	numeral? -if "Numeral expected." fail then
	0 loop
		10 * getc d>num +
		numeral?
	while trim
;

:array pad 33 0 :var pad@
: >pad  pad@ @ ! pad@ inc ; ( char -- )

: name> ( -- id )
	name? -if "Name expected." fail then
	pad pad@ !
	loop
		getc >pad
		name? numeral? or
	while trim
	0 >pad
	pad name+
;

######################################################
##
##  Compilation guts:
##
######################################################

: sizeof ( type -- cells )
	dup TYPE_INT  = if drop 1 exit then
	dup TYPE_CHAR = if drop 1 exit then
	dup TYPE_BOOL = if drop 1 exit then
	# return 1 when field is a pointer?

	dup .type-list @ 0 loop
		( field-node sofar )
		over .field-val  @ sizeof +
		swap .field-next @ swap over 
	while swap drop

	# multiply by array size:
	swap .type-count @ dup 0 < if -1 * then *
;

: local-addr ( id -- offset )
	# Walk down the dictionary chain:
	0 >r head @ loop
		dup -if "Local not found." fail then
		dup .kind @ KIND_VAR = if
			# sum sizes of any local storage,
			dup .type @ sizeof r> + >r
			# stopping when we find a given var:
			2dup .name @ = if break then
		then
	again
	2drop r>
;

: local-alloc ( size -- base )
	DP @ 2dup + DP !
;

: local-top ( size -- )
	1 + DP @ -
;

: local-free ( size -- )
	local-top DP !
;

######################################################
##
##  Code Emission:
##
######################################################

:array code code-size 0
:data code-head code
: >prog  code-head @ ! code-head inc ; ( n -- )

#: >prog   dup "CODE: " type . cr code-head @ ! code-head inc ;

# keep track of how much junk is on the
# data stack in addition to locals:
:var static-offset

: CONST    0 >prog >prog  static-offset inc ; ( n -- )
: CALL     1 >prog >prog                    ; ( addr -- )
: LOAD    10 >prog                          ; ( -- )
: STOR    11 >prog        static-offset dec ; ( -- )
: RETURN  12 >prog                          ; ( -- )
: DROP    13 >prog        static-offset dec ; ( -- )
: SWAP    14 >prog                          ; ( -- )
: DUP     15 >prog        static-offset inc ; ( -- )
: OVER    16 >prog        static-offset inc ; ( -- )
: STR     17 >prog        static-offset dec ; ( -- )
: RTS     18 >prog        static-offset inc ; ( -- )

: ADD     19 >prog        static-offset dec ; ( -- )
: SUB     20 >prog        static-offset dec ; ( -- )
: MUL     21 >prog        static-offset dec ; ( -- )
: DIV     22 >prog        static-offset dec ; ( -- )
: MOD     23 >prog        static-offset dec ; ( -- )

: AND     24 >prog        static-offset dec ; ( -- )
: OR      25 >prog        static-offset dec ; ( -- )
: XOR     26 >prog        static-offset dec ; ( -- )
: NOT     27 >prog                          ; ( -- )
: SGT     28 >prog        static-offset dec ; ( -- )
: SLT     29 >prog        static-offset dec ; ( -- )

: JUMP     2                                ; ( -- op )
: JUMPZ    3              static-offset dec ; ( -- op )
: JUMPIF   4              static-offset dec ; ( -- op )

: forward  >prog code-head @ 0 >prog        ; ( op -- addr )
: patch    code-head @ swap !               ; ( addr -- )
: mark     code-head @                      ; ( -- addr )
: backward >prog >prog                      ; ( addr op -- )

######################################################
##
##  'Heavy' data movers:
##
##  These all take types at compile-time and use
##  them to emit appropriate code for run-time
##  equivalents of the given operations.
##
######################################################

# SHOULD THESE HANDLE ZERO-SIZE STRUCTS AS NOPS?

: T@ ( [addr] type -- [value] )
	sizeof
	dup 1 = if drop LOAD exit then

	dup CONST ' local-alloc CALL # allocate space for target
	dup CONST ' >move       CALL # perform the copy
	2 - static-offset @ + static-offset !
;

: T! ( [val] [addr] type -- )
	sizeof
	dup 1 = if drop STOR exit then

	dup CONST ' local-top CALL SWAP # calculate base address
	dup CONST ' >move CALL          # perform the copy
	dup CONST ' local-free CALL     # deallocate space
	2 + static-offset @ - static-offset !
;

: T>r ( [val] type -- )
	sizeof
	dup 1 = if drop STR exit then

	# I could probably optimize this into a
	# compiled loop for sufficiently large records:
	1 - for STR next
;

: Tr> ( type -- [val] )
	sizeof
	dup 1 = if drop RTS exit then
	1 - for RTS next
;

######################################################
##
##  Expression parsing:
##
######################################################

: parse-variable ( var-record -- type )

	# we'll have to add some special-case logic
	# to see if this is the return value of a function, maybe.

	1 CONST LOAD # DP
	dup local-addr static-offset @ +
	CONST SUB

	# various things are necessary if we're
	# dealing with an array indexing expression.
	
	.type @
;

:proto parse-expression
:proto expression

:vector primary ( -- type )
	"(" match? if
		expression
		")" expect
		exit
	then
	numeral? if
		number> CONST
		TYPE_INT exit
	then
	name> name>dict
	dup .kind @ KIND_CONST = if
		.val @ CONST
		TYPE_INT exit
	then
	dup .kind @ KIND_VAR = if
		parse-variable
		dup T@
		exit
	then
	dup .kind @ KIND_FUNC = if
		( function-record )
		"(" expect
		loop
			parse-expression
			# validate this expression's return type!
			")" match? if break then
			"," expect
		again
		dup
		.val @ CALL
		.ret @ exit
	then
	drop
	"Unrecognized primary expression." fail
;

: unary ( -- type )
	"-" match? if
		unary assert-int
		-1 CONST MUL TYPE_INT exit
	then
	"not" match? if
		unary assert-boolean
		NOT TYPE_BOOL exit
	then
	primary
;

: multiplicative ( -- type )
	unary
	"*" match? if
		multiplicative assert-2int
		MUL TYPE_INT exit
	then
	"/" match? if
		multiplicative assert-2int
		DIV TYPE_INT exit
	then
	"mod" match? if
		multiplicative assert-2int
		MOD TYPE_INT exit
	then
	"and" match? if
		multiplicative assert-2boolean
		AND TYPE_BOOL exit
	then
;

: additive ( -- type )
	multiplicative
	"+" match? if
		additive assert-2int
		ADD TYPE_INT exit
	then
	"-" match? if
		additive assert-2int
		SUB TYPE_INT exit
	then
	"or" match? if
		additive assert-2boolean
		OR TYPE_BOOL exit
	then
;

: expression ( -- type )
	additive
	"<>" match? if
		expression over assert-types sizeof
		dup 1 = if drop ' != CALL TYPE_BOOL exit then

		"LARGE <> NOT IMPLEMENTED" typeln halt
		exit
	then
	"<" match? if
		expression assert-2int
		SLT TYPE_BOOL exit
	then
	">" match? if
		expression assert-2int
		SGT TYPE_BOOL exit
	then
	"<=" match? if
		expression assert-2int
		SGT NOT TYPE_BOOL exit
	then
	"=>" match? if
		expression assert-2int
		SLT NOT TYPE_BOOL exit
	then
	"=" match? if
		expression over assert-types sizeof
		dup 1 = if drop ' = CALL TYPE_BOOL exit then

		"LARGE = NOT IMPLEMENTED" typeln halt
		exit
	then
;

: parse-expression ( -- type )
	expression
;

######################################################
##
##  Top-level parser:
##
######################################################

:proto parse-block

: parse-statement ( -- )
	"if" match? if
		parse-expression assert-boolean
		JUMPZ forward
		"then" expect
		parse-statement
		"else" match? if
			JUMP forward swap patch
			parse-statement
		then
		patch
		exit
	then

	"while" match? if
		mark
		parse-expression assert-boolean
		"do" expect
		JUMPZ forward
		parse-statement
		swap JUMP backward
		patch
		exit
	then

	"repeat" match? if
		mark
		loop
			parse-statement
			";" match?
		while
		"until" expect
		parse-expression assert-boolean
		JUMPZ backward
		exit
	then

	"for" match? if
		"FOR NOT IMPLEMENTED!" typeln halt

		# parse assignment ( x := 10 )
		# name> name>var assert-int >r

		( | variable-record )

		":=" expect
		parse-expression assert-int
		i CONST STOR
		JUMP forward mark
		"to" match? if
			true
		else
			"downto" expect false
		then >r
		j CONST LOAD
		1 CONST i if ADD else SUB then
		j CONST STOR
		swap patch r> r>
		CONST LOAD
		parse-expression assert-int
		if SGT else SLT then JUMPIF forward
		"do" expect
		parse-statement
		swap JUMP backward patch
		exit
	then

	"begin" match? if
		loop
			parse-statement
			";" match?
		while
		"end" expect
		exit
	then

	name> name>dict

	dup .kind @ KIND_VAR = if
		( variable-record )
		dup parse-variable
		dup T>r
		":=" expect
		parse-expression
		dup Tr>
		over assert-types
		T!
		drop exit
	then

	dup .kind @ KIND_PROC = if
		( procedure-record )
		"(" expect
		loop
			parse-expression
			# validate this expression's return type!
			")" match? if break then
			"," expect
		again
		.val @ CALL
		exit
	then

	drop
	"Unrecognized statement." fail
;

: parse-simple-type ( -- type )
	"integer" match? if TYPE_INT  exit then
	"boolean" match? if TYPE_BOOL exit then
	"char"    match? if TYPE_CHAR exit then

	# ADD SUPPORT FOR ENUMS AND RANGES:
	"NOT IMPLEMENTED!" typeln halt
;

: parse-type-def ( name -- type )
	"^" match? if
		name>
		"POINTERS NOT IMPLEMENTED!" typeln halt
		exit
	then
	"array" match? if
		"ARRAYS NOT IMPLEMENTED!" typeln halt		
		exit
	then
	"record" match? if
		"RECORDS NOT IMPLEMENTED!" typeln halt
		exit
	then
	parse-simple-type
;

: parse-param-list ( -- 'head )
	
;

: parse-procedure? ( -- )
	"procedure" match? if
		KIND_PROC dict+ >r
		name> i .name ! # procedure name
		mark  i  .val ! # procedure entrypoint
		push-scope
			parse-param-list i .type !
			";" expect
			# EMIT ARG->PARAM MAPPING
			parse-block drop
			# EMIT ARG TEARDOWN
		pop-scope
		RETURN
		";" expect
		rdrop
	then
;

: parse-function? ( -- )
	"function" match? if
		KIND_FUNC dict+ >r
		name> i .name ! # function name
		mark  i  .val ! # function entrypoint
		push-scope
			parse-param-list i .type !
			":" expect
			name> name>type i .ret !
			";" expect
			# EMIT RET ALLOCATION
			# EMIT ARG->PARAM MAPPING
			parse-block drop
			# EMIT ARG TEARDOWN
		pop-scope
		RETURN
		";" expect
		rdrop
	then
;

: parse-block ( -- entrypoint )
	push-scope
	"const" match? if
		loop
			KIND_CONST dict+ >r
			name>   i .name !
			"=" expect
			number> i  .val !
			rdrop
			";" match?
		while
	then
	"type" match? if
		loop
			name> "=" expect parse-type-def
			# DO SOMETHING WITH THIS TYPE DEF DAMNIT
			";" match?
		while
	then
	0 # running total of local space
	"var" match? if
		loop
			KIND_VAR dict+ >r
			name>              i .name ! # variable name
			":" expect
			parse-type-def dup i .type ! # variable type
			sizeof +
			";" match?
		while
	then
	loop
		parse-procedure?
		parse-function?
		"begin" match?
	until
	mark >r # preserve entrypoint
	dup CONST ' local-alloc CALL
	loop
		parse-statement
		"end" match? if break then
		";" expect
	again
	CONST ' local-free CALL
	pop-scope
	r>
;

: parse-program ( -- entrypoint )
	"program" expect
	name> drop
	";" expect
	parse-block
	"." expect
	RETURN
;

: main ( -- )
	parse-program exec
	"PasK exited successfully." typeln
;