######################################################
##
##  SimpleLang:
##
##  A compiler for a very simple C-like language
##  leveraging the Pask compiler infrastructure,
##  meant as an integration test of Pask.
##
##  John Earnest 
##
######################################################

:include "Pask.fs"

: name>var ( id -- addr type )
	name>dict
	dup .kind @ KIND_VAR != if
		"Variable name expected." fail
	then
	dup   .val @
	swap .type @
;

: name>proc ( id -- addr )
	name>dict
	dup .kind @ KIND_PROC != if
		"Procedure name expected." fail
	then
	.val @
;

# Override the behavior of "primary"
# due to the differences in how variables
# work in this language. Otherwise we
# can just use the normal "parse-expression":

: simple-primary ( -- type )
	"(" match? if
		expression
		")" expect
		exit
	then
	numeral? if
		number> CONST
		TYPE_INT exit
	then
	name> name>var swap
	CONST LOAD
;

:proto parse-block

: parse-statement ( -- )
	"if" match? if
		"(" expect
		parse-expression assert-boolean
		")" expect
		JUMPZ forward
		parse-block
		patch
		exit
	then
	"forever" match? if
		mark
		parse-block
		JUMP backward
		exit
	then
	"for" match? if
		name> name>var assert-int >r
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
		parse-block
		swap JUMP backward patch
		exit
	then
	"print" match? if
		parse-expression assert-int
		' .  CALL
		' cr CALL
		";" expect
		exit
	then
	"return" match? if
		RETURN
		";" expect
		exit
	then
	name> name>dict
	dup .kind @ KIND_VAR = if
		dup   .val @
		swap .type @
		":=" expect
		parse-expression assert-types
		CONST STOR
		";" expect
		exit
	then
	dup .kind @ KIND_PROC = if
		.val @ CALL
		";" expect
		exit
	then
	"Variable or Procedure expected." fail
;

: parse-block ( -- )
	"{" expect
	loop
		"}" match? if break then
		parse-statement
	again
;

:var last-proc

: parse-program ( -- )
	loop
		"#" @ curr = if
			loop skip newline? until
			trim
		then
		"int" match? if
			KIND_VAR dict+ >r
			TYPE_INT i .type !
			name>    i .name !
			mark     i .val  !
			":=" match? if number> else 0 then >prog
			";" expect
			rdrop
		then
		"proc" match? if
			KIND_PROC dict+ >r
			name> i .name   !
			mark  i .val    !
			mark  last-proc !
			parse-block
			RETURN
			rdrop
		then

		eof?
	until
;

: main ( -- )
	' simple-primary ' primary revector
	parse-program
	"compiled successfully." typeln
	last-proc @ exec
	"exited successfully." typeln
;