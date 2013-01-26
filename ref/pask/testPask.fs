######################################################
##
##  testPask:
##
##  A series of bottom-up tests to verify the
##  behavior of the Pask compiler.
##
##  John Earnest 
##
######################################################

:include "Pask.fs"
:include <Test/Tap.fs>

:var A
:var B
:var C

: main ( -- )
	35 plan
	666

	# text primitives:
	  9  white?  true = "white positive"    ok
	 65  white? false = "white negative"    ok
	 65 letter?  true = "letter positive 1" ok
	100 letter?  true = "letter positive 2" ok
	 13 letter? false = "letter negative"   ok
	 50  digit?  true = "digit positive"    ok
	 70  digit? false = "digit negative"    ok
	 50 d>num 2 =       "digit conversion"  ok

	# buffered input:
	"ABCD" >read
		curr 65 = "queue head 1" ok
		curr 65 = "queue head 2" ok
		getc 65 = "getc"         ok
		curr 66 = "queue read"   ok
		skip
		curr 67 = "skip"         ok
		name? true = "name?" ok
	clear-q
	"123456" >read
		0 xq d>num 1 = "queue index 1" ok
		3 xq d>num 4 = "queue index 2" ok
		1 xq d>num 2 = "queue index 3" ok
	clear-q

	# high-level parsing words:
	"123 0 43 " >read
		number> 123 = "number> 1" ok
		number>   0 = "number> 2" ok
		number>  43 = "number> 3" ok
	clear-q

	"var  boolean	int" >read
		"var"     match?  true = "match positive 1" ok
		"boolean" match?  true = "match positive 2" ok
		"failure" match? false = "match negative"   ok
		"int"     match?  true = "match positive 3" ok
	clear-q

	"one other one " >read
	push-scope
		name> A !
		A @ .name-val @ "one" -text 0 = "name value 1" ok
		name> B !
		B @ .name-val @ "other" -text 0 = "name value 2" ok
		name> C !
		A @ B @ != "name distinctness" ok
		A @ C @  = "name equivalence" ok
	pop-scope
	clear-q

	# expression emission
	"1 + 2 * 3 " >read
	parse-expression TYPE_INT = "expression 1 type"  ok
	RETURN code exec        7 = "expression 1 value" ok
	code code-head !
	clear-q

	"(5 - 6) > -2" >read
	parse-expression TYPE_BOOL = "expression 2 type"  ok
	RETURN code exec      true = "expression 2 value" ok
	code code-head !
	clear-q

	"(2 * 9) + 1 = 2 * (9 + 1)" >read
	parse-expression TYPE_BOOL = "expression 3 type"  ok
	RETURN code exec     false = "expression 3 value" ok
	code code-head !
	clear-q

	# check canary
	666 = "canary" ok
;