FPC = fpc -Fu.. -Fi.. -Mobjfpc
PYTHON = python

main: pre
	./pre

pre: pre.pas pre_match.pas pre_gen.pas
	$(FPC) pre.pas

pre_gen.pas :
	$(PYTHON) pre_gen.py > pre_gen.pas

clean:
	rm *~ *.gpi *.o *.pyc

test:
	echo "no tests yet... :("
