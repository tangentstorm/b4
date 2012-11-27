XPL = ../lib/xpl/code
GEN = ../gen
FPC = fpc -Fu$(XPL) -Fi$(XPL) -Mobjfpc -gl -FE$(GEN) -Fi$(GEN)
PYTHON = python

main: grin

pre: pre.pas pre_gen.pas
	$(FPC) pre.pas

grin: grin.pas pre
	$(FPC) grin

pre_gen.pas : pre.pas pre_gen.py
	$(PYTHON) pre_gen.py

clean:
	rm *~ *.gpi *.o *.pyc

run-tests.pas:
	ln -s $(XPL)/../test/run-tests.pas

test: run-tests.pas grin
	@python $(XPL)/../test/gen-tests.py $(GEN)
	@rm -f run-tests.pas
	@ln -s $(XPL)/../test/run-tests.pas
	$(FPC) run-tests.pas
	$(GEN)/run-tests
