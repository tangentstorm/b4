XPL = ../lib/xpl/code
FPC = fpc -Fu$(XPL) -Fi$(XPL) -Mobjfpc -gl -FE../gen
PYTHON = python

main: grin

pre: pre.pas pre_gen.pas
	$(FPC) pre.pas

grin: grin.pas pre
	$(FPC) grin

pre_gen.pas : pre.pas pre_gen.py
	$(PYTHON) pre_gen.py > pre_gen.pas

clean:
	rm *~ *.gpi *.o *.pyc


run-tests.pas:
	ln -s $(XPL)/../test/run-tests.pas
test: run-tests.pas grin
	python $(XPL)/../test/gen-tests.py
	@$(FPC) run-tests.pas
	./run-tests
