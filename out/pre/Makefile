XPL = ../lib/xpl/code
GEN = ../gen
FCL = ~/f/packages/fcl-base/src
FCLXML = ~/f/packages/fcl-xml/src
FPC = fpc -B -Fu$(XPL) -Fi$(XPL) -Mobjfpc -gl -FE$(GEN) -Fi$(GEN) \
          -Fu$(FCL) -Fu$(FCLXML)
PYTHON = python

main: grin

pre: pre.pas pre_gen.pas
	$(FPC) pre.pas

grin: grin.pas pre
	$(FPC) grin

templang: templang.pas
	$(FPC) -B templang.pas

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
