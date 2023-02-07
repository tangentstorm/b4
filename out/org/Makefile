XPL = ../lib/xpl/code
GEN = ../gen
FPC = fpc -Fu$(XPL) -Fi$(XPL) -Mobjfpc -gl -FE$(GEN) -Fi$(GEN)
PYTHON = python

org: org.pas
	$(FPC) org.pas

clean:
	rm *~ *.gpi *.o *.pyc

run-tests.pas:
	ln -s $(XPL)/../test/run-tests.pas

test: run-tests.pas org
	@python $(XPL)/../test/gen-tests.py $(GEN)
	@rm -f run-tests.pas
	@ln -s $(XPL)/../test/run-tests.pas
	$(FPC) run-tests.pas
	$(GEN)/run-tests
