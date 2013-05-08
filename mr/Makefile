# Makefile for minrel
GEN = ../gen
# BIN: where to put the final executables
BIN = ../gen
XPL = ../lib/xpl/code
TANGLE = ../tangle.el
FPC = fpc -Mobjfpc -FE$(BIN) \
	-Fu$(GEN) -Fi$(GEN) \
	-Fu./code -Fi./code \
	-Fu$(XPL) -Fi$(XPL) \
	-gl

targets:
	@echo
	@echo 'available targets:'
	@echo
	@echo '  test    : run test cases'
	@echo '  clean   : delete compiled binaries and backup files'
	@echo

# init contains all the stuff that has to run up front
init:
	@mkdir -p $(GEN)
	@mkdir -p $(BIN)

# 'always' is just a dummy thing that always runs
always:

# clean removes all the generated files
clean:
	@rm -f *~ *.gpi *.o *.pyc
	@rm -f test_sd.b4sd
	@delp $(BIN)
	@rm -f $(GEN)/*


.tangled: ../web/ar.pas.org ../web/mr.pas.org
	cat ../web/ar.pas.org ../web/mr.pas.org > .tangled
	$(TANGLE) .tangled

# we use always here, else it'll see the test directory and assume we're done.
test: always .tangled test-runner
	@$(BIN)/run-tests $(paramline)
test-runner: test/*.pas code/*.pas
	cd test; python gen-tests.py ../$(GEN)
	$(FPC) -B test/run-tests.pas

