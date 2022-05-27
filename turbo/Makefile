FPC = fpc -Fu../lib/xpl/code -Fi../lib/xpl/code -Mobjfpc
FPC = fpc -gl -Fu~/x/code -Fi~/x/code -Mobjfpc

main:
	@echo 'make targets:'
	@echo '   b4 : compile main program'
	@echo ' test : run tests'
	@echo '  run : run b4'

always:

run: always b4
	./b4

b4 : b4.pas ub4.pas ub4ops.pas ub4asm.pas
	$(FPC) -B b4.pas

ub4.ppu: ub4.pas
	$(FPC) -B ub4.pas

ub4ops.pas: mkoptbl.pas ub4.ppu
	$(FPC) -B mkoptbl.pas
	./mkoptbl

test:
	$(FPC) testb4a.pas
	./testb4a
