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

runi: b4i
	./b4i

b4i: b4 b4i.pas
	$(FPC) -B b4i.pas

b4: b4.pas ub4asm.pas ub4.pas ub4ops.pas uhw.pas uhw_vt.pas
	$(FPC) -B b4.pas

ub4.ppu: ub4.pas
	$(FPC) -B ub4.pas

ub4ops.pas: mkoptbl.pas ub4.pas
	$(FPC) -B mkoptbl.pas
	./mkoptbl

b4a: b4 b4a.pas
	$(FPC) -B b4a.pas

test:
	$(FPC) testb4a.pas
	./testb4a
