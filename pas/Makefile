# XPL is http://github.com/tangentstorm/xpl
XPL = ~/x
FPC = fpc -gl -Fu$(XPL)/code -Fi$(XPL)/code -Mobjfpc

main:
	@echo 'make targets:'
	@echo '   b4 : compile main program'
	@echo 'clean : remove fpc residue'
	@echo '  run : run b4'

always:

run: always b4
	./b4

runi: b4i
	./b4i

b4i: b4 b4i.pas ub4i.pas
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

b4d: b4 b4d.pas
	$(FPC) -B b4d.pas

clean:
	@rm -f *.o *.ppu b4 b4.exe *~
