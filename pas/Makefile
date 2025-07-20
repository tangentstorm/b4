# XPL is http://github.com/tangentstorm/xpl

FPC = fpc -gl -Mobjfpc
ARGS ?= # specify ARGS="..." on make command line

main:
	@echo 'usage : make <target> ARGS="..."'
	@echo ''
	@echo 'targets:'
	@echo '   b4 : compile main program'
	@echo 'clean : remove fpc residue'
	@echo '  run : build/run b4'
	@echo ' runf : build/run b4f (b4 $ARGS -a ../bios/b4f.b4a)'
	@echo ' runi : build/run b4i (plain interpreter)'
	@echo ' runx : build/run b4ix (fancy interpreter)'
	@echo ' runa : build/run b4a (batch assembler)'
	@echo ' rund : build/run b4d (disassembler)'

always:

run: always b4
	./b4 $(ARGS)

runf: b4
	./b4 $(ARGS) -a ../bios/b4f.b4a

runi: b4i
	./b4i $(ARGS)

runx: b4ix
	./b4ix $(ARGS)

runa: b4a
	./b4a $(ARGS)

rund: b4d
	./b4d $(ARGS)

b4i: b4 b4i.pas ub4i.pas
	$(FPC) -B b4i.pas

b4ix: b4i b4ix.pas
	$(FPC) -B b4ix.pas

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

colortest: uhw_vt.pas colortest.pas
	$(FPC) -B colortest.pas
	./colortest

clean:
	@rm -f *.o *.ppu *.exe *~
