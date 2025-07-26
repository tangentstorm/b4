# Add .exe for Windows compatibility
EXE=
ifdef ComSpec
  EXE=.exe
endif

FPC = fpc -gl -Mobjfpc
ARGS ?= # specify ARGS="..." on make command line

B4 = b4$(EXE)
B4I = b4i$(EXE)
B4IX = b4ix$(EXE)
B4A = b4a$(EXE)
B4D = b4d$(EXE)
MKOPTBL = mkoptbl$(EXE)

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
	@echo ' colortest: build/run colortest'

always:

.PHONY: main always run runf runi runx runa rund clean colortest

run: always $(B4)
	./$(B4) $(ARGS)

runf: $(B4)
	./$(B4) $(ARGS) -a ../bios/b4f.b4a

runi: $(B4I)
	./$(B4I) $(ARGS)

runx: $(B4IX)
	./$(B4IX) $(ARGS)

runa: $(B4A)
	./$(B4A) $(ARGS)

rund: $(B4D)
	./$(B4D) $(ARGS)

ifdef ComSpec
b4: $(B4)
b4i: $(B4I)
b4ix: $(B4IX)
b4a: $(B4A)
b4d: $(B4D)
endif

$(B4I): $(B4) b4i.pas ub4i.pas
	$(FPC) -o$(B4I) -B b4i.pas

$(B4IX): $(B4I) b4ix.pas
	$(FPC) -o$(B4IX) -B b4ix.pas

$(B4): b4.pas ub4asm.pas ub4.pas ub4ops.pas uhw.pas uhw_vt.pas
	$(FPC) -o$(B4) -B b4.pas

ub4.ppu: ub4.pas
	$(FPC) -B ub4.pas

ub4ops.pas: $(MKOPTBL)
	./$(MKOPTBL)

$(MKOPTBL): mkoptbl.pas ub4.pas
	$(FPC) -o$(MKOPTBL) -B mkoptbl.pas

$(B4A): $(B4) b4a.pas
	$(FPC) -o$(B4A) -B b4a.pas

$(B4D): $(B4) b4d.pas
	$(FPC) -o$(B4D) -B b4d.pas

colortest: uhw_vt.pas colortest.pas
	$(FPC) -B colortest.pas
	./colortest

clean:
	@rm -f *.o *.ppu *.exe *~
