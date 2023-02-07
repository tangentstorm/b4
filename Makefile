# directory paths, relative to this directory:
XPL       = lib/xpl/code
LNPAS     = lib/linenoise
GEN	  = gen
PPU	  = $(GEN)
EXE	  = $(GEN)
RETROPATH = lib/retro
NGAROTEST = $(RETROPATH)/test/ngaro/ngarotest.py

# extra libraries for zengl [ not in git yet :/ ]
AGG       = lib/aggpas/Source
ZGL       = lib/zengl

# TODO: figure out how to not hard-code this path :/
#       (probably means fpc.cfg)
PLATFORM  = i386-win64
ZENGLAGG  = -Fu$(AGG) -Fu$(ZGL)/src \
	-Fu$(ZGL)/lib/ogg/$(PLATFORM) \
	-Fu$(ZGL)/lib/theora/$(PLATFORM) \
	-Fu$(ZGL)/lib/zip/$(PLATFORM)  \
	-Fu$(ZGL)/lib/zlib/$(PLATFORM)

# ROOT should be path back to this directory from GEN
ROOT      = ../

# compiler paths
FPC       = fpc -gl -B -vnw -Fu$(XPL) -Fu$(LNPAS) -Fi$(XPL) -FE$(GEN)
PYTHON    = python

#------------------------------------------------------

targets:
	@echo 'available targets:'
	@echo '--------------------------'
	@echo "make build   -> build ./gen/retro"
	@echo "make retro   -> build and run ./gen/retrovm"
	@echo "make retrogl -> build and run ./gen/retrogl"
	@echo "make test    -> run all tests"
	@echo .
	@echo for grammar engine, cd ./pre

retro : build
	cd $(GEN); ./retro $(args)

retro-e : build
	cd $(GEN); ./retro -e $(args)

build : init any.ng

rxsdl :
	@$(FPC) -Mdelphi -dGL -dSDL $(ZENGLAGG) -dVIDEOKVM ng/retro.pas -oretrogl.exe
	$(GEN)\retrogl $(args)

rxzen :
	@$(FPC) -Mdelphi -dGL -dZEN $(ZENGLAGG) ng/retro.pas -oretrogl
	cd $(GEN); ./retrogl $(args)

retrogl : rxsdl

init    :
	@mkdir -p $(GEN)
	@rm -f $(GEN)/library $(GEN)/retroImage
	@git submodule init
	@git submodule update
	@ln -s ../$(RETROPATH)/library $(GEN)/library
	@ln -n $(RETROPATH)/retroImage $(GEN)/retroImage


any.xpl : lib/xpl/code/*.pas lib/xpl/code/*.pas
any.ng  : ng/*.pas
	@$(FPC) ng/retro.pas

test: test.ngaro test.core test.files. test.clean

test.ngaro : init
	cd $(GEN); $(PYTHON) $(ROOT)$(NGAROTEST) -n ./retro

test.files : init
	cd $(GEN); ./retro --with $(ROOT)$(RETROPATH)/test/files.rx

test.core : init
	cd $(GEN); ./retro --with $(ROOT)$(RETROPATH)/test/core.rx

test.clean : init
	cd $(GEN); rm -f *.img
