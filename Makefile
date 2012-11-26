# directory paths, relative to this directory:
XPL       = ./lib/xpl/code
GEN	  = ./gen
PPU	  = $(GEN)
EXE	  = $(GEN)
RETROPATH = ./lib/retro
NGAROTEST = $(RETROPATH)/test/ngaro/ngarotest.py

# ROOT should be path back to this directory from GEN
ROOT      = ../

# compiler paths
FPC       = fpc -gl -B -Fu$(XPL) -Fi$(XPL) -FE$(GEN)
PYTHON    = python

#------------------------------------------------------

retro : init *.pas
	$(FPC) retro.pas


init    :
	@mkdir -p $(GEN)
	@rm -f $(GEN)/library $(GEN)/retroImage
	@git submodule init
	@git submodule update
	@ln -s $(RETROPATH)/library $(GEN)/library
	@ln -n $(RETROPATH)/retroImage $(GEN)/retroImage

test : retro
	cd $(GEN); $(PYTHON) $(ROOT)$(NGAROTEST) -n ./retro

test.files : retro
	cd $(GEN); ./retro --with $(ROOT)$(RETROPATH)/test/files.rx

test.core : retro
	cd $(GEN); ./retro --with $(ROOT)$(RETROPATH)/test/core.rx

test.clean :
	cd $(GEN); rm -f *.img

