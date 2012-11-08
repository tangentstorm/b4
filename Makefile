RETROPATH = ~/vrx
NGAROTEST = python $(RETROPATH)/test/ngaro/ngarotest.py

retro : *.pas
	fpc -gl -B retro.pas

test : retro
	$(NGAROTEST) -n ./retro

test.files : retro
	./retro --with $(RETROPATH)/test/files.rx

test.core : retro
	./retro --with $(RETROPATH)/test/core.rx

clean :
	rm -f *.img

status : clean
	clear
	@git status
