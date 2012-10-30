RETROPATH = ~/vrx
NGAROTEST = python $(RETROPATH)/test/ngaro/ngarotest.py

retro : *.pas
	fpc -gl -B retro.pas

test : retro
	$(NGAROTEST) -n ./retro

clean :
	rm -f *.img

status : clean
	clear
	@git status
