RETROPATH = ~/vrx

retro : *.pas
	fpc -gl -B retro.pas

test :
	cd $(RETROPATH)/test/ngaro
	make test.pas

