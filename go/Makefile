FPC = fpc -Fu../lib/xpl/code -Fi../lib/xpl/code -Mobjfpc
FPC = fpc -gl -Fu~/x/code -Fi~/x/code -Mobjfpc

main: b4
	./b4

bed: bed.pas sd.pas clean
	$(FPC) -B bed.pas

test_bed: bed test_sd.pas
	$(FPC) -B test_sd.pas

b4: b4.pas vm.pas
	$(FPC) b4.pas

bp: ../ref/bplus.org
	 ../tangle.el ../ref/bplus.org
	$(FPC) -B bpdemo.pas
	./bpdemo

hx: hx.pas
	$(FPC) hx.pas

clean:
	rm -f *~ *.gpi *.o *.ppu

test:
	./test_b4

test_code: b4 test_*.pas
	gpc --automake test_b4.pas -o test_b4

