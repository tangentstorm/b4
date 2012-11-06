FPC = fpc -Fu.. -Fi.. -Mobjfpc

main: b4
	./b4

b4: b4.pas vm.pas
	$(FPC) b4.pas

clean:
	rm *~ *.gpi *.o

test:
	./test_b4

test_code: b4 test_*.pas
	gpc --automake test_b4.pas -o test_b4
