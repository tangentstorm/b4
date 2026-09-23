BEND ?= bend

.PHONY: all check test clean
all: b4i

b4i: b4i.bend vm.bend text.bend
	$(BEND) b4i.bend -o $@

check:
	$(BEND) PROOF.bend

test:
	../test-b4-bend

clean:
	rm -f b4i fleet-test
