play:
	dune build && OCAMLRUNPARAM=b dune exec bin/main.exe
	
test:
	OCAMLRUNPARAM=b dune exec test/main.exe

doc:
	cd .. && dune build @doc

build:
	dune build

zip:
	rm -f camlify.zip
	zip -r camlify.zip . -x@exclude.lst

.PHONY: all test clean
