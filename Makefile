.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

demo1:
	OCAMLRUNPARAM=b dune exec demo1/demo1.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f 3110-final.zip
	zip -r 3110-final.zip . -x@exclude.lst

clean:
	dune clean
	rm -f 3110-final.zip

doc:
	dune build @doc
