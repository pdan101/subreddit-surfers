.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec --instrument-with bisect_ppx test/main.exe

final_demo:
	OCAMLRUNPARAM=b dune exec demo/demo.exe

bisect: clean test
	bisect-ppx-report html --theme=light

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
