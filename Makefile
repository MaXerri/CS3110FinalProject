.PHONY: test

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

doc:
	dune clean
	dune build @doc

opendoc: doc
	@bash opendoc.sh

cloc:
	cloc --by-file --include-lang=OCaml --exclude-dir=_build .


zip:
	rm -f finalproject.zip
	zip -r finalproject.zip . -x@exclude.lst

validate:
	cloc --by-file --include-lang=OCaml --exclude-dir=_build .
	OCAMLRUNPARAM=b dune exec test/main.exe
