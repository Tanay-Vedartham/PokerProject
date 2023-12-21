.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec interface/main.exe

clean:
	dune clean

zip:
	rm -f Poker.zip
	zip -r Poker.zip . -x@exclude.lst

doc:
	dune build @doc