all: main

main: Main.ml
	ocamlbuild Main.byte

clean:
	rm -rf _build *.byte