default: all

all: _build/process_graph.native _build/test.native

test: _build/test.native
	$<

clean:
	ocamlbuild -clean

.PHONY: default all clean

_build/%.native: %.ml $(wildcard *.ml)
	ocamlbuild -use-ocamlfind -no-links $*.native
