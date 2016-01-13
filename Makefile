
LIB = src/polynomial.cma src/polynomial.cmxa
OPTS = -use-ocamlfind

all:
	ocamlbuild $(OPTS) $(LIB)
