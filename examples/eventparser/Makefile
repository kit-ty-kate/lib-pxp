OCAMLPATH=../../src

sample.cma: expr.ml exprlex.ml sample.ml
	ocamlfind ocamlc -a -o sample.cma -package "pxp" \
		expr.mli expr.ml exprlex.ml sample.ml

exprlex.ml: exprlex.mll
	ocamllex exprlex.mll

expr.ml: expr.mly
	ocamlyacc expr.mly


clean:
	rm -rf *.cmo *.cma exprlex.ml expr.ml

CLEAN: clean

distclean: clean
