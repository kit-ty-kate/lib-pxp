#(******************************************************)
#(*    Claudio Sacerdoti Coen <sacerdot@cs.unibo.it>   *)
#(*                   14/05/2000                       *)
#(******************************************************)

OCAMLC = ocamlc 
OCAMLOPT = ocamlopt 
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc

all: ucs2_to_utf8
opt: ucs2_to_utf8.opt

DEPOBJS = ucs2_to_utf8.ml lexer.ml parser.ml parser.mli types.ml

UCS2_TO_UTF8OBJS = types.cmo lexer.cmo parser.cmo ucs2_to_utf8.cmo
UCS2_TO_UTF8OPTOBJS = types.cmx lexer.cmx parser.cmx ucs2_to_utf8.cmx

lexer.ml:
	$(OCAMLLEX) lexer.mll

parser.ml:
	$(OCAMLYACC) parser.mly

parser.mli:
	$(OCAMLYACC) parser.mly

depend: lexer.ml parser.ml parser.mli
	$(OCAMLDEP) $(DEPOBJS) > depend
 
ucs2_to_utf8: $(UCS2_TO_UTF8OBJS)
	$(OCAMLC) -o ucs2_to_utf8 $(UCS2_TO_UTF8OBJS)

ucs2_to_utf8.opt: $(UCS2_TO_UTF8OPTOBJS)
	$(OCAMLOPT) -o ucs2_to_utf8.opt $(UCS2_TO_UTF8OPTOBJS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx
.ml.cmo:
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

clean:
	rm -f *.cm[iox] *.o lexer.ml parser.ml parser.mli \
	ucs2_to_utf8 ucs2_to_utf8.opt

include depend
