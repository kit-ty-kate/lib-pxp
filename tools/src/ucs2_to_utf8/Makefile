#(******************************************************)
#(*    Claudio Sacerdoti Coen <sacerdot@cs.unibo.it>   *)
#(*                   14/05/2000                       *)
#(******************************************************)

# [14-Jun-2001] Modified by Gerd Stolpmann

TOP_DIR = ../../..
include $(TOP_DIR)/Makefile.rules

all: ucs2_to_utf8$(EXEC_SUFFIX)
opt: ucs2_to_utf8.opt$(EXEC_SUFFIX)

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
 
ucs2_to_utf8$(EXEC_SUFFIX): $(UCS2_TO_UTF8OBJS)
	$(OCAMLC) -o ucs2_to_utf8$(EXEC_SUFFIX) $(UCS2_TO_UTF8OBJS)

ucs2_to_utf8.opt$(EXEC_SUFFIX): $(UCS2_TO_UTF8OPTOBJS)
	$(OCAMLOPT) -o ucs2_to_utf8.opt$(EXEC_SUFFIX) $(UCS2_TO_UTF8OPTOBJS)

clean:
	rm -f $(CLEAN_LIST) lexer.ml parser.ml parser.mli \
	ucs2_to_utf8$(EXEC_SUFFIX) ucs2_to_utf8.opt$(EXEC_SUFFIX)

CLEAN: clean

distclean: clean

include depend
