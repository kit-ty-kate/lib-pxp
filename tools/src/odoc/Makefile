TOP_DIR = ../../..
include $(TOP_DIR)/Makefile.rules

SRC = chtml.ml
OBJ = $(SRC:.ml=.cmo)
OCAMLC_OPTIONS += -I +ocamldoc

.PHONY: all
all: chtml.cmo

.PHONY: clean
clean:
	rm -f $(CLEAN_LIST)

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
