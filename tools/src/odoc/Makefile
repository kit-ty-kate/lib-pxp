TOP_DIR = ../../..
include $(TOP_DIR)/Makefile.rules

SRC = chtml.ml
OBJ = $(SRC:.ml=.cmo)
OCAMLC_OPTIONS += -I +ocamldoc
CLEAN_LIST += chtml.ml

PACKAGES = compiler-libs.common

.PHONY: all
all: chtml.cmo

chtml.ml:
	case `ocamlc -version` in \
	  3.*) cp chtml_ocaml3.ml chtml.ml ;; \
	  4.*) cp chtml_ocaml4.ml chtml.ml ;; \
	  *)   echo "Unknown Ocaml version"; exit 1 ;; \
	esac


.PHONY: clean
clean:
	rm -f $(CLEAN_LIST)

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
