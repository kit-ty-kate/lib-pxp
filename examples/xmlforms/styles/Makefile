.PHONY: all
all:

.PHONY: clean
clean:

.PHONY: CLEAN
CLEAN: clean

.PHONY: distclean
distclean: clean
	rm -f *~

.PHONY: symlinks
symlinks:
	for x in *-style.xml; do ln -s ../xmlforms $${x%-style.xml} || true; done
