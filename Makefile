.PHONY: doc install uninstall clean distclean

all: build

build:
	ocaml setup.ml -build

install:
	ocaml setup.ml -reinstall

uninstall:
	ocaml setup.ml -uninstall

clean:
	ocaml setup.ml -clean

fresh: clean uninstall

distclean: clean
	ocaml setup.ml -distclean



update_yaboons: # Go for the last version
	svn export http\://yaboon.googlecode.com/svn/trunk/PolyComp/PolyComp.ml src/lib/dibrawi_Yaboon_PolyComp.ml

doc_library:
	ocaml setup.ml -doc

doc_wiki:
	mkdir -p website
	dbw wiki doc website/wiki -named-template three_columns_greenish

doc_mixes: doc_wiki
	dbw magic doc/examples/Blog.brtx
	rm -fr website/blog
	mv blog website/
	dbw magic doc/examples/Formulae.brtx html
	dbw magic doc/examples/Formulae.brtx pdf

website: doc_mixes doc_library
	mkdir -p website/api
	cp _build/src/lib/dibrawi_libdoc.docdir/ website/api/
