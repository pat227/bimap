PROJECT=bimap
libdir=src/lib/
maindir=src/main/
builddir=build/
testdir=src/test/

ifndef PREFIX
  PREFIX = $(shell dirname $$(dirname $$(which ocamlfind)))
  export PREFIX
endif

ifndef BINDIR
  BINDIR = $(PREFIX)/bin
  export BINDIR
endif

.PHONY: all clean lib #install uninstall tests

all: lib
clean:
	rm -rvf build

lib: $(libdir)bimap.ml $(libdir)bimap_single_module.ml $(libdir)bimap_single_module.mli $(libdir)bimap_multi_module.ml $(libdir)bimap_multi_module.mli $(libdir)bimap_single_class.ml $(libdir)bimap_single_class.mli
	ocamlbuild -classic-display -use-ocamlfind -j 1 -tag thread -tag principal -r -build-dir build -I src/lib -I src/main -I build/src/lib src/lib/bimap.a
	ocamlbuild -classic-display -use-ocamlfind -j 1 -tag thread -tag principal -r -build-dir build -I src/lib -I src/main -I build/src/lib src/lib/bimap.cma
	ocamlbuild -classic-display -use-ocamlfind -j 1 -tag thread -tag principal -r -build-dir build -I src/lib -I src/main -I build/src/lib src/lib/bimap.cmxa

tests: $(testdir)bimap_tests.ml #lib
	ocamlbuild -classic-display -use-ocamlfind -j 1 -tag thread -tag principal -r -package 'oUnit' -build-dir build -I src/lib src/test/bimap_tests.byte

install: lib
	ocamlfind install $(PROJECT) ./$(builddir)$(libdir)* META

uninstall:
	ocamlfind remove $(PROJECT)
