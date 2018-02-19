PROJECT=bimap
libdir=src/lib/
maindir=src/main/
builddir=build/

ifndef PREFIX
  PREFIX = $(shell dirname $$(dirname $$(which ocamlfind)))
  export PREFIX
endif

ifndef BINDIR
  BINDIR = $(PREFIX)/bin
  export BINDIR
endif

.PHONY: all clean lib install uninstall

all: lib
clean:
	rm -rvf build

lib: $(libdir)bimap.ml $(libdir)bimap.mli 
	ocamlbuild -classic-display -use-ocamlfind -j 1 -tag thread -tag principal -r -package 'core uint mysql ppx_deriving ppx_deriving.eq ppx_deriving.ord ppx_deriving.show fieldslib ppx_fields_conv pcre ppx_sexp_conv ppx_compare yojson ppx_deriving_yojson' -build-dir build -I src/lib -I src/main -I build/src/lib src/lib/bimap.a
	ocamlbuild -classic-display -use-ocamlfind -j 1 -tag thread -tag principal -r -package 'core uint mysql ppx_deriving ppx_deriving.eq ppx_deriving.ord ppx_deriving.show fieldslib ppx_fields_conv pcre ppx_sexp_conv ppx_compare yojson ppx_deriving_yojson' -build-dir build -I src/lib -I src/main -I build/src/lib src/lib/bimap.cma
	ocamlbuild -classic-display -use-ocamlfind -j 1 -tag thread -tag principal -r -package 'core uint mysql ppx_deriving ppx_deriving.eq ppx_deriving.ord ppx_deriving.show fieldslib ppx_fields_conv pcre ppx_sexp_conv ppx_compare yojson ppx_deriving_yojson' -build-dir build -I src/lib -I src/main -I build/src/lib src/lib/bimap.cmo
	ocamlbuild -classic-display -use-ocamlfind -j 1 -tag thread -tag principal -r -package 'core uint mysql ppx_deriving ppx_deriving.eq ppx_deriving.ord ppx_deriving.show fieldslib ppx_fields_conv pcre ppx_sexp_conv ppx_compare yojson ppx_deriving_yojson' -build-dir build -I src/lib -I src/main -I build/src/lib src/lib/bimap.cmx

install: lib
	ocamlfind install $(PROJECT) ./$(builddir)$(libdir)* META

uninstall:
	ocamlfind remove $(PROJECT)
#only makes sense to run this after copying output file into the src/lib dir
#test_output: $(builddir)$(maindir)ocaml_mysql_model.native
#	ocamlbuild -classic-display -use-ocamlfind -j 1 -tag thread -tag principal -r -package 'core uint mysql ppx_deriving ppx_deriving.show fieldslib ppx_fields_conv ppx_sexp_conv ppx_deriving.eq ppx_deriving.ord pcre' -build-dir build -I src/lib -I src/main -I build/src/lib src/lib/scrapings.cma
