include MiniLucy/Makefile.conf

SRC_DIR := MiniLucy/src

SRC := \
  ace.ml \
  page.ml \
	minilucy.ml

all: minilucy.js

minilucy.js: minilucy.byte
	js_of_ocaml $^

minilucy.byte: $(SRC_DIR)/minilucy.cma $(SRC)
	ocamlfind ocamlc \
		-package js_of_ocaml \
		-package js_of_ocaml-ppx \
		-package js_of_ocaml-tyxml \
    -I $(SRC_DIR) \
		-linkpkg -o $@ $^

MiniLucy/src/minilucy.cma:
	make -C $(SRC_DIR) minilucy.cma

clean:
	rm -f *.byte minilucy.js

.PHONY:
	all clean
