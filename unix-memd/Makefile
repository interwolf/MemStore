.PHONY: all clean build
all: build
NAME=omemd

#PREFIX ?= /usr/local #for installation only?
#LWT ?= $(shell if ocamlfind query lwt >/dev/null 2>&1; then echo --enable-lwt; fi)

setup.bin: setup.ml
	ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	rm -f setup.cmx setup.cmi setup.o setup.cmo setup.cmt setup.annot

setup.data: setup.bin
	./setup.bin -configure 

build: setup.data setup.bin
	./setup.bin -build

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log setup.bin
