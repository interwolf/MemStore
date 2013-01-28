ifeq ($(MIRAGE_OS),xen)
FLAGS=--enable-xen
else
FLAGS=--disable-xen
endif

all: 
	ocaml setup.ml -configure $(FLAGS)
	ocaml setup.ml -build

.PHONY:clean
clean:
	ocamlbuild -clean
	rm -f main.ml
