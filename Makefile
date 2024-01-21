# -*- Makefile -*-

# --------------------------------------------------------------------
.PHONY: all merlin build build-deps run clean

# --------------------------------------------------------------------
all: build

build:
	@dune build
	ln -fs _build/default/main.exe timelock-utils

install:
	@dune install

clean:
	@dune clean

_opam:
	opam switch create . 4.14.1 --no-install
	eval $$(opam env)

build-deps: _opam
	opam install . --deps-only --working-dir -y

build-deps-dev: build-deps
	opam install merlin ocp-indent -y
