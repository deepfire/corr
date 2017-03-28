V         ?=
VERBOSE   ?= $(V)
NIX_SHELL := nix-shell --max-jobs 8 --cores 8 $(if $(VERBOSE),,--no-build-output)
CORR      := dist/build/corr/corr
HSDEPS    := $(wildcard *.hs)

all: ui

###
###
###
lts-sh:
	$(NIX_SHELL) \
	  shell.lts.nix \
	  --arg youtrack-from-github false

lts-sh-github:
	$(NIX_SHELL) \
	  shell.lts.nix

###
###
###
clean:
	cabal clean
$(CORR): $(HSDEPS)
	cabal build

ui: $(CORR)
	$(CORR) --repo ../gvandra ui

report: $(CORR)
	$(CORR) --repo ../gvandra report --areapath Gvandra --product Gvandra
