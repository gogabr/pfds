LATEX ?= latex
PDFLATEX ?= pdflatex
HC ?= ghc

RM := rm -rf
MKDIR := mkdir -p

LATEXFLAGS := -interaction=batchmode
HOUT := out

latex-files := $(wildcard *.tex)
haskell-files := $(wildcard haskell/*.lhs)
main-file := pfds.tex

define run-twice
  $1 && $1
endef

pfds.dvi: $(latex-files) $(haskell-files)
	$(call run-twice,$(LATEX) $(LATEXFLAGS) $(main-file))

pfds.pdf: $(latex-files) $(haskell-files)
	$(call run-twice,$(PDFLATEX) $(LATEXFLAGS) $(main-file))

build-haskell: $(haskell-files)
	$(MKDIR) $(HOUT) && $(HC) -outputdir $(HOUT) $^

clean:
	$(RM) *.log *.aux *.pdf *.dvi haskell/*.hi haskell/*.o $(HOUT)
