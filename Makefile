LATEX ?= latex
PDFLATEX ?= pdflatex
RM ?= rm -rf

LATEXFLAGS := -interaction=batchmode

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
	$(HC) $^

clean:
	$(RM) *.log *.aux *.pdf *.dvi haskell/*.hi haskell/*.o
