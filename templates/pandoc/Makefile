PANDOC_FILES=$(shell find sections/ -name "*.md")
TEX_FILES=$(PANDOC_FILES:.md=.tex)
TRASH=*.log *.aux *.pdf sections/*.tex *.tex *.bbl *.blg $(full_log) $(prv_log) $(crt_log)
MAIN=main
SORTED_TEX_FILES=$(shell echo $(TEX_FILES) | sort)

TEXC ?= pdflatex
BIBC ?= bibtex

all_sections=$(shell echo $(SORTED_TEX_FILES) | tr ' ' '\n' | sed 's/\(.*\)/\\\\input{\1}/g')

%.tex: %.md
	pandoc $< -o $@

compile: $(TEX_FILES)
	cp templates/main.tex .
	sed -i 's^@@@SECTIONS@@@^$(all_sections)^g' $(MAIN).tex
	$(TEXC) $(MAIN)
	$(BIBC) $(MAIN)
	$(TEXC) $(MAIN)
	$(TEXC) $(MAIN)

clean:
	rm -f $(TRASH)

.PHONY: compile clean
