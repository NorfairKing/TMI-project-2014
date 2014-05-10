INTERACTION=nonstopmode

.PHONY: illustraties

all:
	make illustraties
	rm verslag.pdf
	make verslag.pdf
	make clean
%.pdf: %.tex
	bash makepdf.sh $(INTERACTION) $<
clean:
	rm -f *.aux *.log *.bbl *.bak *.blg *.toc *.out *.glg *.glo *.gls *.ist *~ *.*~* *.backup *.synctex.gz *.dvi
