INTERACTION=nonstopmode

.PHONY: illustraties

all:
	make illustraties
#	rm verslag.pdf
	make verslag.pdf
	make clean
%.pdf: %.tex
	bash makepdf.sh $(INTERACTION) $<

%.ps: %.tex
	bash makepdf.sh $(INTERACTION) $<

clean:
	rm -f *.aux *.log *.bbl *.bak *.blg *.toc *.out *.glg *.glo *.gls *.ist *~ *.*~* *.backup *.synctex.gz *.dvi *.loa *.lof *.lot *.tdo
ps:
	make illustraties
	#rm verslag.pdf
	make verslag.ps
	make clean
