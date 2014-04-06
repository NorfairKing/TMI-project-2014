INTERACTION=nonstopmode
all:
	make verslag.pdf > /dev/null 2>&1
	make clean
%.pdf: %.tex
	bash makepdf.sh $(INTERACTION) $<
clean:
	rm -f *.aux *.log *.bbl *.bak *.blg *.toc *.out *.glg *.glo *.gls *.ist *~ *.*~* *.backup *.synctex.gz *.dvi
