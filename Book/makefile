

book.pdf : book.tex book.bib makefile
	make -C Figures
	pdflatex  book
	bibtex    book
	makeindex book

clean :
	make -C Figures clean
	-rm -f book.aux
	-rm -f book.log
	-rm -f book.pdf
	-rm -f book.aux
	-rm -f book.bbl
	-rm -f book.blg
	-rm -f book.idx
	-rm -f book.ilg
	-rm -f book.ind
	-rm -f book.toc
	-rm -f book.lof
	-rm -f book.lot
	-rm -f book.log
	-rm -f book.out
	-rm -f book.pdf
	echo 'Done'
