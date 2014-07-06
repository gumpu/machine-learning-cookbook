#

svg_files = $(wildcard *.svg)
png_files = $(svg_files:.svg=.png)
pdf_files = $(svg_files:.svg=.pdf)

%.png : %.svg
	inkscape --export-png=$@ $< --export-width=800

%.pdf : %.svg
	inkscape --export-pdf=$@ $<

all : $(pdf_files)
	echo "done"

clean :
	-rm -f $(pdf_files)
	-rm -f $(png_files)
