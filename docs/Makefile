all: pdf html

BITMAPS=src/main_window.png src/tree_window.png

pdf: src/manual.xml $(BITMAPS)
	xmlto pdf -o pdf src/manual.xml

html: src/manual.xml $(BITMAPS)
	cp src/*.png html/
	xmlto xhtml -o html src/manual.xml

clean:
	rm html/*.html
	rm html/*.png
	rm pdf/manual.pdf
