all: pdf html

pdf: src/manual.xml src/main_window.png
	xmlto -o pdf/wxGlade-User-Reference.pdf pdf src/manual.xml

html: src/manual.xml src/main_window.png
	xmlto -o html html src/manual.xml

clean:
	rm html/*.html
	rm pdf/wxGlade-User-Reference.pdf
