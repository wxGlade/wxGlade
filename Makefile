DESTDIR=

PACKAGE=python2.3-wxglade

all: debian/wxglade.1

clean:
	find . -name "*.pyc" -exec rm -f {} \;
	find . -name "*~" -exec rm -f {} \;

DB2MAN=/usr/share/sgml/docbook/stylesheet/xsl/nwalsh/manpages/docbook.xsl
XP=xsltproc --nonet

debian/wxglade.1: debian/manpage.xml
	cd debian && $(XP) $(DB2MAN) manpage.xml

install: all install-doc
	cp -a *.py codegen edit_sizers res widgets \
	  $(DESTDIR)/usr/lib/python2.3/site-packages/wxglade
	# fix one executable flag
	chmod a+x $(DESTDIR)/usr/lib/python2.3/site-packages/wxglade/configUI.py
	cp -a icons $(DESTDIR)/usr/share/$(PACKAGE)
	# get rid of .xvpics subdirectories and .cvsignore files
	find $(DESTDIR)/usr/share/$(PACKAGE) -name '.xvpics' -type d -exec rm -r {} \;
	find $(DESTDIR) -name '.cvsignore' -type f -exec rm {} \;
	ln -s /usr/share/$(PACKAGE)/icons \
	  $(DESTDIR)/usr/lib/python2.3/site-packages/wxglade
	install -m 755 wxglade $(DESTDIR)/usr/bin
install-doc: debian/wxglade.1
	gzip -c9 debian/wxglade.1 > $(DESTDIR)/usr/share/man/man1/wxglade.1.gz
	cp -a docs $(DESTDIR)/usr/share/doc/$(PACKAGE)/
