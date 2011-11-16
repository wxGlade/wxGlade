# Makefile for SMM
#
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

DESTDIR           = /tmp

# Makefile defaults
SHELL             = /bin/sh
INSTALL           = install
INSTALL_PROGRAM   = ${INSTALL}
INSTALL_DATA      = ${INSTALL} -m 644
PACKAGE           = python-wxglade
prefix            = $(DESTDIR)/usr/local
exec_prefix       = $(DESTDIR)/usr/local
bindir            = $(exec_prefix)/bin
datarootdir       = $(prefix)/share
datadir           = $(datarootdir)
docdir            = $(datarootdir)/doc/$(PACKAGE)
mandir            = $(datarootdir)/man
man1dir           = $(mandir)/man1
PYVER             = 2.3
PY_SITE_DIR       = $(prefix)/lib/python$(PYVER)/site-packages/wxglade

BASE_DIR          = .
BIN_FILES         = wxglade.py
BUILD_DIR         = $(BASE_DIR)/build
BDIST_DIR         = $(BASE_DIR)/bdist
DOC_DIR           = $(BASE_DIR)/docs
DIST_DIR          = $(BASE_DIR)/dist
CHECK_FILES       = $(wildcard $(BASE_DIR)/*.py) \
                    $(shell find $(SOURCE_DIRS) -name "*.py")
EPYDOC_BIN        = epydoc
EPYDOC_CONFIG     = $(BASE_DIR)/epydoc.conf
EPYDOC_OPTS       = --config
APIDOC_DIR        = $(BASE_DIR)/docs/apidocs
SOURCE_DIRS       = codegen edit_sizers widgets install 
SOURCE_FILES      = $(wildcard $(BASE_DIR)/*.py) $(shell find $(SOURCE_DIRS) -name "*.py")
PYLINT_BIN        = pylint
PYLINT_OPTS       = --additional-builtins=_ --disable=C \
                    '--dummy-variables=_|dummy|event|empty|unused' \
                    --include-ids=y --reports=n
PYTHON_BIN        = python
DB2MAN            = /usr/share/sgml/docbook/stylesheet/xsl/nwalsh/manpages/docbook.xsl
XP                = xsltproc --nonet

HELP= @grep -B1 '^[a-zA-Z\-]*:' Makefile |\
         awk 'function p(h,t){printf"%-12s=%s\n",h,t;};\
         /\#+/{t=$$0;};\
         /:/{gsub(":.*","");h=$$0};\
         /^--/{p(h,t);t=h="";};\
         END{p(h,t)}' |\
         sed -n 's/=.*\#+/:/gp'

# Rule to compile a single Python file
%.pyc: %.py
	@echo "Compile $< ..."
	@$(PYTHON_BIN) -c "import py_compile; py_compile.compile('$<')"

#+ Show this text
help:
	$(HELP)

#+ Clean python compiler files and automatic generated documentation
clean:
	@echo "Remove all automatically generated files ..."
	@find $(BASE_DIR) -depth -type f -name "*.pyc" -exec rm -f {} \;
	@find $(BASE_DIR) -depth -type f -name "*.pyo" -exec rm -f {} \;
	@if [ -e $(BUILD_DIR) ]; then rm -rf $(BUILD_DIR); fi
	@if [ -e $(BDIST_DIR) ]; then rm -rf $(BDIST_DIR); fi
	@if [ -e $(DIST_DIR) ]; then rm -rf $(DIST_DIR); fi
	@if [ -e $(APIDOC_DIR) ]; then rm -rf $(APIDOC_DIR); fi
	@find $(BASE_DIR) -depth -type f -name "*.orig" -exec rm -f {} \;
	@find $(BASE_DIR) -depth -type f -name "*~" -exec rm -f {} \;
	@$(RM) logdict*.log
	@$(RM) warnwxglade.txt
	@$(RM) MANIFEST

#+ Remove all automatically generated and development files
distclean: clean
	@echo "Remove development files ..."
	@$(RM) .hgtags .hgignore

#+ Compile all python files
compile: $(BYTECODE_FILES)

# Create the documentation
$(APIDOC_DIR)/index.html: $(SOURCE_FILES) $(EPYDOC_CONFIG)
	@echo "Create documentation from source ..."
	@$(EPYDOC_BIN) $(EPYDOC_OPTS) $(EPYDOC_CONFIG)

#+ Create documentation from source files
doc: $(APIDOC_DIR)/index.html

#+ Check all source files for logical errors using pylint
pylint:
	$(PYLINT_BIN) $(PYLINT_OPTS) $(CHECK_FILES) || /bin/true

#+ Set proper permissions for all files and directories
permissions:
	@echo "Set permissions for all files ..."
	@find $(BASE_DIR) -type d -exec chmod 755 {} \;
	@find $(BASE_DIR) -type f -exec chmod 644 {} \;
	@chmod 755 $(BIN_FILES)

# Create the manpage
$(DOC_DIR)/man/wxglade.1: $(DOC_DIR)/man/manpage.xml
	$(XP) --output $@ $(DB2MAN) $<

#+ Create manpage from source files
man: $(DOC_DIR)/man/wxglade.1

install-doc: $(DOC_DIR)/man/wxglade.1
	# create directories first
	$(INSTALL) -d $(man1dir) $(docdir)
	# copy files
	$(INSTALL_DATA) $(DOC_DIR)/man/wxglade.1 $(man1dir)
	gzip -9 $(man1dir)/wxglade.1
	cp -a docs $(docdir)/

install: install-doc
	# create directories first
	$(INSTALL) -d $(PY_SITE_DIR) $(bindir) $(datarootdir)/$(PACKAGE)
	# copy files
	cp -a *.py codegen edit_sizers res widgets $(PY_SITE_DIR)
	# fix executable flags
	for f in configUI.py wxglade.py; do chmod 755 $(PY_SITE_DIR)/$$f; done
	for f in edit_widget.py config.py; do chmod 644 $(PY_SITE_DIR)/$$f; done
	cp -a icons $(datarootdir)/$(PACKAGE)
	ln -s $(datarootdir)/$(PACKAGE)/icons $(PY_SITE_DIR)
	$(INSTALL_PROGRAM) wxglade $(bindir)
