# Makefile for wxGlade
#
# License: MIT (see LICENSE.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

DESTDIR           =

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

BASE_DIR          = .
BIN_FILES         = wxglade
BUILD_DIR         = $(BASE_DIR)/build
BDIST_DIR         = $(BASE_DIR)/bdist
DOC_DIR           = $(BASE_DIR)/docs
DIST_DIR          = $(BASE_DIR)/dist
EGG_DIR           = $(BASE_DIR)/wxGlade.egg-info
CHECK_FILES       = $(filter-out $(BASE_DIR)/test.py, $(wildcard $(BASE_DIR)/*.py)) \
                    $(filter-out tests/%.py, $(shell find $(SOURCE_DIRS) -name "*.py"))
EPYDOC_BIN        = epydoc
EPYDOC_CONFIG     = $(BASE_DIR)/epydoc.conf
EPYDOC_OPTS       = --config $(EPYDOC_CONFIG)
APIDOC_DIR        = $(BASE_DIR)/docs/apidocs
SOURCE_DIRS       = codegen wcodegen edit_sizers widgets install tests
SOURCE_FILES      = $(wildcard $(BASE_DIR)/*.py) $(shell find $(SOURCE_DIRS) -name "*.py")
TEST_BIN          = $(BASE_DIR)/test.py
PYLINT_BIN        = pylint2
PYLINT_OPTS       = --rcfile ./pylintrc
PYLINT_PATH       = "$(BASE_DIR):$(BASE_DIR)/widgets:$(BASE_DIR)/codegen"
DIGGER_BIN        = clonedigger
DIGGER_OUTFILE    = clonedigger.html
DIGGER_OPTS       = --language python --output $(DIGGER_OUTFILE)
PYTHON_BIN        = python2
MANPAGE_XSL       = /usr/share/xml/docbook/xsl-stylesheets-1.79.1/manpages/docbook.xsl
XP                = xsltproc --nonet

MANUAL_HTML_DIR   = $(DOC_DIR)/html
MANUAL_PDF_DIR    = $(DOC_DIR)/pdf
MANUAL_SRC_DIR    = $(DOC_DIR)/src
MANUAL_XML        = $(MANUAL_SRC_DIR)/manual.xml
MANUAL_PICS       = $(wildcard $($(MANUAL_SRC_DIR)/*.png))
MANUAL_PDF        = $(MANUAL_PDF_DIR)/manual.pdf
MANUAL_PDF_XSL    = $(MANUAL_SRC_DIR)/pdf.xsl
MANUAL_HTML       = $(MANUAL_HTML_DIR)/index.html
MANUAL_HTML_XSL   = $(MANUAL_SRC_DIR)/html.xsl

HELP= @grep -B1 '^[a-zA-Z\-]*:' Makefile |\
         awk 'function p(h,t){printf"%-12s=%s\n",h,t;};\
         /\#+/{t=$$0;};\
         /:/{gsub(":.*","");h=$$0};\
         /^--/{p(h,t);t=h="";};\
         END{p(h,t)}' |\
         sed -n 's/=.*\#+/:/gp'

.PHONY: help clean distclean compile apidoc pylint permissions man doc \
        pdf html doc-clean release rel-binary rel-source install \
        maintainer-clean test

# Rule to compile a single Python file
%.pyc: %.py
	@echo "Compile $< ..."
	@$(PYTHON_BIN) -c "import py_compile; py_compile.compile('$<')"

#+ Show this text
help:
	$(HELP)

#+ Run all test package
test: test-nongui test-gui test-compile

#+ Run all non-GUI test package
test-nongui:
	@$(TEST_BIN)

#+ Run all GUI test package
test-gui:
	@$(TEST_BIN) --gui

#+ Run test to compile C++ files
test-compile:
	@$(TEST_BIN) --compile

#+ Clean python compiler files and automatic generated documentation
clean:
	@echo "Remove all automatically generated files ..."
	@find $(BASE_DIR) -depth -type f -name "*.pyc" -exec rm -f {} \;
	@find $(BASE_DIR) -depth -type f -name "*.pyo" -exec rm -f {} \;
	@if [ -e $(BUILD_DIR) ]; then rm -rf $(BUILD_DIR); fi
	@if [ -e $(BDIST_DIR) ]; then rm -rf $(BDIST_DIR); fi
	@if [ -e $(DIST_DIR) ]; then rm -rf $(DIST_DIR); fi
	@if [ -e $(APIDOC_DIR) ]; then rm -rf $(APIDOC_DIR); fi
	@if [ -e $(EGG_DIR) ]; then rm -rf $(EGG_DIR); fi
	@find $(BASE_DIR) -depth -type f -name "*.orig" -exec rm -f {} \;
	@find $(BASE_DIR) -depth -type f -name "*~" -exec rm -f {} \;
	@find $(BASE_DIR) -depth -type f -name "#~wxg.autosave*" -exec rm -f {} \;
	@$(RM) logdict*.log
	@$(RM) warnwxglade.txt
	@$(RM) MANIFEST
	@$(RM) $(DIGGER_OUTFILE)

#+ Remove all automatically generated and Mercurial repository data
distclean: clean
	@echo "Remove Mercurial repository data (.hg*) ..."
	@$(RM) .hgtags .hgignore version.py

#+ Remove almost everything that can bee reconstructed with this Makefile
maintainer-clean: distclean doc-clean
	@echo "This command is intended for maintainers to use; it"
	@echo 'deletes files that may need special tools to rebuild."

#+ Compile all python files
compile: $(BYTECODE_FILES)

# Create the documentation
$(APIDOC_DIR)/index.html: $(SOURCE_FILES) $(EPYDOC_CONFIG)
	@echo "Create documentation from source ..."
	@$(EPYDOC_BIN) $(EPYDOC_OPTS)

#+ Create documentation from source files
apidoc: $(APIDOC_DIR)/index.html

#+ Check all source files for logical errors using pylint
pylint:
	PYTHONPATH=$(PYLINT_PATH) $(PYLINT_BIN) $(PYLINT_OPTS) $(CHECK_FILES) || /bin/true

#+ Check all source files for dublicates
digger:
	$(DIGGER_BIN) $(DIGGER_OPTS) $(CHECK_FILES)

#+ Set proper permissions for all files and directories
permissions:
	@echo "Set permissions for all files ..."
	@find $(BASE_DIR) -type d -exec chmod 755 {} \;
	@find $(BASE_DIR) -type f -exec chmod 644 {} \;
	@chmod 755 $(BIN_FILES)

# Create the manpage
$(DOC_DIR)/man/wxglade.1: $(DOC_DIR)/man/manpage.xml
	$(XP) --output $@ $(MANPAGE_XSL) $<

#+ Create manpage from source files
man: $(DOC_DIR)/man/wxglade.1

#+ Create all documentation from source files
doc: pdf html man

# Create PDF documentation
$(MANUAL_PDF): $(MANUAL_XML) $(MANUAL_PICS)
	dblatex -p $(MANUAL_PDF_XSL) -o $(MANUAL_PDF_DIR)/manual.pdf $(MANUAL_XML)

#+ Create PDF document based on manual xml file
pdf: $(MANUAL_PDF)

# Create HTML documentation
$(MANUAL_HTML): $(MANUAL_XML) $(MANUAL_PICS)
	$(INSTALL_DATA) -t $(MANUAL_HTML_DIR) $(MANUAL_SRC_DIR)/*.png
	xmlto -m $(MANUAL_HTML_XSL) -o $(MANUAL_HTML_DIR) html $(MANUAL_XML)
	$(RM) $(MANUAL_HTML_DIR)/manual.proc

#+ Create a set of HTML documents based on manual xml file
html: $(MANUAL_HTML)

#+ Cleanup all automatically generated manuals (if you really know what you do)
doc-clean:
	$(RM) $(MANUAL_HTML_DIR)/*.html
	$(RM) $(MANUAL_HTML_DIR)/*.png
	$(RM) $(MANUAL_PDF)

#+ Create official release packages
release: rel-source rel-binary

#+ Create Unix binary packages
rel-binary: clean doc
	@echo "Creating Unix release packages ..."
	@$(RM) MANIFEST
	$(PYTHON_BIN) setup.py bdist --format=zip

	@$(RM) MANIFEST

#+ Create Unix source release packages
rel-source: clean doc
	@echo "Creating source packages ..."
	@$(RM) MANIFEST
	$(PYTHON_BIN) setup.py sdist --formats=gztar,zip

	@$(RM) MANIFEST

#+ Install wxGlade locally at $(prefix)
install: doc setup.py
	@echo "Install wxGlade locally at $(prefix) ..."
	$(PYTHON_BIN) setup.py install --prefix $(prefix)
	gzip -9 $(man1dir)/wxglade.1
