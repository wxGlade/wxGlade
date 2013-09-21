#!/usr/bin/env python
"""
Entry point of wxGlade

@copyright: 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import atexit
import codecs
import locale
import logging
import os
import sys
import gettext
import optparse

import common
import config
import log

t = gettext.translation(domain="wxglade", localedir="locale", fallback=True)
t.install("wxglade")

import errors

def _fix_path(path):
    """\
    Returns an absolute version of path, accroding to the invoking dir of
    wxglade (which can be different from '.' if it is invoked from a shell
    script)
    """
    if not os.path.isabs(path):
        return os.path.join(os.getcwd(), path)
        #getenv('WXGLADE_INVOKING_DIR', '.'), path)
    return path

def parse_command_line():
    """\
    Parse command line
    """
    # list of all available languages
    # don't load code generators at this point!!
    languages = ['C++', 'XRC', 'lisp', 'perl', 'python']

    # inject 
    optparse.OptionParser.format_description = lambda self, formatter: self.description

    parser = optparse.OptionParser(
        add_help_option=False,
        usage=_("""\
Usage: wxglade <WXG File>             start the wxGlade GUI
 or:   wxglade <Options> <WXG File>   generate code from command line
 or:   wxglade --version              show programs version number and exit
 or:   wxglade -h|--help              show this help message and exit"""),
        version=_("""\
wxGlade version %s
Copyright (C) 2007-2012 Alberto Griggio
License MIT: The MIT License
             <http://www.opensource.org/licenses/mit-license.php>""") % common.version
        )
    parser.add_option(
        '-h',
        '--help',
        dest='help',
        action='store_true',
        help=_('show this help message and exit'),
        )
    parser.add_option(
        "-g",
        "--generate-code",
        type="choice",
        choices=languages,
        metavar="LANG",
        dest="language",
        help=_("(required) output language, valid languages are: %s") % ", ".join(languages)
        )
    parser.add_option(
        "-o",
        "--output",
        metavar="PATH",
        dest="output",
        help=_("(optional) output file in single-file mode or output directory in multi-file mode"),
        )

    (options, args) = parser.parse_args()

    # print epilog because OptionParser.epilog isn't available to Python 2.3
    if options.help:
        parser.print_help()
        print _("""
Example: Generate Python code out of myapp.wxg

   wxglade -o temp -g python myapp.wxg

Report bugs to:    <wxglade-general@lists.sourceforge.net> or at
                   <http://sourceforge.net/projects/wxglade/>
wxGlade home page: <http://wxglade.sourceforge.net/>""")
        sys.exit()

    # make absolute path
    if len(args) == 1:
        options.filename = _fix_path(args[0])
    else:
        options.filename = None

    # check parameters
    #  - language
    #     - one file            -> cmdline code generation
    #     - no / > one files    -> usage
    #  - no language            -> start gui
    if options.language:
        if len(args) == 1:
            options.start_gui = False
        elif len(args) == 0:
            logging.error(_("No wxg file given!\n"))
            parser.print_help()
            sys.exit(1)
        else:
            logging.error(_("Too many wxg files given!\n"))
            parser.print_help()
            sys.exit(1)
    else:
        options.start_gui = True

    return options


def command_line_code_generation(filename, language, out_path=None):
    """\
    Starts a code generator without starting the GUI.

    @param filename: Name of wxg file to generate code from
    @type filename:  String
    @param language: Code generator language
    @type language:  String
    @param out_path: output file / output directory
    @type out_path:  String
    """
    from xml_parse import CodeWriter
    if not common.code_writers.has_key(language):
        logging.error(_('No writer for language "%s" available'), language)

    writer = common.code_writers[language]
    try:
        CodeWriter(
            writer=writer,
            input=filename,
            out_path=out_path,
            )
    except (errors.WxgOutputDirectoryNotExist,
            errors.WxgOutputDirectoryNotWritable,
            errors.WxgOutputPathIsDirectory,
            ), inst:
        logging.error(inst)
        sys.exit(1)
    except Exception:
        logging.error(
            _("An exception occurred while generating the code for the application.\n"
              "If you think this is a wxGlade bug, please report it.")
             )
        logging.exception(_('Internal Error'))
        sys.exit(1)
    sys.exit(0)


def determine_wxglade_path():
    """\
    @return: wxGlade application directory
    """
    # use directory of the exe in case of frozen packages e.g.
    # PyInstaller or py2exe
    if hasattr(sys, 'frozen'):
        return os.path.dirname(sys.argv[0])

    root = __file__
    if os.path.islink(root):
        root = os.path.realpath(root)
    return os.path.dirname(os.path.abspath(root))


def init_stage1():
    """\
    Initialise paths for wxGlade (first stage)

    Path initialisation is splitted because the test suite doesn't work
    with proper initialised paths.
    
    Initialise locale settings too. The determinated system locale will be
    stored in L{config.encoding}.
    """
    # prepend the widgets dir to the
    wxglade_path = determine_wxglade_path()
    
    # set the program's paths
    common.wxglade_path   = wxglade_path

    # static paths
    common.docs_path      = os.path.join(wxglade_path, 'docs')
    common.icons_path     = os.path.join(wxglade_path, 'icons')
    common.widgets_path   = os.path.join(wxglade_path, 'widgets')
    common.templates_path = os.path.join(wxglade_path, 'templates')
    common.tutorial_file  = os.path.join(common.docs_path, 'html', 'index.html')
    
    # initialise own logging extensions
    # TODO use an platform specific place to store the log file
    log.init(
        filename='%s/wxglade.log' % wxglade_path,
        encoding='utf-8',
        level='INFO',
        )
    atexit.register(log.deinit)
    
    # initialise localization
    encoding = None
    try:
        locale.setlocale(locale.LC_ALL, '')
    except locale.Error:
        # ignore problems by fallback to ascii
        logging.warning(
            _('Setting locale failed. Use "ascii" instead')
            )
        encoding = 'ascii'

    # try to query character encoding used in the selected locale
    if not encoding:
        try:
            encoding = locale.nl_langinfo(locale.CODESET)
        except AttributeError, e:
            logging.warning(
                _('locale.nl_langinfo(locale.CODESET) failed: %s') ,
                str(e)
                )
                
            # try getdefaultlocale, it used environment variables
            try:
                encoding = locale.getdefaultlocale()[1]
            except ValueError:
                encoding = config.default_encoding
            
    # On Mac OS X encoding may None or '' somehow
    if not encoding:
        encoding = config.default_encoding
        logging.warning(
            _('Empty encoding. Use "%s" instead'), encoding
            )

    # check if a codec for the encoding exists
    try:
        codecs.lookup(encoding)
    except LookupError:
        logging.warning(
            _('No codec for encoding "%s" found. Use "ascii" instead'),
            encoding
            )
        encoding = 'ascii'

    # print versions 
    logging.info(
        _("Starting wxGlade version %s on Python %s"),
        common.version,
        common.py_version,
        )

    # show current locale
    loc_langcode, loc_encoding = locale.getlocale()
    logging.info(_('Current locale settings are:'))
    logging.info(_('  Language code: %s'), loc_langcode)
    logging.info(_('  Encoding: %s'), loc_encoding)

    # store determinated encoding
    config.encoding = encoding.upper()

    # search files credits.txt and license.txt at different locations
    # - <wxglade_path>/docs   for linux packages
    # - <wxglade_path>   at Windows or started from source directory
    # - <wxglade_path>/./../../../share/doc/wxglade/   for local installations
    # BTW: <wxglade_path> is something like /.../lib/python2.7/site-packages/wxglade
    common.credits_file = None
    common.license_file = None
    for searchdir in [
        common.wxglade_path,
        common.docs_path,
        os.path.join(common.wxglade_path, '../../../../share/doc/wxglade'),
        ]:
        searchdir = os.path.normpath(searchdir) 
        credits_file = os.path.join(searchdir, 'credits.txt')
        license_file = os.path.join(searchdir, 'license.txt')
        if os.path.exists(credits_file):
            common.credits_file = credits_file
        if os.path.exists(license_file):
            common.license_file = license_file
    if not common.credits_file:
        logging.error(_('Credits file "credits.txt" not found!'))
    if not common.license_file:
        logging.error(_('License file "license.txt" not found!'))

    # print used paths
    logging.info(_('Base directory:             %s'), common.wxglade_path)
    logging.info(_('Documentation directory:    %s'), common.docs_path)
    logging.info(_('Icons directory:            %s'), common.icons_path)
    logging.info(_('Build-in widgets directory: %s'), common.widgets_path)
    logging.info(_('Template directory:         %s'), common.templates_path)
    logging.info(_('Credits file:               %s'), common.credits_file)
    logging.info(_('License file:               %s'), common.license_file)
    logging.info(_('Tutorial file:              %s'), common.tutorial_file)

    # adapt application search path
    sys.path = [common.wxglade_path, common.widgets_path] + sys.path


def init_stage2(use_gui):
    """\
    Initialise the remaining (non-path) parts of wxGlade (second stage)

    @param use_gui: Starting wxGlade GUI
    @type use_gui:  Boolean
    """
    common.use_gui = use_gui
    if use_gui:
        # import proper wx-module using wxversion
        if not hasattr(sys, "frozen") and 'wx' not in sys.modules:
            try:
                import wxversion
                wxversion.ensureMinimal('2.6')
            except ImportError:
                logging.error(
                    _('Please install missing python module "wxversion".'))
                sys.exit(1)

        try:
            import wx
        except ImportError:
            logging.error(
                _('Please install missing python module "wxPython".')
                )
            sys.exit(1)

        # store current platform (None is default)
        common.platform = wx.Platform

        # codewrites, widgets and sizers are loaded in class main.wxGladeFrame
    else:
        # use_gui has to be set before importing config
        config.init_preferences()
        common.load_code_writers()
        common.load_widgets()
        common.load_sizers()


def run_main():
    """\
    This main procedure is started by calling either wxglade.py or
    wxglade.pyw on windows.
    """
    # check command line parameters first
    options = parse_command_line()

    # initialise wxGlade (first stage)
    init_stage1()

    # print versions 
    logging.info(_("Starting wxGlade version %s on Python %s"),
        common.version,
        common.py_version,
        )

    # initialise wxGlade (second stage)
    init_stage2(options.start_gui)

    if options.start_gui:
        # late import of main (imported wx) for using wxversion  in
        # init_stage2()
        import main
        main.main(options.filename)
    else:
        command_line_code_generation(
            filename=options.filename,
            language=options.language,
            out_path=options.output,
            )

if __name__ == "__main__":
    run_main()
