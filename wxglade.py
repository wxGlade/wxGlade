#!/usr/bin/env python2
"""
Entry point of wxGlade

@copyright: 2002-2007 Alberto Griggio
@copyright: 2013-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import atexit
import codecs
import locale
import logging
import os
import sys
import gettext
import optparse

# Use a NullWriter with Unicode support (encoding attribute) to catch and
# drop all output in PyInstaller environment (standalone Edition)
#
# Fix for PyInstaller bug #1324
# https://github.com/pyinstaller/pyinstaller/issues/1324
if hasattr(sys, 'frozen') and not hasattr(sys.stderr, 'encoding'):
    class NullWriter(object):
        softspace = 0
        encoding = 'UTF-8'

        def write(*args):
            pass

        def flush(*args):
            pass

    sys.stdout = NullWriter()
    sys.stderr = NullWriter()

t = gettext.translation(domain="wxglade", localedir="locale", fallback=True)
t.install("wxglade")

# avoid problems with "_" in __builtins__ being replaced:
def my_displayhook(value):
    if value is not None:
        print(repr(value))

sys.displayhook = my_displayhook


import common
import config
import compat
import log
import errors


def parse_command_line():
    "Parse command line"
    # list of all available languages
    # don't load code generators at this point!!
    languages = ['C++', 'XRC', 'lisp', 'perl', 'python']

    # inject
    optparse.OptionParser.format_description = \
        lambda self, formatter: self.description

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
Copyright (C) 2011-2016 Carsten Grohmann
License MIT: The MIT License
             <http://www.opensource.org/licenses/mit-license.php>""") %
        config.get_version()
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
        help=_("(required) output language, valid languages are: %s") %
             ", ".join(languages)
        )
    parser.add_option(
        "-o",
        "--output",
        metavar="PATH",
        dest="output",
        help=_("(optional) output file in single-file mode or output "
               "directory in multi-file mode"),
        )

    (options, args) = parser.parse_args()

    # print epilog because OptionParser.epilog isn't available to Python 2.3
    if options.help:
        parser.print_help()
        print( _("""
Example: Generate Python code out of myapp.wxg

   wxglade -o output.py -g python myapp.wxg

Report bugs to:    <wxglade-general@lists.sourceforge.net> or at
                   <https://sourceforge.net/projects/wxglade/>
wxGlade home page: <http://wxglade.sourceforge.net/>""") )
        sys.exit()

    # Make an absolute version of path.
    # According to the invoking dir of wxGlade (which can be different
    # from '.' if it is invoked from a shell script).
    if len(args) == 1:
        filename = args[0]
        if not os.path.isabs(filename):
            filename = os.path.join(os.getcwd(), filename)
        filename = os.path.normpath(os.path.expanduser(filename))
        options.filename = filename
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
            msg = _("No wxg file given.\n")
            logging.error(msg)
            parser.print_help()
            sys.exit(msg)
        else:
            msg = _("Too many wxg files given.\n")
            logging.error(msg)
            parser.print_help()
            sys.exit(msg)
    else:
        options.start_gui = True

    # check output path
    if options.output:
        options.output = os.path.normpath(os.path.expanduser(options.output))

    return options


def command_line_code_generation(filename, language, out_path=None):
    """Starts a code generator without starting the GUI.

    filename: Name of wxg file to generate code from
    language: Code generator language
    out_path: output file / output directory"""
    from xml_parse import CodeWriter

    try:
        if language not in common.code_writers:
            raise errors.WxgMissingCodeWriter(language)

        writer = common.code_writers[language]
        CodeWriter( writer=writer, input=filename, out_path=out_path )
    except errors.WxgBaseException as inst:
        logging.error(inst)
        sys.exit(inst)
    except Exception:
        logging.error( _("An exception occurred while generating the code for the application.\n"
                         "If you think this is a wxGlade bug, please report it.") )
        logging.exception(_('Internal Error'))
        sys.exit(1)
    sys.exit(0)


def init_stage1():
    """\
    Initialise paths for wxGlade (first stage)

    Initialisation is split because the test suite doesn't work with proper
    initialised paths.

    Initialise locale settings too. The determined system locale will be
    stored in L{config.encoding}.
    """
    config.version = config.get_version()
    common.init_paths()

    # initialise own logging extensions
    log.init(filename=config.log_file, encoding='utf-8', level='INFO')
    atexit.register(log.deinit)

    # print versions
    logging.info( _('Starting wxGlade version "%s" on Python %s'), config.version, config.py_version )

    # print used paths
    logging.info(_('Base directory:             %s'), config.wxglade_path)
    logging.info(_('Documentation directory:    %s'), config.docs_path)
    logging.info(_('Icons directory:            %s'), config.icons_path)
    logging.info(_('Build-in widgets directory: %s'), config.widgets_path)
    logging.info(_('Template directory:         %s'), config.templates_path)
    logging.info(_('Credits file:               %s'), config.credits_file)
    logging.info(_('License file:               %s'), config.license_file)
    logging.info(_('Manual file:                %s'), config.manual_file)
    logging.info(_('Tutorial file:              %s'), config.tutorial_file)
    logging.info(_('Home directory:             %s'), config.home_path)
    logging.info(_('Application data directory: %s'), config.appdata_path)
    logging.info(_('Configuration file:         %s'), config.rc_file)
    logging.info(_('History file:               %s'), config.history_file)
    logging.info(_('Log file:                   %s'), config.log_file)

    # adapt application search path
    sys.path.insert(0, config.wxglade_path)
    sys.path.insert(1, config.widgets_path)


def init_localization():
    "Initialise localization"
    encoding = None
    try:
        locale.setlocale(locale.LC_ALL, '')
    except locale.Error as e:
        # ignore problems by fallback to ascii
        logging.warning(_('Setting locale failed: %s'), str(e))
        logging.warning(_('Use "ascii" locale instead.'))
        encoding = 'ascii'

    # try to query character encoding used in the selected locale
    if not encoding and hasattr(locale, 'nl_langinfo'):
        try:
            encoding = locale.nl_langinfo(locale.CODESET)
        except AttributeError as e:
            logging.warning( _('locale.nl_langinfo(locale.CODESET) failed: %s'), str(e) )

    # try getdefaultlocale, it used environment variables
    if not encoding:
        try:
            encoding = locale.getdefaultlocale()[1]
        except ValueError:
            encoding = config.default_encoding

    # On Mac OS X encoding may None or '' somehow
    if not encoding:
        encoding = config.default_encoding
        logging.warning(_('Empty encoding. Use "%s" instead'), encoding)

    # check if a codec for the encoding exists
    try:
        codecs.lookup(encoding)
    except LookupError:
        logging.warning( _('No codec for encoding "%s" found. Use "ascii" instead'), encoding)
        encoding = 'ascii'

    # store determined encoding and show current locale
    config.encoding = encoding.upper()
    loc_langcode, loc_encoding = locale.getlocale()
    logging.info(_('Current locale settings are:'))
    logging.info(_('  Language code: %s'), loc_langcode)
    logging.info(_('  Encoding: %s'), loc_encoding)
    logging.info(_('  Filesystem encoding: %s'), sys.getfilesystemencoding())


def init_stage2(use_gui):
    """\
    Initialise the remaining (non-path) parts of wxGlade (second stage)

    @param use_gui: Starting wxGlade GUI
    @type use_gui:  bool
    """
    config.use_gui = use_gui
    if use_gui:
        # import proper wx-module using wxversion, which is only available in Classic
        if compat.IS_CLASSIC:
            if not hasattr(sys, "frozen") and 'wx' not in sys.modules:
                try:
                    import wxversion
                    wxversion.ensureMinimal('2.8')
                except ImportError:
                    msg = _('Please install missing Python module "wxversion".')
                    logging.error(msg)
                    sys.exit(msg)

        try:
            import wx
        except ImportError:
            msg = _('Please install missing Python module "wxPython".')
            logging.error(msg)
            sys.exit(msg)

        # store current version and platform ('not_set' is default)
        config.platform = wx.Platform
        config.wx_version = wx.__version__

        # codewrites, widgets and sizers are loaded in class main.wxGladeFrame
    else:
        # use_gui has to be set before importing config
        common.init_preferences()
        if config.preferences.log_debug_info:
            log.setDebugLevel()
        common.init_codegen()


def run_main():
    "This main procedure is started by calling either wxglade.py or wxglade.pyw on windows."
    # check command line parameters first
    options = parse_command_line()

    # initialise wxGlade (first stage and second stage)
    init_stage1()
    init_localization()
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
