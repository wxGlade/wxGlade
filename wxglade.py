#!/usr/bin/env python
"""
Entry point of wxGlade

@copyright: 2002-2007 Alberto Griggio
@copyright: 2013-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import atexit
import codecs
import logging, os, sys, gettext, optparse

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


import common, config, compat, log


def parse_command_line():
    "Parse command line"
    # list of all available languages; don't load code generators at this point!!
    languages = ['C++', 'XRC', 'lisp', 'perl', 'python']

    # inject
    optparse.OptionParser.format_description = lambda self, formatter: self.description

    version = _("wxGlade version %s\n"
                "Copyright (C) 2007-2012 Alberto Griggio\n"
                "Copyright (C) 2011-2016 Carsten Grohmann\n"
                "Copyright (C) 2016-2021 Dietmar Schwertberger\n"
                "License MIT: The MIT License\n"
                "             <http://www.opensource.org/licenses/mit-license.php>") % config.get_version()
    usage = _("Usage: wxglade <WXG File>             start the wxGlade GUI\n"
              " or:   wxglade <Options> <WXG File>   generate code from command line\n"
              " or:   wxglade --version              show programs version number and exit\n"
              " or:   wxglade -h|--help              show this help message and exit")
    parser = optparse.OptionParser( add_help_option=False, version=version, usage=usage )

    parser.add_option('-h', '--help', dest='help', action='store_true', help=_('show this help message and exit'))
    parser.add_option("-g", "--generate-code", type="choice", choices=languages, metavar="LANG", dest="language",
                            help=_("(required) output language, valid languages are: %s") % ", ".join(languages) )
    
    parser.add_option("-o", "--output", metavar="PATH", dest="output",
                            help=_("(optional) output file in single-file mode or output directory in multi-file mode"))

    parser.add_option("-c", "--use-config", dest="rc_file",
                            help=_("use specified wxgladerc config file instead of the default one") )

    options, args = parser.parse_args()

    # print epilog because OptionParser.epilog isn't available to Python 2.3
    if options.help:
        parser.print_help()
        print( _( "Example: Generate Python code out of myapp.wxg\n\n"
                  "   wxglade -o output.py -g python myapp.wxg\n\n"
                  "Report bugs to:    <wxglade-general@lists.sourceforge.net> or at\n"
                  "                   <https://sourceforge.net/projects/wxglade/>\n"
                  "wxGlade home page: <http://wxglade.sourceforge.net/>") )
        sys.exit()

    # Make an absolute version of path.
    # According to the invoking dir of wxGlade (which can be different
    # from '.' if it is invoked from a shell script).
    if len(args) == 1:
        filename = args[0]
        if filename.startswith("file://"): filename = filename[7:]
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
            raise ValueError('Code writer for "%s" is not available.'%language)

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

def _guiless_open_app(filename):
    """Load a new wxGlade project

    GUI-less version of main.wxGladeFrame._open_app().
    Returns True if successful."""
    import time
    from xml.sax import SAXParseException
    from xml_parse import XmlWidgetBuilder, XmlParsingError
    error_msg = None
    infile = None

    start = time.time()

    common.root.clear()
    common.root.init()

    try:
        try:
            logging.info( _('Read wxGlade project from file "%s"'), filename )
            input_file_version = None

            if not isinstance(filename, list):
                common.root.filename = filename
                # decoding will done automatically by SAX XML library
                if compat.PYTHON2:
                    infile = open(filename)
                else:
                    infile = open(filename, "r", encoding="UTF8")
                if hasattr(infile, "seek"):
                    # try to read file version number from the first few lines
                    import re
                    version_re = re.compile(r"<!-- generated by wxGlade (\d+)\.(\d+)\.(\d+)(\S*)\s*")
                    for n in range(3):
                        match = version_re.match( infile.readline() )
                        if match:
                            major, minor, sub, extension = match.groups()
                            input_file_version = (int(major), int(minor), int(major), extension)
                            break
                    infile.seek(0)

            else:
                common.root.filename = None

            p = XmlWidgetBuilder(filename, input_file_version)

            if infile is not None:
                p.parse(infile)
            else:
                p.parse_string(filename)
                filename = None
        except (EnvironmentError, SAXParseException, XmlParsingError) as msg:
            if config.debugging: raise
            if infile is not None:
                error_msg = _("Error loading file %s:\n%s") % (filename, msg)
            else:
                error_msg = _("Error loading from a file-like object:\n%s") % msg
        except Exception as inst:
            if config.debugging: raise
            if filename:
                fn = os.path.basename(filename).encode('ascii','replace')
                msg = _('loading file "%s"') % fn
            else:
                msg = _('loading from a file-like object')
            error_msg = ("%s: %s" % (msg, inst))
    finally:
        if infile and filename:
            infile.close()

        if error_msg:
            common.root.clear()
            common.root.new()
            common.root.saved = True

            logging.error(error_msg)

            return False

    if common.root.is_template:
        logging.info(_("Template loaded"))
        common.root.template_data = template.Template(filename)
        common.root.filename = None

    end = time.time()
    logging.debug(_('Loading time: %.5f'), end - start)

    common.root.saved = True
    #common.property_panel.Raise()

    duration = end - start
    if filename:
        logging.debug( _("Loaded %s in %.2f seconds") % (os.path.basename(filename), duration ))
    else:
        logging.debug( _("Loaded in %.2f seconds") % duration )

    return True

    

def command_line_code_generation(filename, language, out_path=None):
    """Starts a code generator without starting the GUI.

    filename: Name of wxg file to generate code from
    language: Code generator language
    out_path: output file / output directory"""
    import application, tree
    # Instead of instantiating a main.wxGlade() object, that is
    # derived from wx.App, we must do the equivalent work.  The
    # following lines are taken from main.wxGlade().OnInit() and
    # main.wxGladeFrame.__init__()
    common.init_preferences()
    common.root = app = application.Application()

    # Now we can load the file
    if filename is not None:
        b = _guiless_open_app(filename)
        if not b:
            sys.exit(1)
    try:
        if language not in common.code_writers:
            raise ValueError('Code writer for "%s" is not available.'%language)
        common.root.properties["language"].set(language)
        common.root.generate_code(out_path=out_path)
    #except errors.WxgBaseException as inst:
        #if config.debugging: raise
        #logging.error(inst)
        #sys.exit(inst)
    except Exception:
        if config.debugging: raise
        logging.error( _("An exception occurred while generating the code for the application.\n"
                         "If you think this is a wxGlade bug, please report it.") )
        logging.exception(_('Internal Error'))
        sys.exit(1)
    if not config.testing:
        sys.exit(0)


def init_stage1(options):
    """Initialise paths for wxGlade (first stage)
    Initialisation is split because the test suite doesn't work with proper initialised paths."""
    config.version = config.get_version()
    common.init_paths(options)

    # initialise own logging extensions
    log.init(filename=config.log_file, encoding='utf-8',
             level='DEBUG' if config.debugging else 'INFO')
    atexit.register(log.deinit)

    # print versions
    logging.info( _('Starting wxGlade version "%s" on Python %s'), config.version, config.py_version )

    # print used paths
    logging.debug(_('Base directory:             %s'), config.wxglade_path)
    logging.debug(_('Documentation directory:    %s'), config.docs_path)
    logging.debug(_('Icons directory:            %s'), config.icons_path)
    logging.debug(_('Build-in widgets directory: %s'), config.widgets_path)
    logging.debug(_('Template directory:         %s'), config.templates_path)
    logging.debug(_('Credits file:               %s'), config.credits_file)
    logging.debug(_('License file:               %s'), config.license_file)
    logging.debug(_('Manual file:                %s'), config.manual_file)
    logging.debug(_('Tutorial file:              %s'), config.tutorial_file)
    logging.debug(_('Home directory:             %s'), config.home_path)
    logging.debug(_('Application data directory: %s'), config.appdata_path)
    logging.debug(_('Configuration file:         %s'), config.rc_file)
    logging.debug(_('History file:               %s'), config.history_file)
    logging.debug(_('Log file:                   %s'), config.log_file)

    # adapt application search path
    sys.path.insert(0, config.wxglade_path)
    sys.path.insert(1, config.widgets_path)


def init_stage2(use_gui):
    """Initialise the remaining (non-path) parts of wxGlade (second stage)
    use_gui: Starting wxGlade GUI"""
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
        
        if sys.platform=="win32":
            # register ".wxg" extension
            try:
                import msw
                msw.register_extensions(["wxg"], "wxGlade")
                if not os.path.exists(config.rc_file):
                    config.inform_screen_reader = msw.check_for_screen_reader()
            except ImportError:
                pass

        # codewrites, widgets and sizers are loaded in class main.wxGladeFrame
    else:
        # use_gui has to be set before importing config
        common.init_preferences()
        common.init_codegen()


def run_main():
    "This main procedure is started by calling either wxglade.py or wxglade.pyw on windows."
    # check command line parameters first
    options = parse_command_line()

    # initialise wxGlade (first stage and second stage)
    init_stage1(options)
    init_stage2(options.start_gui)

    if options.start_gui:
        # late import of main (imported wx) for using wxversion  in init_stage2()
        import main
        main.main(options.filename)
    else:
        command_line_code_generation( filename=options.filename, language=options.language, out_path=options.output )

if __name__ == "__main__":
    run_main()
