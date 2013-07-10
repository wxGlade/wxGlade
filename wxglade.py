#!/usr/bin/env python
"""
Entry point of wxGlade

@copyright: 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import os
import sys
import gettext
import common
import optparse

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

def error(msg):
    """\
    Print an error message at stderr
    """
    print >> sys.stderr, _("ERROR: %s") % msg

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
            error(_("No wxg file given!\n"))
            parser.print_help()
            sys.exit(1)
        else:
            error(_("Too many wxg files given!\n"))
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
        error(_('No writer for language "%s" available') % language)

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
        error(inst)
        sys.exit(1)
    except Exception:
        common.message.exception(_('Internal Error'))
        error(
            _("An exception occurred while generating the code for the application.\n"
              "If you think this is a wxGlade bug, please report it.")
             )
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
        error(_('Credits file "credits.txt" not found!'))
    if not common.license_file:
        error(_('License file "license.txt" not found!'))

    # print used paths
    print _('Base directory:             %s') % common.wxglade_path
    print _('Documentation directory:    %s') % common.docs_path
    print _('Icons directory:            %s') % common.icons_path
    print _('Build-in widgets directory: %s') % common.widgets_path
    print _('Template directory:         %s') % common.templates_path
    print _('Credits file:               %s') % common.credits_file
    print _('License file:               %s') % common.license_file
    print _('Tutorial file:              %s') % common.tutorial_file

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
        # ensure minimal wx version
        if not hasattr(sys, 'frozen') and \
           'wxversion' not in sys.modules and \
           'wx' not in sys.modules:
            import wxversion
            wxversion.ensureMinimal("2.6")
        
        # store current platform (None is default)
        import wx
        common.platform = wx.Platform

        # codewrites, widgets and sizers are loaded in class main.wxGladeFrame
    else:
        # use_gui has to be set before importing config
        import config
        config.init_preferences()
        common.load_code_writers()
        common.load_widgets()
        common.load_sizers()


def run_main():
    """\
    This main procedure is started by calling either wxglade.py or
    wxglade.pyw on windows.
    
    It parses the command line, install the exception handler and initialise
    wxGlade.
    
    @see: L{common.exceptionHandler()}
    """
    # check command line parameters first
    options = parse_command_line()

    # print versions 
    print _("Starting wxGlade version %s on Python %s") % (
        common.version,
        common.py_version,
        )

    # install own exception handler
    sys.excepthook = common.exceptionHandler

    # initialise wxGlade (first stage)
    init_stage1()

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
