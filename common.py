"""
Global variables

@copyright: 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import os
import pprint
import sys
import traceback
import types

use_gui = True
"""\
If False, the program is invoked from the command-line in "batch" mode
(for code generation only)
"""

nohg_version = '0.6.8'
"""\
Version number to return if no hg repo has been found
"""


def _get_version():
    """\
    Create the version identification string

    Try to query the local hg repository to build the version string or
    return L{nohg_version}.

    @return: The current wxGlade version number
    @rtype: String
    @see: L{nohg_version}
    """
#    main_version = ''
    main_version = nohg_version
    repo_changed = []
#    try:
#        from mercurial.hg import repository
#        from mercurial.ui import ui
#        from mercurial.node import short
#        from mercurial.error import RepoError
#    except ImportError:
#        # no mercurial module available
#        main_version = nohg_version
#    except:
#        # unkown failure
#        main_version = nohg_version
#    else:
#        # try to open local hg repository
#        try:
#            repo = repository(ui(), os.path.dirname(__file__))
#        except RepoError:
#            # no mercurial repository found
#            main_version = nohg_version
#        else:
#            ctx = repo[None]
#            parents = ctx.parents()
#            repo_changed = ctx.files() + ctx.deleted()
#            if len(parents) == 1 and not repo_changed:
#                # release tag isn't at tip it's -2 (one below tip)
#                parents = parents[0].parents()
#                node = parents[0].node()
#                tags = repo.nodetags(node)
#                # look for the special 'rel_X.X' tag
#                for tag in tags:
#                    if tag.startswith('rel_') and len(tag) > 4:
#                        main_version = tag[4:]
#                        break
#                # handle untagged version e.g. tip
#                if not main_version:
#                    main_version = short(node)
#            else:
#                main_version = '%s' % \
#                    '+'.join([short(p.node()) for p in parents])

    suffix_changed = repo_changed and '+' or ''
    suffix_edition = hasattr(sys, 'frozen') \
                     and ' (standalone edition)' \
                     or ''

    ver = "%s%s%s" % (main_version, suffix_changed, suffix_edition)
    return ver

version = _get_version()
"""\
wxGlade version string
@see: L{_get_version()}
"""

py_version = sys.version.split()[0]
"""\
Python version
"""

platform = None
"""\
Current platform (mostly wx.Platform)
"""

wxglade_path = '.'
"""\
Program path, set in wxglade.py
"""

docs_path = 'docs'
"""\
Path to wxGlade documentation (e.g. html tuturial, license.txt, credits.txt)

@note: This path will be set during initialisation
"""

icons_path = 'icons'
"""\
Path to wxGlade icons

@note: This path will be set during initialisation
"""

templates_path = 'templates'
"""\
Path to wxGlade templates

@note: This path will be set during initialisation
"""

widgets_path = 'widgets'
"""\
Path to wxGlade "built-in" widgets

@note: This path will be set during initialisation
"""

credits_file = None
"""\
Path of the credits file "credits.txt"
"""

license_file = None
"""\
Path of the license file "license.txt"
"""

tutorial_file = 'docs/html/index.html'
"""\
Path to wxGlade tutorial (HTML)

@note: This path will be set during initialisation
"""

widgets = {}
"""\
Widgets dictionary: each key is the name of some EditWidget class; the mapped
value is a 'factory' function which actually builds the object. Each of these
functions accept 3 parameters: the parent of the widget, the sizer by which
such widget is controlled, and the position inside this sizer.
"""

widgets_from_xml = {}
"""\
Widgets_from_xml dictionary: table of factory functions to build objects
from an xml file
"""

property_panel = None
"""\
property_panel wxPanel: container inside which Properties of the current
focused widget are displayed
"""

app_tree = None
"""\
app_tree Tree: represents the widget hierarchy of the application; the
root is the application itself
"""

adding_widget = False
"""\
If True, the user is adding a widget to some sizer
"""

adding_sizer = False
"""\
Needed to add toplevel sizers
"""

widget_to_add = None
"""\
Reference to the widget that is being added: this is a key in the
'widgets' dictionary
"""

palette = None
"""\
Reference to the main window (the one which contains the various buttons to
add the different widgets)
"""

refs = {}
"""\
Dictionary which maps the ids used in the event handlers to the
corresponding widgets: used to call the appropriate builder function
when a dropping of a widget occurs, knowing only the id of the event
"""

class_names = {}
"""\
Dictionary which maps the name of the classes used by wxGlade to the
correspondent classes of wxWindows
"""

toplevels = {}
"""\
Names of the Edit* classes that can be toplevels, i.e. widgets for which to
generate a class declaration in the code
"""

code_writers = {}
"""\
Dictionary of objects used to generate the code in a given language.

@note: A code writer object must implement this interface:
 - initialize(out_path, multi_files)
 - language
 - setup
 - add_widget_handler(widget_name, handler[, properties_handler])
 - add_property_handler(property_name, handler[, widget_name])
 - add_object(top_obj, sub_obj)
 - add_class(obj)
 - add_sizeritem(toplevel, sizer, obj_name, option, flag, border)
 - add_app(app_attrs, top_win_class)
 - ...
"""


def load_code_writers():
    """\
    Fills the common.code_writers dictionary: to do so, loads the modules
    found in the 'codegen/' subdir
    """
    codegen_path = os.path.join(wxglade_path, 'codegen')
    sys.path.insert(0, codegen_path)
    for module in os.listdir(codegen_path):
        name, ext = os.path.splitext(module)
        # skip __init__
        if name == "__init__":
            continue
        # allow regular files only
        if not os.path.isfile(os.path.join(codegen_path, module)):
            continue
        # ignore none python files
        if ext not in ['.py', '.pyo', '.pyc']:
            continue
        # skip already imported modules
        if name in sys.modules:
            continue
        # import file and initiate code writer
        try:
            writer = __import__(name).writer
        except (AttributeError, ImportError, NameError, 
                SyntaxError, ValueError):
            message.exception(
                _('"%s" is not a valid code generator module') % module
                )
        else:
            code_writers[writer.language] = writer
            if hasattr(writer, 'setup'):
                writer.setup()
            if use_gui:
                print _('loaded code generator for %s') % writer.language


def load_widgets():
    """\
    Scans the 'widgets/' directory to find the installed widgets,
    and returns 2 lists of buttons to handle them: the first contains the
    ``core'' components, the second the user-defined ones
    """
    import config
    buttons = []
    # load the "built-in" widgets
    buttons.extend(__load_widgets(widgets_path))

    # load the "local" widgets
    local_widgets_dir = config.preferences.local_widget_path
    return buttons, __load_widgets(local_widgets_dir)


def __load_widgets(widget_dir):
    buttons = []
    # test if the "widgets.txt" file exists
    widgets_file = os.path.join(widget_dir, 'widgets.txt')
    if not os.path.isfile(widgets_file):
        return buttons

    # add the dir to the sys.path
    sys.path.append(widget_dir)
    modules = open(widgets_file)
    if use_gui:
        print _('Found widgets listing -> %s') % widgets_file
        print _('loading widget modules:')
    for line in modules:
        module = line.strip()
        if not module or module.startswith('#'):
            continue
        module = module.split('#')[0].strip()
        try:
            try:
                b = __import__(module).initialize()
            except ImportError:
                # try importing from a zip archive
                if os.path.exists(os.path.join(widget_dir, module + '.zip')):
                    sys.path.append(os.path.join(widget_dir, module + '.zip'))
                    try:
                        b = __import__(module).initialize()
                    finally:
                        sys.path.pop()
                else:
                    raise
        except (AttributeError, ImportError, NameError, 
                SyntaxError, ValueError):
            message.exception(_('ERROR loading "%s"') % module)
        else:
            if use_gui:
                print '\t' + module
            buttons.append(b)
    modules.close()
    return buttons


def load_sizers():
    import edit_sizers
    return edit_sizers.init_all()


def add_object(event):
    """\
    Adds a widget or a sizer to the current app.
    """
    global adding_widget, adding_sizer, widget_to_add
    adding_widget = True
    adding_sizer = False
    tmp = event.GetId()
    widget_to_add = refs[tmp]
    # TODO: find a better way
    if widget_to_add.find('Sizer') != -1:
        adding_sizer = True


def add_toplevel_object(event):
    """\
    Adds a toplevel widget (Frame or Dialog) to the current app.
    """
    widgets[refs[event.GetId()]](None, None, 0)
    app_tree.app.saved = False


def make_object_button(widget, icon_path, toplevel=False, tip=None):
    """\
    Creates a button for the widgets toolbar.

    Function used by the various widget modules to add a button to the
    widgets toolbar.

    @param widget: (name of) the widget the button will add to the app
    @param icon_path: path to the icon used for the button
    @param toplevel: true if the widget is a toplevel object (frame, dialog)
    @param tip: tool tip to display
    @return: The newly created wxBitmapButton
    """
    import wx
    from tree import WidgetTree
    id = wx.NewId()
    if not os.path.isabs(icon_path):
        icon_path = os.path.join(wxglade_path, icon_path)
    if wx.Platform == '__WXGTK__':
        style = wx.NO_BORDER
    else:
        style = wx.BU_AUTODRAW
    import misc
    bmp = misc.get_xpm_bitmap(icon_path)
    tmp = wx.BitmapButton(palette, id, bmp, size=(31, 31), style=style)
    if not toplevel:
        wx.EVT_BUTTON(tmp, id, add_object)
    else:
        wx.EVT_BUTTON(tmp, id, add_toplevel_object)
    refs[id] = widget
    if not tip:
        tip = _('Add a %s') % widget.replace(_('Edit'), '')
    tmp.SetToolTip(wx.ToolTip(tip))

    WidgetTree.images[widget] = icon_path

    # add support for ESC key. We bind the handler to the button, because
    # (at least on GTK) EVT_CHAR are not generated for wxFrame objects...
    def on_char(event):
        #print 'on_char'
        if event.HasModifiers() or event.GetKeyCode() != wx.WXK_ESCAPE:
            event.Skip()
            return
        global adding_widget, adding_sizer, widget_to_add
        adding_widget = False
        adding_sizer = False
        widget_to_add = None
        import misc
        if misc._currently_under_mouse is not None:
            misc._currently_under_mouse.SetCursor(wx.STANDARD_CURSOR)
        event.Skip()
    wx.EVT_CHAR(tmp, on_char)

    return tmp


def exceptionHandler(exc_type, exc_value, exc_tb):
    """\
    Logs detailed information about uncatched exceptions

    @param exc_type:  Type of the exception (normally a class object)
    @param exc_value: The "value" of the exception
    @param exc_tb:    Call stack of the exception
    """
    try:
        tb = exc_tb
        while tb.tb_next:
            tb = tb.tb_next
        frame_locals = tb.tb_frame.f_locals
        # log exception details
        message.error(_('An unexpected error occurs!'))
        message.error(_('Error type:    %s'), exc_type)
        message.error(_('Error details: %s'), exc_value)
        message.error(_('Stack Trace:'))
        lines = '\n'.join(traceback.format_exception(exc_type, exc_value, exc_tb)).split('\n')
        for line in lines:
            if not line:
                continue
            message.error(line)
        message.error(_('Local variables of the last stack entry:'))
        for varname in frame_locals.keys():
            # convert variablen name and value to ascii
            var = frame_locals[varname]
            vartype = type(var)
            if vartype == types.UnicodeType:
                varvalue = frame_locals[varname]
                varvalue = varvalue.encode('unicode_escape')
            else:
                varvalue = pprint.pformat(frame_locals[varname])
                varvalue = varvalue
            message.error(_('%s (%s): %s'), varname, vartype, varvalue)

    # delete local references of tracebacks or part of tracebacks
    # to avoid cirular references
    finally:
        del tb
        del frame_locals


def _encode_from_xml(label, encoding=None):
    """\
    Returns a str which is the encoded version of the unicode label
    """
    if encoding is None:
        encoding = app_tree.app.encoding
    return label.encode(encoding, 'replace')


def _encode_to_xml(label, encoding=None):
    """\
    returns a utf-8 encoded representation of label. This is equivalent to:
    str(label).decode(encoding).encode('utf-8')
    """
    if encoding is None:
        encoding = app_tree.app.encoding
    if type(label) == type(u''):
        return label.encode('utf-8')
    return str(label).decode(encoding).encode('utf-8')


_backed_up = {}
"""\
Set of filenames already backed up during this session
"""


def save_file(filename, content, which='wxg'):
    """\
    Save I{content} to file named I{filename} and, if user's preferences say
    so and I{filename} exists, makes a backup copy of it.

    @note: Exceptions that may occur while performing the operations are not
           handled.

    @param filename: Name of the file to create
    @param content:  String to store into 'filename'
    @param which:    Kind of backup: 'wxg' or 'codegen'
    """
    import config
    if which == 'wxg':
        do_backup = config.preferences.wxg_backup
    elif which == 'codegen':
        do_backup = config.preferences.codegen_backup
    else:
        raise NotImplementedError(
            'Unknown value "%s" for parameter "which"!' % which
            )
    try:
        if do_backup and \
           filename not in _backed_up and \
           os.path.isfile(filename):
            # make a backup copy of filename
            infile = open(filename)
            outfile = open(filename + config.preferences.backup_suffix, 'w')
            outfile.write(infile.read())
            infile.close()
            outfile.close()
            _backed_up[filename] = True
        # save content to file (but only if content has changed)
        savecontent = True
        if os.path.isfile(filename):
            oldfile = open(filename)
            savecontent = (oldfile.read() != content)
            oldfile.close()
        if savecontent:
            directory = os.path.dirname(filename)
            if directory and not os.path.isdir(directory):
                os.makedirs(directory)
            outfile = open(filename, 'w')
            outfile.write(content)
            outfile.close()
    finally:
        if 'infile' in locals():
            infile.close()
        if 'outfile' in locals():
            outfile.close()
        if 'oldfile' in locals():
            oldfile.close()


#------------------------------------------------------------------------------
# Autosaving, added 2004-10-15
#------------------------------------------------------------------------------

def get_name_for_autosave(filename=None):
    if filename is None:
        filename = app_tree.app.filename
    if not filename:
        import config
        path, name = config._get_home(), ""
    else:
        path, name = os.path.split(filename)
    ret = os.path.join(path, "#~wxg.autosave~%s#" % name)
    return ret


def autosave_current():
    if app_tree.app.saved:
        return False         # do nothing in this case...
    try:
        outfile = open(get_name_for_autosave(), 'w')
        app_tree.write(outfile)
        outfile.close()
    except Exception, e:
        print e
        return False
    return True


def remove_autosaved(filename=None):
    autosaved = get_name_for_autosave(filename)
    if os.path.exists(autosaved):
        try:
            os.unlink(autosaved)
        except OSError, e:
            print e


def check_autosaved(filename):
    """\
    Returns True iff there are some auto saved data for filename
    """
    if filename is not None and filename == app_tree.app.filename:
        # this happens when reloading, no autosave-restoring in this case...
        return False
    autosaved = get_name_for_autosave(filename)
    try:
        if filename:
            orig = os.stat(filename)
            auto = os.stat(autosaved)
            return orig.st_mtime < auto.st_mtime
        else:
            return os.path.exists(autosaved)
    except OSError, e:
        if e.errno != 2:
            print e
        return False


def restore_from_autosaved(filename):
    autosaved = get_name_for_autosave(filename)
    # when restoring, make a backup copy (if user's preferences say so...)
    if os.access(autosaved, os.R_OK):
        try:
            save_file(filename, open(autosaved).read(), 'wxg')
        except OSError, e:
            print e
            return False
        return True
    return False


def generated_from():
    import config
    if config.preferences.write_generated_from and app_tree and \
           app_tree.app.filename:
        return ' from "' + app_tree.app.filename + '"'
    return ""


class MessageLogger(object):
    """\
    Small own logging facility
    
    @ivar disabled: If True log messages won't be processed
    @type disabled: Boolean
    
    @ivar lines: Message buffer
    @type lines: List of strings
    
    @ivar logger: Reference to an instance of L{msgdialog.MessageDialog}
    """

    def __init__(self):
        self.disabled = False
        self.lines = []
        self.logger = None

    def _setup_logger(self):
        """\
        Create L{msgdialog.MessageDialog} instance.
        """
        import msgdialog
        self.logger = msgdialog.MessageDialog(None, -1, "")
        self.logger.msg_list.InsertColumn(0, "")

    def __call__(self, kind, fmt, *args):
        if self.disabled:
            return
        kind = kind.upper()
        if use_gui:
            import misc
            if args:
                msg = misc.wxstr(fmt) % tuple([misc.wxstr(a) for a in args])
            else:
                msg = misc.wxstr(fmt)
            self.lines.extend(msg.splitlines())

        # show errors and exceptions always at console
        if not use_gui or kind in [_("EXCEPTION"),  _("ERROR")]:
            if args:
                msg = fmt % tuple(args)
            else:
                msg = fmt
            print "%s: %s" % (kind, msg)

    def debug(self, fmt, *args):
        """\
        Show debug messages
        """
        self.__call__(_("DEBUG"), fmt, *args)

    def info(self, fmt, *args):
        """\
        Show informational messages
        """
        self.__call__(_("INFO"), fmt, *args)

    def warn(self, fmt, *args):
        """\
        Show warning messages
        """
        self.__call__(_("WARNING"), fmt, *args)

    def error(self, fmt, *args):
        """\
        Show error  messages
        """
        self.__call__(_("ERROR"), fmt, *args)
        
    def exception(self, fmt, *args):
        """\
        Show exception details
        """
        self.__call__(_("EXCEPTION"), fmt, *args)
        if not sys.exc_info()[0]:
            return
        exceptionHandler(*sys.exc_info())
        sys.exc_clear()

    def flush(self):
        """\
        Show log messages if L{use_gui} is True
        """
        if self.lines and use_gui:
            if not self.logger:
                self._setup_logger()
            self.logger.msg_list.Freeze()
            self.logger.msg_list.DeleteAllItems()
            for line in self.lines:
                self.logger.msg_list.Append([line])
            self.lines = []
            self.logger.msg_list.SetColumnWidth(0, -1)
            self.logger.msg_list.Thaw()
            self.logger.ShowModal()

# end of class MessageLogger

message = MessageLogger()
"""\
L{MessageLogger} instance
"""
