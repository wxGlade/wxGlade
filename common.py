"""
Global variables

@copyright: 2002-2007 Alberto Griggio
@copyright: 2013 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import ConfigParser
import logging
import os
import os.path
import sys

import config


def set_version():
    """\
    Create the version identification string

    Try to query the local hg repository to build the version string or
    return L{config.version_nohgfound}.

    @return: The current wxGlade version number
    @rtype: String
    @see: L{config.version_nohgfound}
    """
    main_version = ''
    repo_changed = []
    try:
        from mercurial.hg import repository
        from mercurial.ui import ui
        from mercurial.node import short
        from mercurial.error import RepoError
    except ImportError:
        # no mercurial module available
        main_version = config.version_nohgfound
    except:
        # unkown failure
        main_version = config.version_nohgfound
    else:
        # try to open local hg repository
        try:
            repo = repository(ui(), os.path.dirname(__file__))
        except RepoError:
            # no mercurial repository found
            main_version = config.version_nohgfound
        else:
            ctx = repo[None]
            parents = ctx.parents()
            repo_changed = ctx.files() + ctx.deleted()
            if len(parents) == 1 and not repo_changed:
                # release tag isn't at tip it's -2 (one below tip)
                parents = parents[0].parents()
                node = parents[0].node()
                tags = repo.nodetags(node)
                # look for the special 'rel_X.X' tag
                for tag in tags:
                    if tag.startswith('rel_') and len(tag) > 4:
                        main_version = tag[4:]
                        break
                # handle untagged version e.g. tip
                if not main_version:
                    main_version = short(node)
            else:
                main_version = '%s' % \
                    '+'.join([short(p.node()) for p in parents])

    suffix_changed = repo_changed and '+' or ''
    suffix_edition = hasattr(sys, 'frozen') \
                     and ' (standalone edition)' \
                     or ''

    ver = "%s%s%s" % (main_version, suffix_changed, suffix_edition)
    return ver

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
    codegen_path = os.path.join(config.wxglade_path, 'codegen')
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
            logging.exception(
                _('"%s" is not a valid code generator module') % module
                )
        else:
            code_writers[writer.language] = writer
            if hasattr(writer, 'setup'):
                writer.setup()
            if config.use_gui:
                logging.info(
                    _('loaded code generator for %s'),
                    writer.language
                    )


def load_widgets():
    """\
    Scans the 'widgets/' directory to find the installed widgets,
    and returns 2 lists of buttons to handle them: the first contains the
    ``core'' components, the second the user-defined ones
    """
    buttons = []
    # load the "built-in" widgets
    buttons.extend(__load_widgets(config.widgets_path))

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
    if config.use_gui:
        logging.info(_('Found widgets listing -> %s'), widgets_file)
        logging.info(_('loading widget modules:'))
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
            logging.exception(_('ERROR loading "%s"'), module)
        else:
            if config.use_gui:
                logging.info('\t%s', module)
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
        icon_path = os.path.join(config.wxglade_path, icon_path)
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
        #logging.debug('on_char')
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


def save_file(filename, content, which='wxg'):
    """\
    Save I{content} to file named I{filename} and, if user's preferences say
    so and I{filename} exists, makes a backup copy of it.

    @note: Exceptions that may occur while performing the operations are not
           handled.

    @see: L{config._backed_up}

    @param filename: Name of the file to create
    @param content:  String to store into 'filename'
    @param which:    Kind of backup: 'wxg' or 'codegen'
    """
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
           filename not in config._backed_up and \
           os.path.isfile(filename):
            # make a backup copy of filename
            infile = open(filename)
            outfile = open(filename + config.preferences.backup_suffix, 'w')
            outfile.write(infile.read())
            infile.close()
            outfile.close()
            config._backed_up[filename] = True
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


def get_name_for_autosave(filename=None):
    if filename is None:
        filename = app_tree.app.filename
    if not filename:
        path, name = config.home_path, ""
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
    except Exception:
        logging.exception(_('Internal Error'))
        return False
    return True


def remove_autosaved(filename=None):
    autosaved = get_name_for_autosave(filename)
    if os.path.exists(autosaved):
        try:
            os.unlink(autosaved)
        except OSError:
            logging.exception(_('Internal Error'))


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
            logging.exception(_('Internel Error'))
        return False


def restore_from_autosaved(filename):
    autosaved = get_name_for_autosave(filename)
    # when restoring, make a backup copy (if user's preferences say so...)
    if os.access(autosaved, os.R_OK):
        try:
            save_file(filename, open(autosaved).read(), 'wxg')
        except OSError:
            logging.exception(_('Internel Error'))
            return False
        return True
    return False


def init_preferences():
    """\
    Load / initialise preferences
    """
    if config.preferences is None:
        config.preferences = Preferences()
        search_path = [os.path.join(config.wxglade_path, config.rc_file)]
        if 'WXGLADE_CONFIG_PATH' in os.environ:
            search_path.append(
                os.path.expandvars('$WXGLADE_CONFIG_PATH/%s' % config.rc_file)
            )
        search_path.append(
            os.path.join(config.appdata_path, '.wxglade', config.rc_file)
        )
        config.preferences.read(search_path)
        if not config.preferences.has_section('wxglade'):
            config.preferences.add_section('wxglade')


def save_preferences():
    """\
    Save current settings as well as the file history
    """
    # let the exception be raised
    if 'WXGLADE_CONFIG_PATH' in os.environ:
        path = os.path.expandvars('$WXGLADE_CONFIG_PATH')
    else:
        path = config.appdata_path
        if path != config.wxglade_path:
            path = os.path.join(path, '.wxglade')
    if not os.path.isdir(path):
        os.makedirs(path)
        # always save the file history
    if config.use_file_history:
        fh = palette.file_history
        count = fh.GetCount()
        encoding = 'utf-8'
        filenames = [_encode_to_xml(fh.GetHistoryFile(i), encoding)
                     for i in
                     range(min(config.preferences.number_history, count))]
        outfile = open(os.path.join(path, 'file_history.txt'), 'w')
        print >> outfile, "# -*- coding: %s -*-" % encoding
        for filename in filenames:
            print >> outfile, filename
        outfile.close()
    if config.preferences.changed:
        outfile = open(os.path.join(path, config.rc_file), 'w')
        # let the exception be raised to signal abnormal behaviour
        config.preferences.write(outfile)
        outfile.close()


def load_history():
    """\
    Loads the file history and returns a list of paths
    """
    if 'WXGLADE_CONFIG_PATH' in os.environ:
        path = os.path.expandvars('$WXGLADE_CONFIG_PATH')
    else:
        path = config.appdata_path
        if path != config.wxglade_path:
            path = os.path.join(path, '.wxglade')
    try:
        history = open(os.path.join(path, 'file_history.txt'))
        lines = history.readlines()
        if lines and lines[0].startswith('# -*- coding:'):
            try:
                encoding = 'utf-8'
                #lines = [common._encode_from_xml(e, encoding) for e in l[1:]]
                lines = [e.decode(encoding) for e in lines[1:]]
            except Exception:
                logging.exception(_("Internal Error"))
                lines = lines[1:]
        history.close()
        if config.use_gui:
            import misc
            lines = [misc.wxstr(e, 'utf-8') for e in lines]
        return lines
    except IOError:
        # don't consider this an error
        return []


class Preferences(ConfigParser.ConfigParser):
    _has_home = os.path.expanduser('~') != '~'
    _defaults = {
        'use_menu_icons': config.use_gui and config.platform != '__WXGTK__',
        'frame_tool_win': True,
        'open_save_path': '',
        'codegen_path': '',
        'use_dialog_units': False,
        'number_history': 4,
        'show_progress': True,
        'wxg_backup': True,
        'codegen_backup': True,
        'backup_suffix': sys.platform == 'win32' and '.bak' or '~',
        'buttons_per_row': 5,
        'remember_geometry': False,
        'local_widget_path': '',
        'default_border': False,
        'default_border_size': 3,
        'show_sizer_handle': True,
        'allow_duplicate_names': False,
        'autosave': True,
        'autosave_delay': 120,  # in seconds
        'use_kde_dialogs': False,
        'show_completion': True,
        'write_timestamp': True,
        'write_generated_from': False,
        }

    def __init__(self, defaults=None):
        # set defaults of 'codegen_path', 'local_widget_path', and
        # 'open_save_path' and 'codegen_path' if the class is
        # instantiated first time, because the home_path is set later
        if config.home_path and not self._defaults['open_save_path']:
            self._defaults['open_save_path'] = config.home_path
            self._defaults['codegen_path'] = config.home_path
        if config.appdata_path and not self._defaults['local_widget_path']:
            self._defaults['local_widget_path'] = os.path.join(
                config.appdata_path, '.wxglade', 'widgets'
                )
        self.def_vals = defaults
        if self.def_vals is None:
            self.def_vals = Preferences._defaults
        self.changed = False
        ConfigParser.ConfigParser.__init__(self)
        #self.default_border_size = 3

    def __getattr__(self, attr):
        val = self.def_vals.get(attr, "")
        # UGLY!!!
        cast = type(val)
        if cast is bool:
            cast = self._cast_to_bool
        # ...but I haven't found a better way: the problem is that
        # bool('0') == True, while int('0') == False, and we want the
        # latter behaviour
        try:
            return cast(self.get('wxglade', attr))
        except (ConfigParser.NoOptionError, ValueError):
            return val

    def __iter__(self):
        def do_iter():
            for key in self.def_vals:
                yield key, self[key]
        return do_iter()

    def _cast_to_bool(self, val):
        try:
            return int(val)
        except ValueError:
            val = val.lower().strip()
            if val in ('true', 'on'):
                return 1
            elif val in ('false', 'off'):
                return 0
            else:
                raise

    def __getitem__(self, attr):
        return self.__getattr__(attr)

    def __setitem__(self, attr, val):
        self.set('wxglade', attr, str(val))
        self.changed = True

    def set_geometry(self, name, geometry):
        if geometry is not None:
            section = 'geometry_%s' % name
            if not self.has_section(section):
                self.add_section(section)
            self.set(section, 'x', geometry[0])
            self.set(section, 'y', geometry[1])
            self.set(section, 'w', geometry[2])
            self.set(section, 'h', geometry[3])

    def get_geometry(self, name):
        section = 'geometry_%s' % name
        if self.has_section(section):
            x = self.get(section, 'x')
            y = self.get(section, 'y')
            w = self.get(section, 'w')
            h = self.get(section, 'h')
            return x, y, w, h
        else:
            return None

# end of class Preferences