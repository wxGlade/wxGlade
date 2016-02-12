"""\
Global functions and variables

@copyright: 2002-2007 Alberto Griggio
@copyright: 2013-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import codecs
import ConfigParser
import logging
import md5
import os
import os.path
import sys
import types
from xml.sax.saxutils import escape, quoteattr

import config
import plugins

widgets = {}
"""\
Widgets dictionary: each key is the name of some EditWidget class; the mapped
value is a 'factory' function which actually builds the object. Each of these
functions accept 3 parameters: the parent of the widget, the sizer by which
such widget is controlled, and the position inside this sizer.

@type: dict
"""

widgets_from_xml = {}
"""\
Factory functions to build objects from a XML file

@type: dict
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

@type: dict
"""

class_names = {}
"""\
Dictionary which maps the name of the classes used by wxGlade to the
correspondent classes of wxWindows

@type: dict
"""

toplevels = {}
"""\
Names of the Edit* classes that can be toplevels, i.e. widgets for which to
generate a class declaration in the code

@type: dict
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

@type: dict[str, BaseLangCodeWriter]
"""


def init_codegen():
    """\
    Load available code generators, core and user widgets as well as sizers

    If wxGlade has been started in GUI mode, the function returns three lists
    of wxBitmapButton objects to handle them. The first contains the
    built-in widgets and the second one the user widgets and the third list
    contains the sizers.

    @return: List of core buttons, list of local buttons and list of sizer
             buttons

    @see: L{load_config()}
    @see: L{load_code_writers()}
    @see: L{load_widgets()}
    @see: L{load_sizers()}
    """
    # process generic related style attributes
    style_attrs_to_sets(config.widget_config['generic_styles'])
    load_config()
    load_code_writers()
    core_buttons, local_buttons = load_widgets()
    sizer_buttons = load_sizers()

    return core_buttons, local_buttons, sizer_buttons


def load_code_writers():
    """\
    Fills the common.code_writers dictionary: to do so, loads the modules
    found in the 'codegen/' subdir
    """
    logging.info('Load code generators:')
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
        except (AttributeError, ImportError, NameError, SyntaxError,
                ValueError):
            logging.exception(
                _('"%s" is not a valid code generator module'), module
            )
        else:
            code_writers[writer.language] = writer
            if config.use_gui:
                logging.info(
                    _('  %s generator loaded'),
                    writer.language
                )


def load_config():
    """\
    Load widget configuration.

    @see: L{plugins.load_widgets_from_dir()}
    """
    # load the "built-in" widgets
    plugins.load_widgets_from_dir(
        config.widgets_path,
        'wconfig'
    )

    # load the "user" widgets
    plugins.load_widgets_from_dir(
        config.preferences.local_widget_path,
        'wconfig'
    )

    return


def load_sizers():
    """\
    Load and initialise the sizer support modules.

    @return: A list of BitmapButton objects to handle sizer buttons

    @see: L{edit_sizers}
    """
    logging.info('Load sizer generators:')
    for lang in code_writers.keys():
        module_name = 'edit_sizers.%s_sizers_codegen' % \
                      code_writers[lang].lang_prefix
        try:
            sizer_module = plugins.import_module(config.wxglade_path,
                                                 module_name)
            if not sizer_module:
                # error already logged
                pass
            elif hasattr(sizer_module, 'initialize'):
                sizer_module.initialize()
            else:
                logging.warning(
                    _('Missing function "initialize()" in imported '
                      'module %s. Skip initialisation.'), module_name)

            if config.use_gui:
                logging.info(_('  for %s'), lang)

        except (AttributeError, ImportError, NameError, SyntaxError,
                ValueError):
            logging.exception(
                _('ERROR loading module "%s"'), module_name)
        except:
            logging.exception(
                _('Unexpected error during import of widget module %s'),
                module_name
            )

    # initialise sizer GUI elements
    import edit_sizers
    return edit_sizers.init_gui()


def load_widgets():
    """\
    Load core and user widgets.

    Scans the application 'widgets/' directory as well as the user widgets
    directory to find the installed widgets.

    It loads built-in widgets and "user" widgets. The build-in widgets are
    load from path in L{config.widgets_path}. The user widget path is stored
    in C{config.preferences.local_widget_path}.

    If wxGlade has been started in GUI mode, the function returns two lists
    of wxBitmapButton objects to handle them. The first contains the
    built-in widgets and the second one the user widgets.

    Both lists are empty in the batch mode.

    If widget ZIP files are found, they will be process first and the default
    Python imports will be the second.

    @see: L{plugins.load_widgets_from_dir()}
    """
    # load the "built-in" widgets
    core_buttons = plugins.load_widgets_from_dir(
        config.widgets_path,
    )

    # load the "user" widgets
    local_buttons = plugins.load_widgets_from_dir(
        config.preferences.local_widget_path,
    )

    # load (remaining) widget code generators
    # Python, C++ and XRC are often loaded via
    # plugins.load_widgets_from_dir() above
    for path in [config.widgets_path,
                 config.preferences.local_widget_path]:
        for lang in ['perl', 'lisp']:
            if lang not in code_writers:
                continue
            codegen_name = '%s_codegen' % code_writers[lang].lang_prefix
            plugins.load_widgets_from_dir(
                path,
                submodule=codegen_name,
            )
    return core_buttons, local_buttons


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

    Icons with a relative path will be loaded from config.icon_path.

    @param widget: (name of) the widget the button will add to the app
    @type widget:   str | Unicode
    @param icon_path: Path to the icon_path used for the button
    @type icon_path:  str | Unicode
    @param toplevel: True if the widget is a toplevel object (frame, dialog)
    @type toplevel:  bool
    @param tip: Tool tip to display
    @type tip:  str | Unicode

    @return: The newly created wxBitmapButton
    """
    import wx
    import misc
    from tree import WidgetTree

    widget_id = wx.NewId()
    if not os.path.isabs(icon_path):
        icon_path = os.path.join(config.icons_path, icon_path)
    if wx.Platform == '__WXGTK__':
        style = wx.NO_BORDER
    else:
        style = wx.BU_AUTODRAW
    bmp = misc.get_xpm_bitmap(icon_path)
    tmp = wx.BitmapButton(palette, widget_id, bmp, size=(31, 31), style=style)
    if not toplevel:
        wx.EVT_BUTTON(tmp, widget_id, add_object)
    else:
        wx.EVT_BUTTON(tmp, widget_id, add_toplevel_object)
    refs[widget_id] = widget
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
        if misc.currently_under_mouse is not None:
            misc.currently_under_mouse.SetCursor(wx.STANDARD_CURSOR)
        event.Skip()

    wx.EVT_CHAR(tmp, on_char)

    return tmp


def encode_to_unicode(item, encoding=None):
    """\
    Decode the item to a Unicode string. The encoding to UTF-8 will be done
    later.

    Non-string items will be converted to string automatically.

    If no encoding given, app_tree.app.encoding or 'UFT-8' will be used.

    @param item: Item to convert
    @type item:  str | Unicode

    @param encoding: Codec to decode
    @type encoding:  str | None

    @rtype: unicode

    @see: L{app_tree}
    """
    if isinstance(item, types.UnicodeType):
        return item
    if not isinstance(item, types.StringTypes):
        item = str(item)
    if not encoding:
        if app_tree:
            encoding = app_tree.app.encoding
        else:
            encoding = 'UTF-8'
    item = item.decode(encoding)
    return item


def register(lang, klass_name, code_writer, property_name=None,
             property_handler=None, widget_name=None):
    """\
    Initialise and register widget code generator instance. The property
    handler will registered additionally.

    @param lang:             Code code_writer language
    @param klass_name:       wxWidget class name
    @param code_writer:      Code generator class
    @param property_name:    Property name
    @param property_handler: Property handler
    @param widget_name:      Widget name

    @see: L{codegen.BaseLangCodeWriter.add_widget_handler()}
    @see: L{codegen.BaseLangCodeWriter.add_property_handler()}
    """
    codegen = code_writers[lang]
    if codegen:
        codegen.add_widget_handler(klass_name, code_writer)
        if property_name and property_handler:
            codegen.add_property_handler(property_name, property_handler,
                                         widget_name)


def _smart_checksum(content):
    """\
    Generate a "smart" checksum of the given content. The version line
    "generated by wxGlade" as well as tailing whitespaces will ignored during
    generation of the checksum.

    The version line will be ignored within the first ten lines only.

    @param content: Content to generate a checksum for
    @type content:  unicode

    @rtype: str
    """
    assert isinstance(content, types.StringTypes)
    content_list = [x.rstrip() for x in content.splitlines()]

    # use md5 to be compatible with Python 2.4
    chksum = md5.new()

    for line in content_list[:10]:
        if 'generated by wxGlade' in line:
            content_list.remove(line)
            break

    for line in content_list:
        if isinstance(line, types.UnicodeType):
            line = line.encode('utf-8')
        chksum.update(line)

    return chksum.hexdigest()


def save_file(filename, content, which='wxg'):
    """\
    Save I{content} to file named I{filename} and, if user's preferences say
    so and I{filename} exists, makes a backup copy of it.

    @note: The content of 'wxg' files must be Unicode always!

    @note: Exceptions that may occur while performing the operations are not
           handled.

    @see: L{config.backed_up}

    @param filename: Name of the file to create
    @type filename:  str
    @param content:  String to store into 'filename'
    @type content:   str
    @param which:    Kind of backup: 'wxg' or 'codegen'
    @type which:     str
    """
    if which == 'wxg':
        assert isinstance(content, types.UnicodeType)
        do_backup = config.preferences.wxg_backup
    elif which == 'codegen':
        do_backup = config.preferences.codegen_backup
    else:
        raise NotImplementedError(
            'Unknown value "%s" for parameter "which"!' % which
        )

    # read existing file to check content
    chksum_oldcontent = None
    if os.path.isfile(filename):
        oldfile = None
        try:
            oldfile = open(filename)
            oldcontent = oldfile.read()
            oldfile.close()
            chksum_oldcontent = _smart_checksum(oldcontent)
        finally:
            if oldfile:
                oldfile.close()

    # nothing changed?
    chksum_content = _smart_checksum(content)
    if chksum_oldcontent == chksum_content:
        return

    # create the backup file only with the first save
    need_backup = do_backup and filename not in config.backed_up and \
        os.path.isfile(filename)

    outfile = None
    try:
        if need_backup:
            backup_name = filename + config.preferences.backup_suffix
            if os.path.isfile(backup_name):
                os.remove(backup_name)
            os.rename(filename, backup_name)
            config.backed_up[filename] = True

        # create necessary subdirectories on demand
        directory = os.path.dirname(filename)
        if directory and not os.path.isdir(directory):
            os.makedirs(directory)

        outfile = open(filename, 'w')
        if which == 'wxg':
            # encode from unicode to utf-8
            content = content.encode('utf-8')
        outfile.write(content)
        outfile.close()
    finally:
        if outfile:
            outfile.close()


def get_name_for_autosave(filename=None):
    """\
    Return the filename for the automatic backup.

    @param filename: File to generate a backup for
    @type filename: str | None

    @rtype: str
    """
    if not filename:
        filename = app_tree.app.filename
    if not filename:
        path, name = config.home_path, ""
    else:
        path, name = os.path.split(filename)
    ret = os.path.join(path, "#~wxg.autosave~%s#" % name)
    return ret


def autosave_current():
    """\
    Generate a automatic backup for the current and un-saved design.

    @rtype: bool
    """
    if app_tree.app.saved:
        return False         # do nothing in this case...
    try:
        autosave_name = get_name_for_autosave()
        outfile = codecs.open(autosave_name, 'w', 'utf-8')
        app_tree.write(outfile)
        outfile.close()
    except:
        logging.exception(_('Internal Error'))
        return False
    return True


def remove_autosaved(filename=None):
    """\
    Remove the automatic backup

    @see: L{get_name_for_autosave()}
    """
    autosave_name = get_name_for_autosave(filename)
    if os.path.exists(autosave_name):
        try:
            os.unlink(autosave_name)
        except OSError:
            logging.exception(_('Internal Error'))


def check_autosaved(filename):
    """\
    Returns True if there are an automatic backup for filename

    @rtype: bool
    """
    if filename is not None and filename == app_tree.app.filename:
        # this happens when reloading, no auto-save-restoring in this case...
        return False
    autosave_name = get_name_for_autosave(filename)
    try:
        if filename:
            orig = os.stat(filename)
            auto = os.stat(autosave_name)
            return orig.st_mtime < auto.st_mtime
        else:
            return os.path.exists(autosave_name)
    except OSError, e:
        if e.errno != 2:
            logging.exception(_('Internal Error'))
        return False


def restore_from_autosaved(filename):
    """\
    Copy the content of an auto-saved file to the current file. The
    auto-saved file will still remain as a kind of backup.

    @param filename: Original filename to restore to
    @type filename:  str

    @rtype: bool
    """
    autosave_name = get_name_for_autosave(filename)
    if os.access(autosave_name, os.R_OK):
        try:
            content = codecs.open(autosave_name, encoding='UTF-8').read()
            save_file(filename, content, 'wxg')
        except OSError:
            logging.exception(_('Internal Error'))
            return False
        return True
    return False


def init_paths():
    """\
    Set all wxGlade related paths.

    The paths will be stored in L{config}.
    """
    # use directory of the exe in case of frozen packages e.g.
    # PyInstaller or py2exe
    if hasattr(sys, 'frozen'):
        wxglade_path = os.path.dirname(sys.argv[0])
    else:
        wxglade_path = __file__
        if os.path.islink(wxglade_path):
            wxglade_path = os.path.realpath(wxglade_path)
        wxglade_path = os.path.dirname(os.path.abspath(wxglade_path))

    # set the program's paths
    config.wxglade_path = wxglade_path

    share_dir = _get_share_path()
    if _get_install_method() == 'single_directory':
        config.docs_path = os.path.join(share_dir, 'docs')
        config.icons_path = os.path.join(share_dir, 'icons')
        config.templates_path = os.path.join(share_dir, 'templates')
    else:
        config.docs_path = os.path.join(share_dir, 'doc', 'wxglade')
        config.icons_path = os.path.join(share_dir, 'wxglade', 'icons')
        config.templates_path = os.path.join(share_dir, 'wxglade',
                                             'templates')

    _set_home_path()
    _set_appdata_path()
    _set_file_paths()
    _normalise_paths()
    _create_appdata_path()


def _create_appdata_path():
    """\
    Create missing application data directory

    otherwise log initialisation will failed with an IOError
    "No such file or directory".

    The file logger will be initialised after this function returns.
    """
    if not os.path.isdir(config.appdata_path):
        try:
            os.makedirs(config.appdata_path, 0700)
        except (IOError, OSError), e:
            logging.error(_('Failed to create config directory: "%s"'), e)


def _set_appdata_path():
    """\
    Set the path of the application data directory
    """
    if 'WXGLADE_CONFIG_PATH' in os.environ:
        config.appdata_path = os.path.expandvars(
            os.environ['WXGLADE_CONFIG_PATH'])
        return

    if os.name == 'nt' and 'APPDATA' in os.environ:
        path = os.path.expandvars(os.environ['APPDATA'])
        old_name = '%s/.wxglade' % path
        new_name = '%s/wxglade' % path
        if os.path.isdir(new_name):
            path = new_name
        elif os.path.isdir(old_name):
            logging.info(
                _('Rename appdata path from "%s" to "%s"'),
                old_name, new_name)
            try:
                os.rename(old_name, new_name)
                path = new_name
            except (IOError, OSError), e:
                # ignore rename errors and just write an info message
                logging.info(_('Renaming failed: "%s"'), e)
                logging.info(_('Using the old path "%s" instead'), old_name)
                path = old_name
        else:
            path = new_name

        config.appdata_path = path
        return

    if os.name == 'nt':
        path = os.path.join(config.home_path, 'wxglade')
    else:
        path = os.path.join(config.home_path, '.wxglade')

    config.appdata_path = path


def _set_home_path():
    """\
    Set the path of the home directory
    """
    home_dir = os.path.expanduser('~')
    if home_dir not in ('~', '%USERPROFILE%'):
        config.home_path = home_dir
        return

    if os.name == 'nt' and home_dir == '%USERPROFILE%':
        config.home_path = os.environ.get('USERPROFILE', config.wxglade_path)
        return

    config.home_path = config.wxglade_path

    return


def _get_install_method():
    """\
    Return a string indicating the installation method.

    There are two different methods:
      - C{single_directory} - just extract the source into an empty directory
      - C{filesystem_installation} - install the software below /usr resp.
        C:/Program Files

    @rtype: str
    """
    # on Windows or installation in a single directory
    if os.name == 'nt' or \
            os.path.isdir(os.path.join(config.wxglade_path, 'icons')):
        return 'single_directory'
    else:
        return 'filesystem_installation'


def _get_share_path():
    """\
    Return the path of the "share" directory (architecture independent data
    files).

    That's something like "/usr/share" or "/usr/local/share" on Unix or the
    installation directory on Windows.

    @rtype: str
    @see: L{_get_install_method()}
    """

    # all in a single directory (extract and run)
    if _get_install_method() == 'single_directory':
        share_dir = config.wxglade_path

    # alternative installation path
    else:
        assert config.wxglade_path.endswith('wxglade')
        # split path into single components to check the last four elements
        dir_list = split_path(os.path.normpath(config.wxglade_path))
        if len(dir_list) > 4 and dir_list[-1] == 'wxglade' and \
            dir_list[-2] in ['site-packages', 'dist-packages'] and \
            dir_list[-3].startswith('python') and \
            dir_list[-4].startswith('lib'):
            share_dir = os.path.join(*dir_list[:-4])
            share_dir = os.path.join(share_dir, 'share')
        else:
            logging.error(
                _('Unknown path structure %s'), config.wxglade_path)
            share_dir = ''

    if not share_dir:
        logging.error(_('Share directory not found'))
    elif not os.path.exists(share_dir):
        logging.error(
            _('Share directory "%s" does not exists'), share_dir)
    elif not os.path.isdir(share_dir):
        logging.error(
            _('Share directory "%s" is not a directory'), share_dir)

    return share_dir


def split_path(path):
    """\
    Split the path into single components.

    B{Example}::
        >>> split_path('/usr/local/share')
        ['/', 'usr', 'local', 'share']

    @rtype: list[str]
    """
    drive, path = os.path.splitdrive(path)
    components = []
    while True:
        path, tail = os.path.split(path)
        if tail:
            components.append(tail)
        else:
            if path:
                components.append(path)
            break
    components.reverse()
    if drive:
        components.insert(0, drive)
    return components


def _normalise_paths():
    """\
    Normalise all paths stored in config module
    """
    for name in ['appdata_path', 'credits_file', 'docs_path',
                 'history_file', 'home_path', 'icons_path', 'license_file',
                 'manual_file', 'rc_file', 'templates_path',
                 'tutorial_file', 'widgets_path', 'wxglade_path']:
        assert hasattr(config, name)
        path = getattr(config, name)
        path = os.path.normpath(path)
        setattr(config, name, path)


def _set_file_paths():
    """\
    Set the full path for all files (config.*_file except default_output_file)
    """
    install_method = _get_install_method()
    if install_method == 'single_directory':
        config.credits_file = os.path.join(config.wxglade_path, 'CREDITS.txt')
        config.license_file = os.path.join(config.wxglade_path, 'LICENSE.txt')
        config.manual_file = os.path.join(config.docs_path, 'html', 'index.html')
        config.tutorial_file = os.path.join(config.docs_path, 'tutorial.html')
    else:
        config.credits_file = os.path.join(config.docs_path, 'CREDITS.txt')
        config.license_file = os.path.join(config.docs_path, 'LICENSE.txt')
        config.manual_file = os.path.join(config.docs_path,
                                          'manual_html', 'index.html')
        config.tutorial_file = os.path.join(config.docs_path,
                                            'tutorial', 'tutorial.html')

    if not os.path.exists(config.credits_file):
        logging.error(_('Credits file "CREDITS.txt" not found!'))
        config.credits_file = ''
    if not os.path.exists(config.license_file):
        logging.error(_('License file "LICENSE.txt" not found!'))
        config.license_file = ''

    config.widgets_path = os.path.join(config.wxglade_path, 'widgets')

    # complete path to rc file
    if os.name == 'nt':
        config.rc_file = os.path.join(config.appdata_path, 'wxglade.ini')
    else:
        config.rc_file = os.path.join(config.appdata_path, 'wxgladerc')
    config.history_file = os.path.join(
        config.appdata_path, 'file_history.txt'
    )
    config.log_file = os.path.join(config.appdata_path, 'wxglade.log')


def init_preferences():
    """\
    Load / initialise preferences
    """
    if config.preferences is None:
        config.preferences = Preferences()
        config.preferences.read(config.rc_file)
        if not config.preferences.has_section('wxglade'):
            config.preferences.add_section('wxglade')


def save_preferences():
    """\
    Save current settings as well as the file history

    @see: L{config.history_file}
    @see: L{config.use_file_history}
    """
    # let the exception be raised
    path = os.path.dirname(config.rc_file)
    if not os.path.isdir(path):
        os.makedirs(path)
        # always save the file history
    if config.use_file_history:
        content = u'# -*- coding: utf-8 -*-\n'
        for pos in range(min(config.preferences.number_history,
                             palette.file_history.GetCount())):
            content += u'%s\n' % palette.file_history.GetHistoryFile(pos)
        outfile = codecs.open(config.history_file, 'w', encoding='utf-8')
        outfile.write(content)
        outfile.close()
    if config.preferences.changed:
        outfile = open(config.rc_file, 'w')
        # let the exception be raised to signal abnormal behaviour
        config.preferences.write(outfile)
        outfile.close()


def load_history():
    """\
    Loads the file history and returns a list of paths

    @see: L{config.history_file}
    @see: L{config.use_file_history}

    @rtype: list[unicode]
    """
    try:
        infile = codecs.open(config.history_file, encoding='utf-8')
        lines = infile.readlines()
        if lines and lines[0].startswith(u'# -*- coding:'):
            lines = lines[1:]
        infile.close()
        return lines
    except IOError:
        # don't consider this an error
        return []


class Preferences(ConfigParser.ConfigParser):
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
        'remember_geometry': True,
        'local_widget_path': '',
        'default_border': False,
        'default_border_size': 3,
        'show_sizer_handle': True,
        'allow_duplicate_names': False,
        'autosave': True,
        'autosave_delay': 120,  # in seconds
        'show_completion': True,
        'write_timestamp': True,
        'write_generated_from': False,
        'log_debug_info': False,
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
                config.appdata_path, 'widgets'
                )
        self.def_vals = defaults
        if self.def_vals is None:
            self.def_vals = Preferences._defaults
        self.changed = False
        ConfigParser.ConfigParser.__init__(self)

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
        """\
        Save the current widget position and size

        @param name: Widget name
        @type name: str

        @param geometry: Position and Size
        @type geometry: (int, int, int, int)
        """
        if geometry is not None:
            section = 'geometry_%s' % name
            if not self.has_section(section):
                self.add_section(section)
            self.set(section, 'x', geometry[0])
            self.set(section, 'y', geometry[1])
            self.set(section, 'w', geometry[2])
            self.set(section, 'h', geometry[3])

    def get_geometry(self, name):
        """\
        Return saved widget position and size.

        @param name: Widget name
        @type name: str

        @rtype: (int, int, int, int) | None
        """
        section = 'geometry_%s' % name
        if not self.has_section(section):
            return None
        try:
            x = int(self.get(section, 'x'))
            y = int(self.get(section, 'y'))
            w = int(self.get(section, 'w'))
            h = int(self.get(section, 'h'))
            return x, y, w, h
        except:
            pass
        return None

# end of class Preferences


def style_attrs_to_sets(styles):
    """\
    Convert the style attributes 'combination', 'exclude', 'include' and
    'require' from string to a set.

    @param styles: Style dictionary
    @type styles: dict

    @return: Style dictionary with modified attributes
    @rtype: dict
    """
    for style_name in styles.keys():
        for attr in ['combination', 'exclude', 'include', 'require', ]:
            try:
                styles[style_name][attr] = \
                    set(styles[style_name][attr].split('|'))
            except (AttributeError, KeyError):
                pass

    return styles


def format_xml_tag(tag, value, indentlevel=0, **kwargs):
    r"""
    Generate a valid XML tag. The content will be proper escaped and quoted.

    Example::
        >>> common.format_xml_tag(u'label', 'Exit', 1)
        u'    <label>Exit</label\n'

    @note: The returned statement has a tailing newline character

    @param tag: XML tag
    @type tag: str | Unicode

    @param value: Content of the XML tag
    @type value: str | Unicode

    @param indentlevel: Indention level, Each level are four spaces
    @type indentlevel:  int

    @keyword is_xml: Content of value argument is already escaped
                     and formatted XML
    @type is_xml: bool

    @param kwargs: XML attributes to include (see L{format_xml_attrs()} too)

    @rtype: str

    @see: L{format_xml_attrs()}
    """
    assert isinstance(tag, types.StringTypes)
    assert isinstance(indentlevel, types.IntType)

    is_xml = kwargs.get('is_xml', False)
    if 'is_xml' in kwargs:
        del kwargs['is_xml']

    tabs = u'    ' * indentlevel
    tag = encode_to_unicode(tag)

    if not is_xml:
        value = encode_to_unicode(value)
        value = escape(value)

    attrs = format_xml_attrs(**kwargs)
    if attrs:
        attrs = u' %s' % attrs

    if is_xml:
        tmpl = u'%(tabs)s<%(tag)s%(attrs)s>\n%(value)s%(tabs)s</%(tag)s>\n'
    elif not value:
        tmpl = u'%(tabs)s<%(tag)s%(attrs)s />\n'
    else:
        tmpl = u'%(tabs)s<%(tag)s%(attrs)s>%(value)s</%(tag)s>\n'

    stmt = tmpl % {'attrs': attrs, 'tabs': tabs, 'tag': tag,
                   'value': value, }

    return stmt


def format_xml_attrs(**kwargs):
    """\
    Format the given keyword arguments to use as XML attributes.
    The arguments will be escaped and quoted.

    The returned Unicode string doesn't contain leading and tailing spaces.

    Example::
        >>> common.format_xml_attrs(base='EditFrame', name='Frame_1')
        u'base="EditFrame" name="Frame1"'

    @param kwargs: Attributes to process

    @return: Processed and joined kwargs
    @rtype: Unicode
    """
    if not kwargs:
        return u''

    attr_list = []
    for name in kwargs:
        value = encode_to_unicode(kwargs[name])
        value = quoteattr(value)
        value = value.strip()
        attr_list.append(u'%s=%s' % (name, value))

    attr_list.sort()
    res = u' '.join(attr_list)
    res = res.strip()
    return res
