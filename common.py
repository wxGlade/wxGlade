"""\
Global functions and variables

@copyright: 2002-2007 Alberto Griggio
@copyright: 2013-2016 Carsten Grohmann
@copyright: 2016-2023 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import codecs, errno
try:
    # Python 2
    import ConfigParser
    from md5 import new as md5
except:
    # Python 3
    import configparser as ConfigParser
    from hashlib import md5
from collections import OrderedDict

import logging, os, os.path, sys, tempfile
from xml.sax.saxutils import escape, quoteattr

import config, compat, plugins, misc


widget_classes = {}   # EditWidget class name -> EditWidget class
widgets = {}          # all widgets: EditWidget class name -> factory(parent, index)
widgets_from_xml = {} # Factory functions to build objects from a XML file

class_names = {} # maps the name of the classes used by wxGlade to the correspondent classes of wxWindows

# references to windows:
main = None            # main window
palette = None         # the panel which contains the various buttons to add the different widgets
property_panel = None  # panel for editing the current widgets properties
app_tree = None        # widget hierarchy of the application; root is application itself; a tree.WidgetTree instance
shell = None           # will be created only when selecting from the help menu
root = None

# these will be set when clicking an item on the palette window:
adding_widget = False # If True, the user is adding a widget to some sizer
adding_sizer = False  # "Needed to add toplevel sizers"
widget_to_add = None  # widget class name that is being added
adding_window = None  # the tree or the design window; used for centering dialogs

pin_design_window = False

# Dictionary which maps the ids used in the event handlers to the corresponding widgets:
# used to call the appropriate builder function when a dropping of a widget occurs, knowing only the id of the event
refs = {}

history = None

########################################################################################################################
# application initialization

# Dictionary of language name -> BaseLangCodeWriter objects used to generate the code in a given language.
code_writers = {}


def init_codegen():
    """Load available code generators, built-in and user widgets as well as sizers

    Returns OrderedDict with module sections as key and assigned list of wxBitmapButtons in GUI mode.
    The dict is empty in batch mode.

    see: load_config() load_code_writers(), load_widgets(), load_sizers()"""
    # process generic related style attributes
    style_attrs_to_sets(config.widget_config['generic_styles'])
    load_config()
    load_code_writers()
    all_widgets = load_widgets()
    sizer_buttons = load_sizers()

    # initialize preview code generator
    preview_codegen = code_writers["preview"] = code_writers["python"].copy()
    preview_codegen.for_version = compat.version

    # merge sizer buttons
    for section in sizer_buttons:
        if section not in all_widgets:
            all_widgets[section] = sizer_buttons[section]
        else:
            all_widgets[section].extend(sizer_buttons[section])

    return all_widgets


def load_code_writers():
    "Fills the common.code_writers dictionary: to do so, loads the modules found in the 'codegen/' subdir"
    if config.use_gui:
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
        except (AttributeError, ImportError, NameError, SyntaxError, ValueError):
            logging.exception( _('"%s" is not a valid code generator module'), module )
        else:
            code_writers[writer.language] = writer
            if config.use_gui:
                logging.info( _('  %s generator loaded'), writer.language )


def load_config():
    "Load widget configuration;  see: plugins.load_widgets_from_dir()"
    # load the "built-in" and "user" widgets
    plugins.load_widgets_from_dir( config.widgets_path,                  'wconfig' )
    plugins.load_widgets_from_dir( config.preferences.local_widget_path, 'wconfig' )

    return


def load_sizers():
    """Load and initialise the sizer support modules into ordered dict instance.
    See edit_sizers.edit_sizers.init_all."""
    if config.use_gui:
        logging.info('Load sizer generators:')
    for lang in code_writers.keys():
        module_name = 'edit_sizers.%s_sizers_codegen' % code_writers[lang].lang_prefix
        try:
            sizer_module = plugins.import_module(config.wxglade_path, module_name)
            if not sizer_module:
                # error already logged
                pass
            elif hasattr(sizer_module, 'initialize'):
                sizer_module.initialize()
            else:
                logging.warning(
                    _('Missing function "initialize()" in imported module %s. Skip initialisation.'), module_name)

            if config.use_gui:
                logging.info(_('  for %s'), lang)

        except (AttributeError, ImportError, NameError, SyntaxError, ValueError):
            logging.exception( _('ERROR loading module "%s"'), module_name )
        except:
            logging.exception( _('Unexpected error during import of widget module %s'), module_name )

    # initialise sizer GUI elements
    import edit_sizers
    return edit_sizers.init_gui()


def load_widgets():
    """Load built-in and user widgets.

    Scans the built-in and user widget directories to find the installed widgets and loads it.

    returns OrderedDict

    see: plugins.load_widgets_from_dir() for more details e.g. the structure of the dictionary."""
    # load the "built-in" and "user" widgets
    core_buttons  = plugins.load_widgets_from_dir(config.widgets_path, default_section=_('Core widgets'))
    local_buttons = plugins.load_widgets_from_dir(config.preferences.local_widget_path,
                                                  default_section=_('Custom widgets'))

    # load (remaining) widget code generators
    # Python, C++ and XRC are often loaded via plugins.load_widgets_from_dir() above
    for path in [config.widgets_path, config.preferences.local_widget_path]:
        for lang in ['perl', 'lisp']:
            if lang not in code_writers:
                continue
            codegen_name = '%s_codegen' % code_writers[lang].lang_prefix
            plugins.load_widgets_from_dir( path, submodule=codegen_name )

    all_widgets = OrderedDict()
    all_widgets.update(core_buttons)

    for section in local_buttons:
        if section not in all_widgets:
            all_widgets[section] = local_buttons[section]
        else:
            all_widgets[section].extend(local_buttons[section])

    return all_widgets


########################################################################################################################
# user interaction

def add_object(event):
    "Adds a widget or a sizer to the current app"
    global adding_widget, adding_sizer, widget_to_add
    tmp = event.GetId()
    widget_to_add = refs[tmp]
    adding_widget = True
    adding_sizer = "Sizer" in widget_to_add

    btn = event.GetEventObject()
    if compat.version >= (3,0):
        btn.SetValue(True)
    palette.reset_togglebuttons(keep=btn)

    msg = "Adding %s; click on free (hatched) sizer slot to place it"
    main.user_message( msg%widget_to_add.lstrip("Edit") )


def add_toplevel_object(event):
    "Adds a toplevel widget (Frame or Dialog) to the current app"
    palette.reset_togglebuttons()
    editor = widgets[refs[event.GetId()]](root, 0)
    if editor is None: return
    history.widget_adding(root)
    history.widget_added(editor)
    misc.rebuild_tree(widget=editor, recursive=editor.children, focus=True)


########################################################################################################################
# application GUI initialization

def make_object_button(widget, icon_path, toplevel=False, tip=None):
    """Creates a button for the widgets toolbar.

    Function used by the various widget modules to add a button to the widgets toolbar.

    Icons with a relative path will be loaded from config.icon_path.

    widget: (name of) the widget the button will add to the app 
    icon_path: Path to the icon_path used for the button
    toplevel: True if the widget is a toplevel object (frame, dialog)
    tip: Tool tip to display

    return: The newly created wxBitmapButton instance"""
    if not config.use_gui: return None
    import wx
    import misc
    from tree import WidgetTree

    if not os.path.isabs(icon_path):
        icon_path = os.path.join(config.icons_path, icon_path)
    bmp = misc.get_xpm_bitmap(icon_path)
    label = widget.replace('Edit', '')
    if compat.version < (3,0):
        # old wx version: use BitmapButton
        tmp = wx.BitmapButton(palette, -1, bmp, size=(31,31))
        if not toplevel:
            tmp.Bind(wx.EVT_BUTTON, add_object)
        else:
            tmp.Bind(wx.EVT_BUTTON, add_toplevel_object)
    else:
        # for more recent versions, we support config options to display icons and/or labels
        if not toplevel:
            if not config.preferences.show_palette_labels:
                # icons only -> set size
                tmp = wx.ToggleButton(palette, -1, size=(31,31), name=label)
            else:
                tmp = wx.ToggleButton(palette, -1, label, name=label )
            tmp.Bind(wx.EVT_TOGGLEBUTTON, add_object)
        else:
            if not config.preferences.show_palette_labels:
                tmp = wx.Button(palette, -1, size=(31,31), name=label)
            else:
                tmp = wx.Button(palette, -1, label, name=label )
            tmp.Bind(wx.EVT_BUTTON, add_toplevel_object)
        if config.preferences.show_palette_icons:
            tmp.SetBitmap( bmp )
    refs[tmp.GetId()] = widget
    if not tip:
        tip = _('Add a %s') % label
    tmp.SetToolTip(wx.ToolTip(tip))

    WidgetTree.images[widget] = icon_path

    return tmp


def encode_to_unicode(item, encoding=None):
    """Decode the item to a Unicode string. The encoding to UTF-8 will be done later.

    Non-string items will be converted to string automatically.
    If no encoding given, root.encoding or 'UFT-8' will be used."""
    if not isinstance(item, compat.basestring):
        item = str(item)
    if isinstance(item, compat.unicode):
        return item
    encoding = encoding or (app_tree and root.encoding) or "UTF-8"
    item = item.decode(encoding)
    return item


def register(lang, klass_name, code_writer):
    """Initialise and register widget code generator instance.

    lang:             Code code_writer language
    klass_name:       wxWidget class name
    code_writer:      Code generator class

    see: codegen.BaseLangCodeWriter.register_widget_code_generator()"""
    codegen = code_writers[lang]
    if codegen:
        codegen.register_widget_code_generator(klass_name, code_writer)


########################################################################################################################
# file utilities

def _smart_checksum(content):
    """Generate a "smart" checksum of the given content. The version line "generated by wxGlade" as well as tailing
    whitespaces will ignored during generation of the checksum. Returns a strings.

    The version line will be ignored within the first ten lines only.

    content: Content to generate a checksum for; list of bytes"""
    chksum = md5()  # use md5 to be compatible with Python 2.4

    for i,line in enumerate(content):
        if isinstance(line, compat.unicode):
            line = line.encode('utf-8')
        if b'generated by wxGlade' in line and i<10: continue
        chksum.update(line.rstrip())

    return chksum.hexdigest()


def _read_file(filename):
    "read file into a list of lines (bytes); line ending is normalized to \n"
    ret = []
    with open(filename, "rb") as f:
        for line in f.readlines():
            if line.endswith(b"\r\n"): line = line[:-2]+b"\n"
            yield line


def save_file(filename, content, which='wxg'):
    """Save content to named file and, if user's preferences say so and filename exists, makes a backup copy of it.

    The content of 'wxg' files must be Unicode always!
    Exceptions that may occur while performing the operations are not handled.

    see: config.backed_up

    filename: Name of the file to create
    content:  list of strings to store into 'filename'
    which:    Kind of backup: 'wxg' or 'codegen'"""
    if which == 'wxg':
        content = [line.encode('utf-8') for line in content] # encode from unicode to utf-8
        do_backup = config.preferences.wxg_backup
    elif which == 'codegen':
        do_backup = config.preferences.codegen_backup
    else:
        raise NotImplementedError( 'Unknown value "%s" for parameter "which"!' % which )

    if os.path.isfile(filename):
        # read existing file to check content
        chksum_oldcontent = _smart_checksum( _read_file(filename) )

        # nothing changed?
        chksum_content = _smart_checksum(content)
        if chksum_oldcontent == chksum_content:
            return

    # create the backup file only with the first save
    need_backup = do_backup and filename not in config.backed_up and os.path.isfile(filename)

    outfile = None
    try:
        if which=="codegen":
            if os.path.isfile(filename):
                with open(filename, 'rb') as infile:
                    win_line_ending = infile.readline().endswith(b"\r\n")
            else:
                win_line_ending = sys.platform.startswith("win")

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

        outfile = open(filename, 'wb')
        for line in content:
            if which=="codegen" and win_line_ending:
                line = line.replace(b"\n", b"\r\n")
            outfile.write(line)
        outfile.close()
    finally:
        if outfile:
            outfile.close()


########################################################################################################################
# files and paths

def get_name_for_autosave(filename=None):
    "Return filename for the automatic backup of named file or current file (root.filename)"
    if not filename:
        filename = root.filename
    if not filename:
        path, name = config.home_path, ""
    else:
        path, name = os.path.split(filename)
    ret = os.path.join(path, "#~wxg.autosave~%s#" % name)
    return ret


class _Writer(object):
    # file with a list compatible interface: append and extend
    def __init__(self, filename):
        self.outfile = codecs.open(filename, 'w', 'utf-8')
    def append(self, line):
        self.outfile.write(line)
    def extend(self, lines):
        for line in lines: self.outfile.write(line)
    def close(self):
        self.outfile.close()


def autosave_current():
    "Save automatic backup copy for the current and un-saved design;  returns 0: error; 1: no changes to save; 2: saved"
    if root.saved:
        return 1            # do nothing in this case...

    autosave_name = get_name_for_autosave()
    try:
        outfile = _Writer(autosave_name)
        root.write(outfile)
        outfile.close()
    except EnvironmentError as details:
        logging.warning( _('Saving the autosave file "%s" failed: %s'), autosave_name, details )
        return 0
    return 2


def remove_autosaved(filename=None):
    "Remove the automatic backup;  see: get_name_for_autosave()"
    autosave_name = get_name_for_autosave(filename)
    if os.path.exists(autosave_name):
        try:
            os.unlink(autosave_name)
        except EnvironmentError:
            logging.exception(_('Internal Error'))


def check_autosaved(filename):
    "Returns True if there are an automatic backup for filename"
    if filename is not None and filename == root.filename:
        # this happens when reloading, no auto-save-restoring in this case...
        return False
    autosave_name = get_name_for_autosave(filename)
    try:
        if filename:
            orig = os.stat(filename)
            auto = os.stat(autosave_name)
            if orig.st_mtime > auto.st_mtime: return False
        else:
            if not os.path.exists(autosave_name): return False
            auto = os.stat(autosave_name)
        # check contents for empty or incomplete file
        if auto.st_size < 50: return False
        f = open(autosave_name, "rb")
        f.seek(-16,2)  # from the end
        file_end = f.read(16)
        f.close()
        return b"</application>" in file_end
    except EnvironmentError as inst:
        # File doesn't exists
        if inst.errno == errno.ENOENT:
            pass
        # Security frameworks like SELinux may deny the write access even if
        # the check for write permissions was successful.
        elif inst.errno in [errno.EPERM, errno.EACCES]:
            logging.info(
                _('Ignore autosave permission error: %s'), str(inst))
        else:
            logging.exception(_('Internal Error'))
        return False


def restore_from_autosaved(filename):
    """Copy the content of an auto-saved file to the current file.
    The auto-saved file will still remain as a kind of backup.
    Returns True on success."""

    autosave_name = get_name_for_autosave(filename)
    if os.access(autosave_name, os.R_OK):
        try:
            content = codecs.open(autosave_name, encoding='UTF-8').read()
            save_file(filename, content, 'wxg')
        except EnvironmentError:
            logging.exception(_('Internal Error'))
            return False
        return True
    return False


def init_paths(options):
    "Set all wxGlade related paths; the paths will be stored in config."
    # use directory of the exe in case of frozen packages e.g. PyInstaller or py2exe
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
        config.docs_path      = os.path.join(share_dir, 'docs')
        config.icons_path     = os.path.join(share_dir, 'icons')
        config.templates_path = os.path.join(share_dir, 'templates')
    else:
        config.docs_path      = os.path.join(share_dir, 'doc', 'wxglade')
        config.icons_path     = os.path.join(share_dir, 'wxglade', 'icons')
        config.templates_path = os.path.join(share_dir, 'wxglade', 'templates')

    _set_home_path()
    _set_appdata_path()
    _set_file_paths(options)
    _normalise_paths()
    _create_appdata_path()


def _create_appdata_path():
    """Create missing application data directory.
    Otherwise log initialisation will fail with an IOError "No such file or directory".
    The file logger will be initialised after this function returns."""
    if not os.path.isdir(config.appdata_path):
        try:
            os.makedirs(config.appdata_path, 0o700)
        except EnvironmentError as e:
            logging.error(_('Failed to create config directory: "%s"'), e)


def _set_appdata_path():
    "Set the path of the application data directory"
    if 'WXGLADE_CONFIG_PATH' in os.environ:
        config.appdata_path = os.path.expandvars( os.environ['WXGLADE_CONFIG_PATH'] )
        return

    if os.name == 'nt' and 'APPDATA' in os.environ:
        path = os.path.expandvars(os.environ['APPDATA'])
        old_name = '%s/.wxglade' % path
        new_name = '%s/wxglade' % path
        if os.path.isdir(new_name):
            path = new_name
        elif os.path.isdir(old_name):
            logging.info( _('Rename appdata path from "%s" to "%s"'), old_name, new_name)
            try:
                os.rename(old_name, new_name)
                path = new_name
            except EnvironmentError as e:
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
    "Set the path of the home directory"
    home_path = os.path.expanduser('~')

    # to prevent unexpanded "%HOMEDRIVE%%HOMEPATH%" as reported in SF Bug #185
    home_path = os.path.expandvars(home_path)

    if home_path == '~':
        home_path = tempfile.gettempdir()
        logging.info( _('Expansion of the home directory shortcut "~" failed. Use temp directory "%s" instead'),
                      home_path )

    if not os.path.isdir(home_path):
        tmp_dir = tempfile.gettempdir()
        logging.info( _('The home path "%s" is not a directory. Use the temp directory "%s" instead.'),
                      home_path, tmp_dir )
        home_path = tmp_dir

    if not os.access(home_path, os.W_OK):
        logging.info(_('The home path "%s" is not writable.'), home_path)
        tmp_dir = tempfile.gettempdir()
        logging.info(_('The home path "%s" is not writable. Use the temp directory "%s" instead.'), home_path, tmp_dir)
        home_path = tmp_dir

    config.home_path = home_path
    return


def _get_install_method():
    """Return a string indicating the installation method as string:
      - 'single_directory' - just extract the source into an empty directory
      - 'filesystem_installation' - install the software below /usr resp. C:/Program Files"""
    # on Windows or installation in a single directory
    if os.name == 'nt' or os.path.isdir(os.path.join(config.wxglade_path, 'icons')):
        return 'single_directory'
    else:
        return 'filesystem_installation'


def _get_share_path():
    """Return the path of the "share" directory (architecture independent data files).
    That's something like "/usr/share" or "/usr/local/share" on Unix or the installation directory on Windows.
    see: _get_install_method()"""

    # all in a single directory (extract and run)
    if _get_install_method() == 'single_directory':
        share_dir = config.wxglade_path

    # alternative installation path
    else:
        assert config.wxglade_path.endswith('wxglade')
        # split path into single components to check the last four elements
        dir_list = split_path(os.path.normpath(config.wxglade_path))
        if len(dir_list) > 4 and dir_list[-1] == 'wxglade' and dir_list[-2] in ['site-packages', 'dist-packages'] and \
            dir_list[-3].startswith('python') and dir_list[-4].startswith('lib'):
            share_dir = os.path.join(*dir_list[:-4])
            share_dir = os.path.join(share_dir, 'share')
        elif len(dir_list) > 4 and dir_list[-1] == 'wxglade' and dir_list[-2].endswith('.egg'):
            # egg installation
            share_dir = os.path.join(*dir_list[:-1])
            share_dir = os.path.join(share_dir, 'share')
        else:
            logging.error(_('Unknown path structure %s'), config.wxglade_path)
            share_dir = ''

    if not share_dir:
        logging.error(_('Share directory not found'))
    elif not os.path.exists(share_dir):
        logging.error(_('Share directory "%s" does not exists'), share_dir)
    elif not os.path.isdir(share_dir):
        logging.error(_('Share directory "%s" is not a directory'), share_dir)

    return share_dir


def split_path(path):
    "Split the path into single components; e.g. split_path('/usr/local/share') -> ['/', 'usr', 'local', 'share']"
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
    "Normalise all paths stored in config module"
    for name in ['appdata_path', 'credits_file', 'docs_path', 'history_file', 'home_path', 'icons_path', 'license_file',
                 'manual_file', 'rc_file', 'templates_path', 'tutorial_file', 'widgets_path', 'wxglade_path']:
        assert hasattr(config, name)
        path = getattr(config, name)
        path = os.path.normpath(path)
        setattr(config, name, path)


def _set_file_paths(options):
    "Set the full path for all files (config.*_file except default_output_file)"
    install_method = _get_install_method()
    if install_method == 'single_directory':
        config.credits_file  = os.path.join(config.wxglade_path, 'CREDITS.txt')
        config.license_file  = os.path.join(config.wxglade_path, 'LICENSE.txt')
        config.manual_file   = os.path.join(config.docs_path, 'html', 'index.html')
        config.bmp_manual_file = os.path.join(config.docs_path, 'html', 'bitmaps.html')
        #config.tutorial_file = os.path.join(config.docs_path, 'Tutorial.html')
    else:
        config.credits_file  = os.path.join(config.docs_path, 'CREDITS.txt')
        config.license_file  = os.path.join(config.docs_path, 'LICENSE.txt')
        config.manual_file   = os.path.join(config.docs_path, 'html', 'index.html')
        config.bmp_manual_file = os.path.join(config.docs_path, 'html', 'bitmaps.html')
        #config.tutorial_file = os.path.join(config.docs_path, 'html', 'tutorial.html')

    if not os.path.exists(config.credits_file):
        logging.error(_('Credits file "CREDITS.txt" not found!'))
        config.credits_file = ''
    if not os.path.exists(config.license_file):
        logging.error(_('License file "LICENSE.txt" not found!'))
        config.license_file = ''

    config.widgets_path = os.path.join(config.wxglade_path, 'widgets')

    # complete path to rc file
    if options and options.rc_file:
        if not os.path.exists(options.rc_file):
            logging.error(_('Specified config file does not exist'))
        config.rc_file = options.rc_file
    elif os.name == 'nt':
        config.rc_file = os.path.join(config.appdata_path, 'wxglade.ini')
    else:
        config.rc_file = os.path.join(config.appdata_path, 'wxgladerc')
    config.history_file = os.path.join(config.appdata_path, 'file_history.txt')
    config.log_file = os.path.join(config.appdata_path, 'wxglade.log')


def init_preferences():
    "Load / initialise preferences"
    if config.preferences is not None: return
    config.preferences = Preferences()
    try:
        config.preferences.read(config.rc_file)
    except:
        msg = ("Could not load preferences from file '%s'.\nContinuing with default settings.\n"
               "Go to Edit->Preferences and save preferences with 'OK'.")
        misc.error_message(msg%config.rc_file, title="Warning", display_traceback=True)
    if not config.preferences.has_section('wxglade'):
        config.preferences.add_section('wxglade')


def save_preferences():
    "Save current settings as well as the file history; see: config.history_file and config.use_file_history"
    # let the exception be raised
    path = os.path.dirname(config.rc_file)
    if not os.path.isdir(path):
        os.makedirs(path)
        # always save the file history
    if config.use_file_history:
        content = u'# -*- coding: utf-8 -*-\n'
        for pos in range(min(config.preferences.number_history,
                             main.file_history.GetCount())):
            content += u'%s\n' % main.file_history.GetHistoryFile(pos)
        outfile = codecs.open(config.history_file, 'w', encoding='utf-8')
        outfile.write(content)
        outfile.close()
    if config.preferences.changed:
        outfile = open(config.rc_file, 'w')
        # let the exception be raised to signal abnormal behaviour
        config.preferences.write(outfile)
        outfile.close()


def load_file_history():
    "Loads the file history and returns a list of paths; see: config.history_file and config.use_file_history"
    try:
        infile = codecs.open(config.history_file, encoding='utf-8')
        lines = infile.readlines()
        if lines and lines[0].startswith(u'# -*- coding:'):
            lines = lines[1:]
        infile.close()
        return lines
    except EnvironmentError:
        # don't consider this an error
        return []


class Preferences(ConfigParser.ConfigParser):
    _defaults = {
        'open_save_path': '',
        'codegen_path': '',
        'use_dialog_units': False,
        'allow_custom_widgets':False,
        'number_history': 12,
        'font_scale_tree': 1.0,
        'show_progress': True,
        'wxg_backup': True,
        'codegen_backup': True,
        'backup_suffix': sys.platform == 'win32' and '.bak' or '~',
        'remember_geometry': True,
        'local_widget_path': '',
        'default_border': False,
        'default_border_size': 3,
        'show_sizer_handle': True,
        'show_palette_icons': True,
        'show_palette_labels': False,
        'show_gridproperty_editors': False,
        'use_checkboxes_workaround': False,
        'no_checkbox_label_colours':False,
        'allow_duplicate_names': False,
        'autosave': True,
        'autosave_delay': 120,  # in seconds
        'show_completion': True,
        'write_timestamp': True,
        'write_generated_from': False
        }

    def __init__(self, defaults=None):
        # set defaults of 'codegen_path', 'local_widget_path', and 'open_save_path' and 'codegen_path' if the class is
        # instantiated first time, because the home_path is set later
        if config.home_path and not self._defaults['open_save_path']:
            self._defaults['open_save_path'] = config.home_path
            self._defaults['codegen_path'] = config.home_path
        if config.appdata_path and not self._defaults['local_widget_path']:
            self._defaults['local_widget_path'] = os.path.join( config.appdata_path, 'widgets' )
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
        # ...but I haven't found a better way: the problem is that bool('0') == True, while int('0') == False,
        # and we want the latter behaviour
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

    def set_struct(self, section, option, value):
        # recursively store a dictionary
        if not self.has_section(section):
            self.add_section(section)
        if not isinstance(option, list):
            option = [option]
        if isinstance(value, dict):
            for key in sorted(value.keys()):
                self.set_struct(section, option+[key], value[key])
        elif isinstance(value, list):
            for i,item in enumerate(value):
                self.set_struct(section, option+["l%d"%i], item)
        elif isinstance(value, tuple):
            for i,item in enumerate(value):
                self.set_struct(section, option+["t%d"%i], item)
        else:
            option = "_".join(option)
            self.set(section, option, str(value))
    def set_dict(self, section, value):
        self.set_struct(section, [], value)

    def get_int(self, section, option):
        return int(self.get(section, option))


########################################################################################################################
# XML utilities

def style_attrs_to_sets(styles):
    """Convert the style attributes 'combination', 'exclude', 'include' and 'require' from string to a set.

    styles: Style dictionary

    returns: Style dictionary with modified attributes"""
    for style_name in styles.keys():
        for attr in ['combination', 'exclude', 'include', 'require', 'disabled']:
            try:
                styles[style_name][attr] = set(styles[style_name][attr].split('|'))
            except (AttributeError, KeyError):
                pass

    return styles


def format_xml_tag(tag, value, indentlevel=0, **kwargs):
    r"""Generate a valid XML tag as string. The content will be proper escaped and quoted.

    Example::
        >>> common.format_xml_tag(u'label', 'Exit', 1)
        u'    <label>Exit</label\n'

    The returned statement has a tailing newline character

    tag: XML tag name
    value: Content of the XML tag as string or unicode
    indentlevel: Indention level as int, Each level are four spaces
    is_xml: Content of value argument is already escaped and formatted XML
    kwargs: XML attributes to include (see Lformat_xml_attrs() too)

    see format_xml_attrs()"""
    assert isinstance(tag, compat.basestring)
    assert isinstance(indentlevel, int)

    is_xml = kwargs.get('is_xml', False)
    if 'is_xml' in kwargs:
        del kwargs['is_xml']

    tabs = u'    ' * indentlevel
    tag = encode_to_unicode(tag)

    if not is_xml:
        value = escape( encode_to_unicode(value) )

    attrs = format_xml_attrs(**kwargs)
    if attrs: attrs = u' %s' % attrs

    data = {'attrs': attrs, 'tabs': tabs, 'tag': tag, 'value': value }  # for the format string

    if is_xml:
        tmpl1 = u'%(tabs)s<%(tag)s%(attrs)s>\n'
        tmpl2 = u'%(tabs)s</%(tag)s>\n'
        return [tmpl1%data] + value + [tmpl2%data]
    if not value:
        tmpl = u'%(tabs)s<%(tag)s%(attrs)s />\n'
    else:
        tmpl = u'%(tabs)s<%(tag)s%(attrs)s>%(value)s</%(tag)s>\n'
    return [tmpl%data]


def format_xml_prop(tag, value, indentlevel=0, **kwargs):
    # format a single property as indented string
    assert isinstance(tag, compat.basestring)
    assert isinstance(indentlevel, int)

    tabs = u'    ' * indentlevel
    tag = encode_to_unicode(tag)

    value = escape( encode_to_unicode(value) )

    attrs = format_xml_attrs(**kwargs)
    if attrs: attrs = u' %s' % attrs

    if not value:
        tmpl = u'%(tabs)s<%(tag)s%(attrs)s />\n'
    else:
        tmpl = u'%(tabs)s<%(tag)s%(attrs)s>%(value)s</%(tag)s>\n'
    return tmpl%{'attrs': attrs, 'tabs': tabs, 'tag': tag, 'value': value }


def format_xml_attrs(**kwargs):
    """Format the given keyword arguments to use as XML attributes.
    The arguments will be escaped and quoted.

    The returned Unicode string doesn't contain leading and tailing spaces.

    Example::
        >>> common.format_xml_attrs(base='EditFrame', name='Frame_1')
        u'base="EditFrame" name="Frame1"'

    kwargs: Attributes to process

    Returns processed and joined kwargs as unicode"""
    if not kwargs:
        return u''

    attr_list = []
    for name in sorted(kwargs):
        value = encode_to_unicode(kwargs[name])
        value = quoteattr(value)
        value = value.strip()
        attr_list.append(u'%s=%s' % (name, value))

    attr_list.sort()
    res = u' '.join(attr_list)
    res = res.strip()
    return res

