"""
Configuration related stuff

@see: L{configdialog}
@copyright: 2007 Alberto Griggio
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import general python modules
import os.path
import sys
from ConfigParser import *

import common
import misc

# default configuration values
default_encoding = 'UTF-8'
"""\
Default value for encoding

@type: String
"""

default_indent_amount = 4
"""\
Default value for indentation

@type: Integer
"""

default_indent_symbol = ' '
"""\
Default value for indentation symbol

@type: String
"""

default_multiple_files = 0
"""\
Default value for writing multiple files (each class in a separate file)

@type: Integer
"""

default_overwrite = 1
"""\
Default value for overwriting existing sources

@type: Integer
"""

default_use_gettext = 1
"""\
Default value to usage of gettext

@type: Integer
"""

def _get_home(default=common.wxglade_path):
    h = os.path.expanduser('~')
    if h not in ('~', '%USERPROFILE%'):
        return h
    if os.name == 'nt' and h == '%USERPROFILE%':
        return os.environ.get('USERPROFILE', default)
    return default


def _get_appdatapath(default=common.wxglade_path):
    if os.name == 'nt':
        result = os.environ.get('APPDATA')
        if result:
            return result
    return _get_home(default)


class Preferences(ConfigParser):
    _has_home = os.path.expanduser('~') != '~'
    _defaults = {
        'use_menu_icons': common.use_gui and common.platform != '__WXGTK__',
        'frame_tool_win': True,
        'open_save_path': _get_home(),
        'codegen_path': _get_home(),
        'use_dialog_units': False,
        'number_history': 4,
        'show_progress': True,
        'wxg_backup': True,
        'codegen_backup': True,
        'backup_suffix': sys.platform == 'win32' and '.bak' or '~',
        'buttons_per_row': 5,
        'remember_geometry': False,
        'local_widget_path': (_get_appdatapath('') and \
                              os.path.join(_get_appdatapath(),
                                           '.wxglade', 'widgets')
                              or ''),
        'default_border' : False,
        'default_border_size' : 3,
        'show_sizer_handle': True,
        'allow_duplicate_names': False,
        'autosave': True,
        'autosave_delay': 120, # in seconds
        'use_kde_dialogs': False,
        'show_completion': True,
        'write_timestamp': True,
        'write_generated_from': False,
        }
    def __init__(self, defaults=None):
        self.def_vals = defaults
        if self.def_vals is None:
            self.def_vals = Preferences._defaults
        self.changed = False
        ConfigParser.__init__(self)
        #self.default_border_size = 3

    def __getattr__(self, attr):
        val = self.def_vals.get(attr, "")
        # UGLY!!!
        cast = type(val)
        if cast is bool: cast = self._cast_to_bool
        # ...but I haven't found a better way: the problem is that
        # bool('0') == True, while int('0') == False, and we want the
        # latter behaviour
        try:
            return cast(self.get('wxglade', attr))
        except (NoOptionError, ValueError):
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
            if val in ('true', 'on'): return 1
            elif val in ('false', 'off'): return 0
            else: raise

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
            return (x, y, w, h)
        else:
            return None

# end of class Preferences
        
preferences = None

if sys.platform == 'win32':
    _rc_name = 'wxglade.ini'
else:
    _rc_name = 'wxgladerc'

_use_file_history = False
if common.use_gui:
    _use_file_history = True


def init_preferences():
    global preferences
    if preferences is None:
        preferences = Preferences()
        h = _get_appdatapath('')
        search_path = [os.path.join(common.wxglade_path, _rc_name)]
        if h:
            search_path.append(os.path.join(h, '.wxglade', _rc_name))
        if 'WXGLADE_CONFIG_PATH' in os.environ:
            search_path.append(
                os.path.expandvars('$WXGLADE_CONFIG_PATH/%s' % _rc_name))
        preferences.read(search_path)
        if not preferences.has_section('wxglade'):
            preferences.add_section('wxglade')


def save_preferences():
    # let the exception be raised
    if 'WXGLADE_CONFIG_PATH' in os.environ:
        path = os.path.expandvars('$WXGLADE_CONFIG_PATH')
    else:
        path = _get_appdatapath()
        if path != common.wxglade_path:
            path = os.path.join(path, '.wxglade')
    if not os.path.isdir(path):
        os.makedirs(path)
    # always save the file history
    if _use_file_history:
        fh = common.palette.file_history
        count = fh.GetCount()
        encoding = 'utf-8'
        filenames = [common._encode_to_xml(fh.GetHistoryFile(i), encoding)
                     for i in range(min(preferences.number_history, count))]
        outfile = open(os.path.join(path, 'file_history.txt'), 'w')
        print >> outfile, "# -*- coding: %s -*-" % encoding
        for filename in filenames:
            print >> outfile, filename
        outfile.close()
    if preferences.changed:
        outfile = open(os.path.join(path, _rc_name), 'w')
        # let the exception be raised to signal abnormal behaviour
        preferences.write(outfile)
        outfile.close()


def load_history():
    """\
    Loads the file history and returns a list of paths
    """
    if 'WXGLADE_CONFIG_PATH' in os.environ:
        path = os.path.expandvars('$WXGLADE_CONFIG_PATH')
    else:
        path = _get_appdatapath()
        if path != common.wxglade_path:
            path = os.path.join(path, '.wxglade')
    try:
        history = open(os.path.join(path, 'file_history.txt'))
        l = history.readlines()
        if l and l[0].startswith('# -*- coding:'):
            try:
                encoding = 'utf-8' 
                #l = [common._encode_from_xml(e, encoding) for e in l[1:]]
                l = [e.decode(encoding) for e in l[1:]]
            except Exception, e:
                print _("ERR:"), e
                l = l[1:]
        history.close()
        if common.use_gui:
            l = [misc.wxstr(e, 'utf-8') for e in l]
        return l
    except IOError:
        # don't consider this an error
        return [] 
