from ConfigParser import *
import common, sys, os, os.path

if common.use_gui:
    from wxPython.wx import *
    from configUI import *

    class wxGladePreferences(wxGladePreferencesUI):
        def __init__(self, preferences):
            wxGladePreferencesUI.__init__(self, None, -1, "")
            EVT_BUTTON(self, self.apply.GetId(),
                       lambda e: self.set_preferences())
            
            EVT_BUTTON(self, self.choose_widget_path.GetId(),
                       self.on_widget_path)
    
            self.preferences = preferences
            self.set_values()
            
        def set_values(self):
            try:
                self.use_menu_icons.SetValue(self.preferences.use_menu_icons)
                self.frame_tool_win.SetValue(self.preferences.frame_tool_win)
                self.open_save_path.SetValue(self.preferences.open_save_path)
                self.codegen_path.SetValue(self.preferences.codegen_path)
                self.use_dialog_units.SetValue(
                    self.preferences.use_dialog_units)
                self.number_history.SetValue(self.preferences.number_history)
                self.show_progress.SetValue(self.preferences.show_progress)
                self.wxg_backup.SetValue(self.preferences.wxg_backup)
                self.codegen_backup.SetValue(self.preferences.codegen_backup)
                #MARCELLO
                self.default_border.SetValue(self.preferences.default_border)
                self.default_border_size.SetValue(
                    self.preferences.default_border_size)
                if self.preferences.backup_suffix == '.bak':
                    self.backup_suffix.SetSelection(1)
                self.buttons_per_row.SetValue(self.preferences.buttons_per_row)
                self.remember_geometry.SetValue(
                    self.preferences.remember_geometry)
                self.local_widget_path.SetValue(
                    self.preferences.local_widget_path)
            except Exception, e:
                wxMessageBox('Error reading config file:\n%s' % e, 'Error',
                             wxOK|wxCENTRE|wxICON_ERROR)
    
        def set_preferences(self):
            prefs = self.preferences
            prefs['use_menu_icons'] = self.use_menu_icons.GetValue()
            prefs['frame_tool_win'] = self.frame_tool_win.GetValue()
            prefs['open_save_path'] = self.open_save_path.GetValue()
            prefs['codegen_path'] = self.codegen_path.GetValue()
            prefs['use_dialog_units'] = self.use_dialog_units.GetValue()
            prefs['number_history'] = self.number_history.GetValue()
            prefs['show_progress'] = self.show_progress.GetValue()
            prefs['wxg_backup'] = self.wxg_backup.GetValue()
            prefs['codegen_backup'] = self.codegen_backup.GetValue()
            #MARCELLO
            prefs['default_border'] = self.default_border.GetValue()
            prefs['default_border_size'] = self.default_border_size.GetValue()
            if self.backup_suffix.GetSelection():
                prefs['backup_suffix'] = '.bak'
            else: prefs['backup_suffix'] = '~'
            prefs['buttons_per_row'] = self.buttons_per_row.GetValue()
            prefs['remember_geometry'] = self.remember_geometry.GetValue()
            prefs['local_widget_path'] = self.local_widget_path.GetValue()
            
        def on_widget_path(self, event):
            # create a file choice dialog
            dlg = wxDirDialog(self, "Choose a directory:", os.getcwd(),
                              style=wxDD_DEFAULT_STYLE|wxDD_NEW_DIR_BUTTON)
            if dlg.ShowModal() == wxID_OK:
                wxMessageBox('Changes to local widget path take effect '
                             'on restart of wxGlade', 'wxGlade',
                             wxOK|wxICON_INFORMATION|wxCENTRE)
                self.local_widget_path.SetValue(dlg.GetPath())
            dlg.Destroy()



def _get_home(default=common.wxglade_path):
    h = os.path.expanduser('~')
    if h not in ('~', '%USERPROFILE%'):
        return h
    if os.name == 'nt' and h == '%USERPROFILE%':
        return os.environ.get('USERPROFILE', default)
    return default


class Preferences(ConfigParser):
    _has_home = os.path.expanduser('~') != '~'
    _defaults = {
        'use_menu_icons': common.use_gui and wxPlatform != '__WXGTK__',
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
        'local_widget_path': (_get_home('') and \
                              os.path.join(_get_home(), '.wxglade', 'widgets')
                              or ''),
        'default_border' : False,
        'default_border_size' : 3
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
        if cast is bool: cast = int
        # ...but I haven't found a better way: the problem is that
        # bool('0') == True, while int('0') == False, and we want the
        # latter behaviour
        try:
            return cast(self.get('wxglade', attr))
        except (NoOptionError, ValueError):
            return val

    def __getitem__(self, attr):
        return self.__getattr__(attr)
    
    def __setitem__(self, attr, val):
        self.set('wxglade', attr, str(val))
        self.changed = True

    def set_geometry(self, name, geometry):
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

if sys.platform == 'win32': _rc_name = 'wxglade.ini'
else: _rc_name = 'wxgladerc'

_use_file_history = False
if common.use_gui:
    import misc
    if misc.check_wx_version(2, 3, 3): _use_file_history = True

def init_preferences():
    global preferences
    if preferences is None:
        preferences = Preferences()
        h = _get_home('')
        search_path = [os.path.join(common.wxglade_path, _rc_name)]
        if h:
            search_path.append(os.path.join(h, '.wxglade', _rc_name))
        if 'WXGLADE_CONFIG_PATH' in os.environ:
            search_path.append(
                os.path.expandvars('$WXGLADE_CONFIG_PATH/%s' % _rc_name))
        preferences.read(search_path)
        if not preferences.has_section('wxglade'):
            preferences.add_section('wxglade')

def edit_preferences():
    dialog = wxGladePreferences(preferences)
    if dialog.ShowModal() == wxID_OK:
        dialog.set_preferences()
    dialog.Destroy()

def save_preferences():
    # let the exception be raised
    if 'WXGLADE_CONFIG_PATH' in os.environ:
        path = os.path.expandvars('$WXGLADE_CONFIG_PATH')
    else:
        path = _get_home()
        if path != common.wxglade_path:
            path = os.path.join(path, '.wxglade')
    if not os.path.isdir(path):
        os.mkdir(path)
    # always save the file history
    if _use_file_history:
        fh = common.palette.file_history
        filenames = [ fh.GetHistoryFile(i) for i in
                      range(min(preferences.number_history,
                                fh.GetNoHistoryFiles())) ]
        outfile = open(os.path.join(path, 'file_history.txt'), 'w')
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
        path = _get_home()
        if path != common.wxglade_path:
            path = os.path.join(path, '.wxglade')
    try:
        history = open(os.path.join(path, 'file_history.txt'))
        l = history.readlines()
        history.close()
        return l
    except IOError:
        # don't consider this an error
        return [] 
    
