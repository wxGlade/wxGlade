# config.py: wxGlade configuration handling
# $Id: config.py,v 1.16 2003/09/11 06:35:20 dinogen Exp $
# 
# Copyright (c) 2002-2003 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

# generated by wxGlade 0.1.3 on Tue Oct 22 16:13:00 2002

from ConfigParser import *
import common, sys, os, os.path

if common.use_gui:
    from wxPython.wx import *

    class wxGladePreferences(wxDialog):
        def __init__(self, preferences):
            wxDialog.__init__(self, None, -1, "")
            # begin wxGlade: wxGladePreferences.__init__
            self.notebook_1 = wxNotebook(self, -1, style=0)
            self.notebook_1_pane_2 = wxPanel(self.notebook_1, -1)
            self.notebook_1_pane_1 = wxPanel(self.notebook_1, -1)
            self.use_menu_icons = wxCheckBox(self.notebook_1_pane_1, -1,
                                             "Use icons in menu items")
            self.frame_tool_win = wxCheckBox(self.notebook_1_pane_1, -1,
                                             "Show properties and tree windows"
                                             " as small frames (Win32 only)")
            self.show_progress = wxCheckBox(self.notebook_1_pane_1, -1,
                                            "Show progress dialog when loading"
                                            " wxg files")
            self.remember_geometry = wxCheckBox(self.notebook_1_pane_1, -1,
                                                "Remember position and size "
                                                "of wxGlade windows")
            self.open_save_path = wxTextCtrl(self.notebook_1_pane_1, -1, "")
            self.codegen_path = wxTextCtrl(self.notebook_1_pane_1, -1, "")
            self.number_history = wxSpinCtrl(self.notebook_1_pane_1, -1, "4",
                                             min=0, max=100)
            self.buttons_per_row = wxSpinCtrl(self.notebook_1_pane_1, -1, "5",
                                              min=1, max=100)
            self.use_dialog_units = wxCheckBox(self.notebook_1_pane_2, -1,
                                               "Use dialog units by default "
                                               "for size properties")
            self.wxg_backup = wxCheckBox(self.notebook_1_pane_2, -1,
                                         "Create backup wxg files")
            self.codegen_backup = wxCheckBox(self.notebook_1_pane_2, -1,
                                             "Create backup files for "
                                             "generated source")
            self.backup_suffix = wxRadioBox(
                self.notebook_1_pane_2, -1, "Backup options",
                choices=["append ~ to filename", "append .bak to filename"],
                majorDimension=2, style=wxRA_SPECIFY_COLS)
            self.local_widget_path = wxTextCtrl(self.notebook_1_pane_2, -1, "")
            self.choose_widget_path = wxButton(self.notebook_1_pane_2, -1,
                                               "...")
            self.ok = wxButton(self, wxID_OK, "OK")
            self.cancel = wxButton(self, wxID_CANCEL, "Cancel")
            self.apply = wxButton(self, -1, "Apply")
            #MARCELLO
            self.default_border = wxCheckBox(self.notebook_1_pane_2, -1,
                                         "Create widgets with a default border")

            self.__set_properties()
            self.__do_layout()
            # end wxGlade
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
            
        def __set_properties(self):
            # begin wxGlade: wxGladePreferences.__set_properties
            self.SetTitle("wxGlade: preferences")
            self.use_menu_icons.SetValue(1)
            self.frame_tool_win.SetValue(1)
            self.show_progress.SetValue(1)
            self.remember_geometry.SetValue(1)
            self.open_save_path.SetSize((196, -1))
            self.codegen_path.SetSize((196, -1))
            self.number_history.SetSize((196, -1))
            self.buttons_per_row.SetSize((196, -1))
            self.wxg_backup.SetValue(1)
            self.codegen_backup.SetValue(1)
            self.default_border.SetValue(1)
            self.backup_suffix.SetSelection(0)
            self.choose_widget_path.SetSize(wxDLG_SZE(self.choose_widget_path,
                                                      (12, -1)))
            self.ok.SetDefault()
            # end wxGlade

        def __do_layout(self):
            # begin wxGlade: wxGladePreferences.__do_layout
            sizer_1 = wxBoxSizer(wxVERTICAL)
            sizer_2 = wxBoxSizer(wxHORIZONTAL)
            sizer_5 = wxBoxSizer(wxVERTICAL)
            sizer_6 = wxStaticBoxSizer(
                wxStaticBox(self.notebook_1_pane_2, -1, "Local widget path"),
                wxHORIZONTAL)
            sizer_3 = wxBoxSizer(wxVERTICAL)
            sizer_4 = wxFlexGridSizer(4, 2, 0, 0)
            sizer_3.Add(self.use_menu_icons, 0, wxALL|wxEXPAND, 5)
            sizer_3.Add(self.frame_tool_win, 0, wxALL|wxEXPAND, 5)
            sizer_3.Add(self.show_progress, 0, wxALL|wxEXPAND, 5)
            sizer_3.Add(self.remember_geometry, 0, wxALL|wxEXPAND, 5)
            label_1 = wxStaticText(self.notebook_1_pane_1, -1,
                                   "Initial path for \nfile opening/saving "
                                   "dialogs:")
            sizer_4.Add(label_1, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5)
            sizer_4.Add(self.open_save_path, 1,
                        wxALL|wxALIGN_CENTER_VERTICAL, 5)
            label_2_copy = wxStaticText(self.notebook_1_pane_1, -1,
                                        "Initial path for \ncode generation "
                                        "file dialogs:")
            sizer_4.Add(label_2_copy, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5)
            sizer_4.Add(self.codegen_path, 1, wxALL|wxALIGN_CENTER_VERTICAL, 5)
            label_2 = wxStaticText(self.notebook_1_pane_1, -1,
                                   "Number of items in file history\n"
                                   "(wxPython >= 2.3.3)")
            sizer_4.Add(label_2, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5)
            sizer_4.Add(self.number_history, 0,
                        wxALL|wxALIGN_CENTER_VERTICAL, 5)
            label_2_copy_1 = wxStaticText(self.notebook_1_pane_1, -1,
                                          "Number of buttons per row\nin the "
                                          "main palette")
            sizer_4.Add(label_2_copy_1, 0, wxALL|wxALIGN_CENTER_VERTICAL, 5)
            sizer_4.Add(self.buttons_per_row, 0,
                        wxALL|wxALIGN_CENTER_VERTICAL, 5)
            sizer_4.AddGrowableCol(1)
            sizer_3.Add(sizer_4, 0, wxEXPAND, 3)
            self.notebook_1_pane_1.SetAutoLayout(1)
            self.notebook_1_pane_1.SetSizer(sizer_3)
            sizer_3.Fit(self.notebook_1_pane_1)
            sizer_3.SetSizeHints(self.notebook_1_pane_1)
            sizer_5.Add(self.use_dialog_units, 0, wxALL|wxEXPAND, 5)
            sizer_5.Add(self.wxg_backup, 0, wxALL|wxEXPAND, 5)
            sizer_5.Add(self.codegen_backup, 0, wxALL|wxEXPAND, 5)
            #MARCELLO
            sizer_5.Add(self.default_border, 0, wxALL|wxEXPAND, 5)
            sizer_5.Add(self.backup_suffix, 0, wxALL|wxEXPAND, 5)
            sizer_6.Add(self.local_widget_path, 1, wxALL, 3)
            sizer_6.Add(self.choose_widget_path, 0,
                        wxALL|wxALIGN_CENTER_VERTICAL, 3)
            sizer_5.Add(sizer_6, 0, wxALL|wxEXPAND, 5)
            self.notebook_1_pane_2.SetAutoLayout(1)
            self.notebook_1_pane_2.SetSizer(sizer_5)
            sizer_5.Fit(self.notebook_1_pane_2)
            sizer_5.SetSizeHints(self.notebook_1_pane_2)
            self.notebook_1.AddPage(self.notebook_1_pane_1, "Interface")
            self.notebook_1.AddPage(self.notebook_1_pane_2, "Other")
            sizer_1.Add(wxNotebookSizer(self.notebook_1), 1, wxALL|wxEXPAND, 5)
            sizer_2.Add(self.ok, 0, 0, 0)
            sizer_2.Add(self.cancel, 0, wxLEFT, 10)
            sizer_2.Add(self.apply, 0, wxLEFT, 10)
            sizer_1.Add(sizer_2, 0, wxALL|wxALIGN_RIGHT, 10)
            self.SetAutoLayout(1)
            self.SetSizer(sizer_1)
            sizer_1.Fit(self)
            sizer_1.SetSizeHints(self)
            self.Layout()
            # end wxGlade
            self.CentreOnScreen()

    # end of class wxGladePreferences


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
        'default_border' : False
        }
    def __init__(self, defaults=None):
        self.def_vals = defaults
        if self.def_vals is None:
            self.def_vals = Preferences._defaults
        self.changed = False
        ConfigParser.__init__(self)
        # Maybe this will be an option. 
        # For now I fix it to 3
        self.default_border_size = 3

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
    
