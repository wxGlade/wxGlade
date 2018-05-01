"""
Dialog for editing wxGlade preferences

@see: L{config.preferences}
@copyright: 2007 Alberto Griggio
@copyright: 20013-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

# import general python modules
import os
import wx

import bugdialog
from preferences_ui import wxGladePreferencesUI


class wxGladePreferences(wxGladePreferencesUI):
    def __init__(self, preferences):
        wxGladePreferencesUI.__init__(self, None, -1, "")

        self.choose_widget_path.Bind(wx.EVT_BUTTON, self.on_widget_path)

        self.preferences = preferences
        self.set_values()

    def set_values(self):
        try:
            self.open_save_path.SetValue(self.preferences.open_save_path)
            self.codegen_path.SetValue(self.preferences.codegen_path)
            self.use_dialog_units.SetValue( self.preferences.use_dialog_units )
            self.number_history.SetValue(self.preferences.number_history)
            self.show_progress.SetValue(self.preferences.show_progress)
            self.wxg_backup.SetValue(self.preferences.wxg_backup)
            self.codegen_backup.SetValue(self.preferences.codegen_backup)
            self.default_border.SetValue(self.preferences.default_border)
            self.default_border_size.SetValue( self.preferences.default_border_size )
            if self.preferences.backup_suffix == '.bak':
                self.backup_suffix.SetSelection(1)
            self.remember_geometry.SetValue( self.preferences.remember_geometry )
            self.local_widget_path.SetValue( self.preferences.local_widget_path )
            self.show_sizer_handle.SetValue( self.preferences.show_sizer_handle )
            self.allow_duplicate_names.SetValue( self.preferences.allow_duplicate_names )
            self.autosave.SetValue(self.preferences.autosave)
            self.autosave_delay.SetValue(self.preferences.autosave_delay)
            self.show_completion.SetValue(self.preferences.show_completion)
            self.write_timestamp.SetValue(self.preferences.write_timestamp)
            self.write_generated_from.SetValue( self.preferences.write_generated_from )
            self._fix_spin_ctrls()
        except Exception as inst:
            bugdialog.Show(_('Read Configuration'), inst)

    def _fix_spin_ctrls(self):
        "Workaround to a wxGTK 2.8.4.2 bug in wx.SpinCtrl.GetValue"
        done = {}
        for name in ('autosave_delay', 'number_history', 'default_border_size'):
            def fix(n):
                done[n] = False

                def update(e):
                    done[n] = True
                    e.Skip()

                def get_val():
                    if not done[n]:
                        return getattr(self.preferences, n)
                    else:
                        return wx.SpinCtrl.GetValue(getattr(self, n))
                return update, get_val

            spin = getattr(self, name)
            if spin.GetValue() != getattr(self.preferences, name):
                update, get_val = fix(name)
                spin.GetValue = get_val
                spin.Bind(wx.EVT_SPINCTRL, update)

    def set_preferences(self):
        prefs = self.preferences
        prefs['open_save_path'] = self.open_save_path.GetValue()
        prefs['codegen_path'] = self.codegen_path.GetValue()
        prefs['use_dialog_units'] = self.use_dialog_units.GetValue()
        prefs['number_history'] = self.number_history.GetValue()
        prefs['show_progress'] = self.show_progress.GetValue()
        prefs['wxg_backup'] = self.wxg_backup.GetValue()
        prefs['codegen_backup'] = self.codegen_backup.GetValue()
        prefs['default_border'] = self.default_border.GetValue()
        prefs['default_border_size'] = self.default_border_size.GetValue()
        if self.backup_suffix.GetSelection():
            prefs['backup_suffix'] = '.bak'
        else:
            prefs['backup_suffix'] = '~'
        prefs['remember_geometry'] = self.remember_geometry.GetValue()
        prefs['local_widget_path'] = self.local_widget_path.GetValue()
        prefs['show_sizer_handle'] = self.show_sizer_handle.GetValue()
        prefs['allow_duplicate_names'] = \
            self.allow_duplicate_names.GetValue()
        prefs['autosave'] = self.autosave.GetValue()
        prefs['autosave_delay'] = self.autosave_delay.GetValue()
        prefs['show_completion'] = self.show_completion.GetValue()

        prefs['write_timestamp'] = self.write_timestamp.GetValue()
        prefs['write_generated_from'] = self.write_generated_from.GetValue()

    def on_widget_path(self, event):
        "Create a file choice dialog"
        pth = wx.DirSelector(_("Choose a directory:"), os.getcwd(), style=wx.DD_DEFAULT_STYLE | wx.DD_NEW_DIR_BUTTON)
        if pth:
            self.local_widget_path.SetValue(pth)

# end of class wxGladePreferences
