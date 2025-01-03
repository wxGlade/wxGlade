# -*- coding: ISO-8859-15 -*-
#
# generated by wxGlade 1.1.0 on Sat Nov 16 15:17:49 2024
#

import wx

# begin wxGlade: dependencies
import gettext
# end wxGlade

# begin wxGlade: extracode
import config
import os

_icon_path = os.path.join(config.icons_path, 'icon.png')
# end wxGlade


class wxGladePreferencesUI(wx.Dialog):
    def __init__(self, *args, **kwds):
        # begin wxGlade: wxGladePreferencesUI.__init__
        kwds["style"] = kwds.get("style", 0) | wx.DEFAULT_DIALOG_STYLE
        wx.Dialog.__init__(self, *args, **kwds)
        self.SetTitle(_("wxGlade: preferences"))
        _icon = wx.NullIcon
        _icon.CopyFromBitmap(wx.Bitmap(_icon_path, wx.BITMAP_TYPE_ANY))
        self.SetIcon(_icon)

        sizer_1 = wx.BoxSizer(wx.VERTICAL)

        self.notebook_1 = wx.Notebook(self, wx.ID_ANY, style=0)
        sizer_1.Add(self.notebook_1, 1, wx.ALL | wx.EXPAND, 5)

        self.notebook_1_pane_1 = wx.Panel(self.notebook_1, wx.ID_ANY)
        self.notebook_1.AddPage(self.notebook_1_pane_1, _("Interface"))

        sizer_3 = wx.BoxSizer(wx.VERTICAL)

        self.show_progress = wx.CheckBox(self.notebook_1_pane_1, wx.ID_ANY, _("Show progress dialog when loading wxg files"))
        self.show_progress.SetValue(1)
        sizer_3.Add(self.show_progress, 0, wx.ALL | wx.EXPAND, 5)

        self.remember_geometry = wx.CheckBox(self.notebook_1_pane_1, wx.ID_ANY, _("Remember position and size of wxGlade windows"))
        self.remember_geometry.SetValue(1)
        sizer_3.Add(self.remember_geometry, 0, wx.ALL | wx.EXPAND, 5)

        self.show_sizer_handle = wx.CheckBox(self.notebook_1_pane_1, wx.ID_ANY, _("Show \"handles\" of sizers"))
        self.show_sizer_handle.SetValue(1)
        sizer_3.Add(self.show_sizer_handle, 0, wx.ALL | wx.EXPAND, 5)

        self.show_completion = wx.CheckBox(self.notebook_1_pane_1, wx.ID_ANY, _("Show success message for code generation"))
        self.show_completion.SetValue(1)
        sizer_3.Add(self.show_completion, 0, wx.ALL | wx.EXPAND, 5)

        sizer_4 = wx.FlexGridSizer(3, 2, 0, 0)
        sizer_3.Add(sizer_4, 0, wx.EXPAND, 3)

        label_1 = wx.StaticText(self.notebook_1_pane_1, wx.ID_ANY, _("Initial path for \nfile opening/saving dialogs:"))
        sizer_4.Add(label_1, 0, wx.ALL, 5)

        self.open_save_path = wx.TextCtrl(self.notebook_1_pane_1, wx.ID_ANY, "")
        self.open_save_path.SetMinSize((196, -1))
        sizer_4.Add(self.open_save_path, 1, wx.ALIGN_CENTER_VERTICAL | wx.ALL, 5)

        label_2_copy = wx.StaticText(self.notebook_1_pane_1, wx.ID_ANY, _("Initial path for \ncode generation file dialogs:"))
        sizer_4.Add(label_2_copy, 0, wx.ALL, 5)

        self.codegen_path = wx.TextCtrl(self.notebook_1_pane_1, wx.ID_ANY, "")
        self.codegen_path.SetMinSize((196, -1))
        sizer_4.Add(self.codegen_path, 1, wx.ALIGN_CENTER_VERTICAL | wx.ALL, 5)

        label_2 = wx.StaticText(self.notebook_1_pane_1, wx.ID_ANY, _("Number of items in file history"))
        sizer_4.Add(label_2, 0, wx.ALL, 5)

        self.number_history = wx.SpinCtrl(self.notebook_1_pane_1, wx.ID_ANY, "4", min=0, max=100, style=0)
        self.number_history.SetMinSize((196, -1))
        sizer_4.Add(self.number_history, 1, wx.ALIGN_CENTER_VERTICAL | wx.ALL, 5)

        sizer_9 = wx.StaticBoxSizer(wx.StaticBox(self.notebook_1_pane_1, wx.ID_ANY, _("Font Sizes")), wx.VERTICAL)
        sizer_3.Add(sizer_9, 1, wx.EXPAND, 0)

        grid_sizer_1 = wx.FlexGridSizer(1, 2, 4, 4)
        sizer_9.Add(grid_sizer_1, 1, wx.EXPAND, 0)

        label_4 = wx.StaticText(sizer_9.GetStaticBox(), wx.ID_ANY, _("Tree"))
        label_4.SetToolTip(_("Enter scale factor"))
        grid_sizer_1.Add(label_4, 0, wx.ALIGN_CENTER_VERTICAL, 0)

        self.font_scale_tree = wx.SpinCtrlDouble(sizer_9.GetStaticBox(), wx.ID_ANY, initial=1.0, min=0.0, max=5.0, style=wx.ALIGN_RIGHT | wx.SP_ARROW_KEYS)
        self.font_scale_tree.SetToolTip(_("Enter scale factor"))
        self.font_scale_tree.SetIncrement(0.1)
        self.font_scale_tree.SetDigits(2)
        grid_sizer_1.Add(self.font_scale_tree, 0, 0, 0)

        self.notebook_1_pane_3 = wx.Panel(self.notebook_1, wx.ID_ANY)
        self.notebook_1.AddPage(self.notebook_1_pane_3, _("Accessibility"))

        self.sizer_accessibility = wx.BoxSizer(wx.VERTICAL)

        sizer_8 = wx.StaticBoxSizer(wx.StaticBox(self.notebook_1_pane_3, wx.ID_ANY, _("Palette Panel")), wx.HORIZONTAL)
        self.sizer_accessibility.Add(sizer_8, 0, wx.EXPAND | wx.LEFT | wx.TOP, 6)

        self.show_palette_icons = wx.CheckBox(sizer_8.GetStaticBox(), wx.ID_ANY, _("Show icons"))
        self.show_palette_icons.SetValue(1)
        sizer_8.Add(self.show_palette_icons, 0, wx.ALL | wx.EXPAND, 5)

        self.show_palette_labels = wx.CheckBox(sizer_8.GetStaticBox(), wx.ID_ANY, _("Show labels"))
        sizer_8.Add(self.show_palette_labels, 0, wx.ALL | wx.EXPAND, 5)

        sizer_10 = wx.StaticBoxSizer(wx.StaticBox(self.notebook_1_pane_3, wx.ID_ANY, _("Property Panel")), wx.VERTICAL)
        self.sizer_accessibility.Add(sizer_10, 0, wx.EXPAND | wx.LEFT | wx.TOP, 6)

        self.show_gridproperty_editors = wx.CheckBox(sizer_10.GetStaticBox(), wx.ID_ANY, _("Show editors for Grid Properties"))
        self.show_gridproperty_editors.SetToolTip(_("As grids are not well editable with NVDA, this option will enable separate text controls for e.g. widget Events."))
        sizer_10.Add(self.show_gridproperty_editors, 0, wx.ALL | wx.EXPAND, 5)

        self.use_checkboxes_workaround = wx.CheckBox(sizer_10.GetStaticBox(), wx.ID_ANY, _("Use checkbox label workaround"))
        self.use_checkboxes_workaround.SetToolTip(_("Use a workaround to add invisible labels to enable/disable checkboxes such that NVDA will indicate the property name.\nThis is not yet tested on all platforms. It may introduce display problems."))
        self.use_checkboxes_workaround.SetValue(1)
        sizer_10.Add(self.use_checkboxes_workaround, 0, wx.ALL | wx.EXPAND, 5)

        self.no_checkbox_label_colours = wx.CheckBox(sizer_10.GetStaticBox(), wx.ID_ANY, _("Don't set checkbox label colours"))
        self.no_checkbox_label_colours.SetToolTip(_("When colours are set, screen readers will not recognize the selection state of checkboxes."))
        sizer_10.Add(self.no_checkbox_label_colours, 0, wx.ALL | wx.EXPAND, 5)

        self.notebook_1_pane_2 = wx.Panel(self.notebook_1, wx.ID_ANY)
        self.notebook_1.AddPage(self.notebook_1_pane_2, _("Other"))

        sizer_5 = wx.BoxSizer(wx.VERTICAL)

        self.allow_custom_widgets = wx.CheckBox(self.notebook_1_pane_2, wx.ID_ANY, _("Allow custom widget code in Design and Preview windows"))
        self.allow_custom_widgets.SetToolTip(_("This is a potential security risk, as malicious code might be executed."))
        sizer_5.Add(self.allow_custom_widgets, 0, wx.ALL | wx.EXPAND, 5)

        self.use_dialog_units = wx.CheckBox(self.notebook_1_pane_2, wx.ID_ANY, _("Use dialog units by default for size properties"))
        sizer_5.Add(self.use_dialog_units, 0, wx.ALL | wx.EXPAND, 5)

        self.codegen_backup = wx.CheckBox(self.notebook_1_pane_2, wx.ID_ANY, _("Create backup files for generated source"))
        self.codegen_backup.SetValue(1)
        sizer_5.Add(self.codegen_backup, 0, wx.ALL | wx.EXPAND, 5)

        self.allow_duplicate_names = wx.CheckBox(self.notebook_1_pane_2, wx.ID_ANY, _("Allow duplicate widget names"))
        self.allow_duplicate_names.Hide()
        sizer_5.Add(self.allow_duplicate_names, 0, wx.ALL | wx.EXPAND, 5)

        sizer_7 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_5.Add(sizer_7, 0, wx.EXPAND, 0)

        self.default_border = wx.CheckBox(self.notebook_1_pane_2, wx.ID_ANY, _("Default border width for widgets"))
        sizer_7.Add(self.default_border, 0, wx.ALIGN_CENTER_VERTICAL | wx.ALL, 5)

        self.default_border_size = wx.SpinCtrl(self.notebook_1_pane_2, wx.ID_ANY, "", min=0, max=20, style=0)
        sizer_7.Add(self.default_border_size, 0, wx.ALL, 5)

        sizer_7_copy = wx.BoxSizer(wx.HORIZONTAL)
        sizer_5.Add(sizer_7_copy, 0, wx.EXPAND, 0)

        self.autosave = wx.CheckBox(self.notebook_1_pane_2, wx.ID_ANY, _("Auto save wxg files every "))
        sizer_7_copy.Add(self.autosave, 0, wx.ALIGN_CENTER_VERTICAL | wx.BOTTOM | wx.LEFT | wx.TOP, 5)

        self.autosave_delay = wx.SpinCtrl(self.notebook_1_pane_2, wx.ID_ANY, "120", min=30, max=300, style=0)
        sizer_7_copy.Add(self.autosave_delay, 0, wx.ALL, 5)

        label_3 = wx.StaticText(self.notebook_1_pane_2, wx.ID_ANY, _(" seconds"))
        sizer_7_copy.Add(label_3, 0, wx.ALIGN_CENTER_VERTICAL | wx.BOTTOM | wx.FIXED_MINSIZE | wx.TOP, 5)

        self.write_timestamp = wx.CheckBox(self.notebook_1_pane_2, wx.ID_ANY, _("Insert timestamp on generated source files"))
        self.write_timestamp.SetValue(1)
        sizer_5.Add(self.write_timestamp, 0, wx.ALL | wx.EXPAND, 5)

        self.write_generated_from = wx.CheckBox(self.notebook_1_pane_2, wx.ID_ANY, _("Insert .wxg file name on generated source files"))
        sizer_5.Add(self.write_generated_from, 0, wx.ALL | wx.EXPAND, 5)

        self.wxg_backup = wx.CheckBox(self.notebook_1_pane_2, wx.ID_ANY, _("Create backup wxg files"))
        self.wxg_backup.SetValue(1)
        sizer_5.Add(self.wxg_backup, 0, wx.ALL | wx.EXPAND, 5)

        self.backup_suffix = wx.RadioBox(self.notebook_1_pane_2, wx.ID_ANY, _("Backup options"), choices=[_("append ~ to filename"), _("append .bak to filename")], majorDimension=2, style=wx.RA_SPECIFY_COLS)
        self.backup_suffix.SetSelection(0)
        sizer_5.Add(self.backup_suffix, 0, wx.ALL | wx.EXPAND, 5)

        sizer_6 = wx.StaticBoxSizer(wx.StaticBox(self.notebook_1_pane_2, wx.ID_ANY, _("Local widget path")), wx.HORIZONTAL)
        sizer_5.Add(sizer_6, 0, wx.ALL | wx.EXPAND, 5)

        self.local_widget_path = wx.TextCtrl(sizer_6.GetStaticBox(), wx.ID_ANY, "")
        sizer_6.Add(self.local_widget_path, 1, wx.ALL, 3)

        self.choose_widget_path = wx.Button(sizer_6.GetStaticBox(), wx.ID_ANY, _("..."), style=wx.BU_EXACTFIT)
        sizer_6.Add(self.choose_widget_path, 0, wx.ALIGN_CENTER_VERTICAL | wx.ALL, 3)

        sizer_2 = wx.StdDialogButtonSizer()
        sizer_1.Add(sizer_2, 0, wx.ALIGN_RIGHT | wx.ALL, 10)

        self.ok = wx.Button(self, wx.ID_OK, "")
        self.ok.SetDefault()
        sizer_2.AddButton(self.ok)

        self.cancel = wx.Button(self, wx.ID_CANCEL, "")
        sizer_2.AddButton(self.cancel)

        sizer_2.Realize()

        self.notebook_1_pane_2.SetSizer(sizer_5)

        self.notebook_1_pane_3.SetSizer(self.sizer_accessibility)

        sizer_4.AddGrowableCol(1)

        self.notebook_1_pane_1.SetSizer(sizer_3)

        self.SetSizer(sizer_1)
        sizer_1.Fit(self)

        self.Layout()
        self.Centre()
        # end wxGlade

# end of class wxGladePreferencesUI
