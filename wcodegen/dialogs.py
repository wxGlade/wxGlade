"""\
Dialogs to ask users during widget initialisation triggered by an graphical
user interaction.

@copyright: 2014-2016 Carsten Grohmann
@copyright: 2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx


class WidgetStyleSelectionDialog(wx.Dialog):
    "User dialog to select a style during widget creation"
    def __init__(self, dlg_title, box_label, choices, options=None, defaults=None):
        """Initialise the dialog and draw the content

        dlg_title: Dialog title
        box_label: Label of the draw around the listed choices
        choices: Choices to select one (string list)"""
        pos = wx.GetMousePosition()
        wx.Dialog.__init__(self, None, -1, dlg_title, pos)

        szr = wx.BoxSizer(wx.VERTICAL)

        self.box = wx.RadioBox( self, wx.ID_ANY, box_label, wx.DefaultPosition, wx.DefaultSize,choices.split('|'),
                                1, style=wx.RA_SPECIFY_COLS )
        self.box.SetSelection(0)
        szr.Add(self.box, 5, wx.ALL | wx.EXPAND, 10)

        if options:
            self.options = []
            for o, option in enumerate(options):
                cb = wx.CheckBox(self, -1, option)
                cb.SetValue(defaults and defaults[o])
                szr.Add(cb, 0, wx.ALL, 10)
                self.options.append(cb)

        btn = wx.Button(self, wx.ID_OK, _('OK'))
        btn.SetDefault()
        szr.Add(btn, 0, wx.BOTTOM | wx.ALIGN_CENTER, 10)

        self.SetAutoLayout(True)
        self.SetSizer(szr)
        szr.Fit(self)

    def get_selection(self):
        "Return the selected choice."
        return self.box.GetStringSelection()

    def get_options(self):
        return [o.GetValue() for o in self.options]