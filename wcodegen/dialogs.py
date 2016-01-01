"""\
Dialogs to ask users during widget initialisation triggered by an graphical
user interaction.

@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx


class WidgetStyleSelectionDialog(wx.Dialog):
    """\
    User dialog to select a style during widget creation
    """

    def __init__(self, dlg_title, box_label, choices):
        """\
        Initialise the dialog and draw the content

        @param dlg_title: Dialog title
        @type dlg_title:  str | unicode

        @param box_label: Label of the draw around the listed choices
        @type box_label:  str | unicode

        @param choices: Choices to select one
        @type choices:  str
        """
        wx.Dialog.__init__(self, None, -1, dlg_title)

        szr = wx.BoxSizer(wx.VERTICAL)

        self.box = wx.RadioBox(
            self, wx.ID_ANY, box_label, wx.DefaultPosition, wx.DefaultSize,
            choices.split('|'), 1, style=wx.RA_SPECIFY_COLS)
        self.box.SetSelection(0)
        szr.Add(self.box, 5, wx.ALL | wx.EXPAND, 10)

        btn = wx.Button(self, wx.ID_OK, _('OK'))
        btn.SetDefault()
        szr.Add(btn, 0, wx.BOTTOM | wx.ALIGN_CENTER, 10)

        self.SetAutoLayout(True)
        self.SetSizer(szr)
        szr.Fit(self)
        self.CenterOnScreen()

    def get_selection(self):
        """\
        Return the selected choice.

        @rtype: str
        """
        return self.box.GetStringSelection()
