# -*- coding: ISO-8859-15 -*-
#
# generated by wxGlade "faked test version"
#

import wx

# begin wxGlade: dependencies
# end wxGlade

# begin wxGlade: extracode
# extra code added using wxGlade
import time
# end wxGlade


class PyOgg2_MyDialog(wx.Dialog):
    def __init__(self, *args, **kwds):
        # begin wxGlade: PyOgg2_MyDialog.__init__
        kwds["style"] = wx.DEFAULT_DIALOG_STYLE | wx.RESIZE_BORDER | wx.THICK_FRAME
        wx.Dialog.__init__(self, *args, **kwds)
        self.notebook_1 = wx.Notebook(self, wx.ID_ANY, style=0)
        self.notebook_1_pane_1 = wx.Panel(self.notebook_1, wx.ID_ANY)
        self.text_ctrl_1 = wx.TextCtrl(self.notebook_1_pane_1, wx.ID_ANY, "")
        self.button_3 = wx.Button(self.notebook_1_pane_1, wx.ID_OPEN, "")
        self.notebook_1_pane_2 = wx.Panel(self.notebook_1, wx.ID_ANY)
        self.radio_box_1 = wx.RadioBox(self.notebook_1_pane_2, wx.ID_ANY, _("Sampling Rate"), choices=[_("44 kbit"), _("128 kbit")], majorDimension=0, style=wx.RA_SPECIFY_ROWS)
        self.notebook_1_pane_3 = wx.Panel(self.notebook_1, wx.ID_ANY)
        self.text_ctrl_2 = wx.TextCtrl(self.notebook_1_pane_3, wx.ID_ANY, "", style=wx.TE_MULTILINE | wx.TE_LINEWRAP)
        self.notebook_1_pane_4 = wx.Panel(self.notebook_1, wx.ID_ANY)
        self.label_2 = wx.StaticText(self.notebook_1_pane_4, wx.ID_ANY, _("File name:"))
        self.text_ctrl_3 = wx.TextCtrl(self.notebook_1_pane_4, wx.ID_ANY, "")
        self.button_4 = wx.Button(self.notebook_1_pane_4, wx.ID_OPEN, "")
        self.checkbox_1 = wx.CheckBox(self.notebook_1_pane_4, wx.ID_ANY, _("Overwrite existing file"))
        self.static_line_1 = wx.StaticLine(self, wx.ID_ANY)
        self.button_5 = wx.Button(self, wx.ID_CLOSE, "")
        self.button_2 = wx.Button(self, wx.ID_CANCEL, "", style=wx.BU_TOP)
        self.button_1 = wx.Button(self, wx.ID_OK, "", style=wx.BU_TOP)

        self.__set_properties()
        self.__do_layout()

        self.Bind(wx.EVT_BUTTON, self.startConverting, self.button_1)
        # end wxGlade

    def __set_properties(self):
        # begin wxGlade: PyOgg2_MyDialog.__set_properties
        self.SetTitle(_("mp3 2 ogg"))
        self.SetSize((500, 300))
        self.radio_box_1.SetSelection(0)
        self.checkbox_1.SetToolTipString(_("Overwrite an existing file"))
        self.checkbox_1.SetValue(1)
        # end wxGlade

    def __do_layout(self):
        # begin wxGlade: PyOgg2_MyDialog.__do_layout
        sizer_1 = wx.FlexGridSizer(3, 1, 0, 0)
        sizer_2 = wx.FlexGridSizer(1, 3, 0, 0)
        grid_sizer_2 = wx.FlexGridSizer(2, 3, 0, 0)
        sizer_3 = wx.BoxSizer(wx.HORIZONTAL)
        sizer_4 = wx.BoxSizer(wx.HORIZONTAL)
        grid_sizer_1 = wx.FlexGridSizer(1, 3, 0, 0)
        label_1 = wx.StaticText(self.notebook_1_pane_1, wx.ID_ANY, _("File name:"))
        grid_sizer_1.Add(label_1, 0, wx.ALL | wx.ALIGN_CENTER_VERTICAL, 5)
        grid_sizer_1.Add(self.text_ctrl_1, 1, wx.ALL | wx.EXPAND | wx.ALIGN_CENTER_VERTICAL, 5)
        grid_sizer_1.Add(self.button_3, 0, wx.ALL, 5)
        self.notebook_1_pane_1.SetSizer(grid_sizer_1)
        grid_sizer_1.AddGrowableCol(1)
        sizer_4.Add(self.radio_box_1, 1, wx.ALL | wx.EXPAND | wx.SHAPED, 5)
        self.notebook_1_pane_2.SetSizer(sizer_4)
        sizer_3.Add(self.text_ctrl_2, 1, wx.ALL | wx.EXPAND, 5)
        self.notebook_1_pane_3.SetSizer(sizer_3)
        grid_sizer_2.Add(self.label_2, 0, wx.ALL | wx.ALIGN_CENTER_VERTICAL, 5)
        grid_sizer_2.Add(self.text_ctrl_3, 0, wx.ALL | wx.EXPAND, 5)
        grid_sizer_2.Add(self.button_4, 0, wx.ALL, 5)
        grid_sizer_2.Add((20, 20), 0, 0, 0)
        grid_sizer_2.Add(self.checkbox_1, 0, wx.ALL | wx.EXPAND, 5)
        grid_sizer_2.Add((20, 20), 0, 0, 0)
        self.notebook_1_pane_4.SetSizer(grid_sizer_2)
        grid_sizer_2.AddGrowableCol(1)
        self.notebook_1.AddPage(self.notebook_1_pane_1, _("Input File"))
        self.notebook_1.AddPage(self.notebook_1_pane_2, _("Converting Options"))
        self.notebook_1.AddPage(self.notebook_1_pane_3, _("Converting Progress"))
        self.notebook_1.AddPage(self.notebook_1_pane_4, _("Output File"))
        sizer_1.Add(self.notebook_1, 1, wx.EXPAND, 0)
        sizer_1.Add(self.static_line_1, 0, wx.ALL | wx.EXPAND, 5)
        sizer_2.Add(self.button_5, 0, wx.ALL | wx.ALIGN_RIGHT, 5)
        sizer_2.Add(self.button_2, 0, wx.ALL | wx.ALIGN_RIGHT, 5)
        sizer_2.Add(self.button_1, 0, wx.ALL | wx.ALIGN_RIGHT, 5)
        sizer_1.Add(sizer_2, 0, wx.ALIGN_RIGHT, 0)
        self.SetSizer(sizer_1)
        sizer_1.AddGrowableRow(0)
        sizer_1.AddGrowableCol(0)
        self.Layout()
        self.Centre()
        # end wxGlade

    def startConverting(self, event):  # wxGlade: PyOgg2_MyDialog.<event_handler>
        print "Event handler `startConverting' not implemented!"
        event.Skip()

# end of class PyOgg2_MyDialog
