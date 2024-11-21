"""
Dialog to select a colour

@copyright: 2007 Marcello Semboli
@copyright: 2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from wx.lib.colourchooser import PyColourChooser
import misc, compat


try:
    from wx.lib.colourchooser import ColourChangedEvent, ColourChangedEventBase
    ColourChangedEvent(None)
except TypeError:
    class ColourChangedEvent(ColourChangedEventBase):
        """Adds GetColour()/GetValue() for compatibility with ColourPickerCtrl and colourselect"""
        def __init__(self, newColour):
            super(ColourChangedEvent, self).__init__(newColour = newColour)

        def GetColour(self):
            return self.newColour

        def GetValue(self):
            return self.newColour

    class PyColourChooser(PyColourChooser):
        def updateDisplayColour(self, colour):
            """Update the displayed colour box (solid) and send the EVT_COLOUR_CHANGED"""
            self.solid.SetColour(colour)
            evt = ColourChangedEvent(newColour=colour)
            wx.PostEvent(self, evt)
except ImportError:
    pass  # for wxPython 2.8


class wxGladeColourDialog(wx.Dialog):
    def __init__(self, colours_dict, parent=None):
        wx.Dialog.__init__(self, parent, -1, "")
        self.colours_dict = colours_dict
        choices = list( self.colours_dict.keys() )
        choices.sort()

        self.panel_1 = wx.Panel(self, -1)
        self.use_null_colour = wx.RadioButton( self.panel_1, -1, "wxNullColour", style=wx.RB_GROUP )
        self.use_sys_colour = wx.RadioButton( self.panel_1, -1, _("System Colour") )
        self.sys_colour = wx.ComboBox( self.panel_1, -1, choices=choices, style=wx.CB_DROPDOWN | wx.CB_READONLY)
        self.sys_colour_panel = wx.Panel(self.panel_1, -1, size=(250, 20))
        self.sys_colour_panel.SetBackgroundColour(wx.RED)
        self.use_chooser = wx.RadioButton(self.panel_1, -1, _("Custom Colour"))
        self.colour_chooser = PyColourChooser(self, -1)
        self.ok = wx.Button(self, wx.ID_OK)
        self.ok.SetDefault()
        self.cancel = wx.Button(self, wx.ID_CANCEL)

        self.__set_properties()
        self.__do_layout()

        self.use_null_colour.Bind(wx.EVT_RADIOBUTTON, self.on_use_null_colour)
        self.use_sys_colour.Bind(wx.EVT_RADIOBUTTON, self.on_use_sys_colour)
        self.use_chooser.Bind(wx.EVT_RADIOBUTTON, self.on_use_chooser)
        self.sys_colour.Bind(wx.EVT_COMBOBOX, self.display_sys_colour)
        self.display_sys_colour()
        for ctrl in (self.use_null_colour, self.use_sys_colour, self.use_chooser):
            ctrl.Bind(wx.EVT_LEFT_DCLICK, lambda evt: self.EndModal(wx.ID_OK) )

    def display_sys_colour(self, event=None):
        colour = self.sys_colour.GetValue().strip()
        if colour.startswith("wxSYS_COLOUR_"):
            colour = getattr(wx, colour[2:], None)
        else:
            colour = None
        if colour:
            self.sys_colour.SetBackgroundColour(wx.WHITE)
            colour = compat.wx_SystemSettings_GetColour(colour)
        else:
            self.sys_colour.SetBackgroundColour(wx.RED)
            colour = wx.NullColour
        self.sys_colour_panel.SetBackgroundColour(colour)
        self.sys_colour_panel.Refresh()

    def on_use_null_colour(self, event):
        self.sys_colour.Enable(False)
        self.colour_chooser.Enable(False)
        self.use_chooser.SetValue(0)
        self.use_sys_colour.SetValue(0)

    def on_use_sys_colour(self, event):
        self.sys_colour.Enable(True)
        self.colour_chooser.Enable(False)
        self.use_chooser.SetValue(0)
        self.use_null_colour.SetValue(0)

    def on_use_chooser(self, event):
        self.sys_colour.Enable(False)
        self.colour_chooser.Enable(True)
        self.use_sys_colour.SetValue(0)
        self.use_null_colour.SetValue(0)

    def get_value(self):
        if self.use_null_colour.GetValue():
            return "wxNullColour"
        if self.use_sys_colour.GetValue():
            return self.sys_colour.GetStringSelection()
        else:
            return misc.colour_to_string(self.colour_chooser.GetValue())

    def set_value(self, value):
        value = value.strip()
        if value=="wxNullColour":
            self.use_null_colour.SetValue(1)
            self.use_sys_colour.SetValue(0)
            self.use_chooser.SetValue(0)
            self.sys_colour.Enable(False)
            self.colour_chooser.Enable(False)
        elif value in self.colours_dict:
            self.use_sys_colour.SetValue(1)
            self.use_chooser.SetValue(0)
            self.use_null_colour.SetValue(0)
            self.sys_colour.SetValue(value)
            self.sys_colour.Enable(True)
            self.colour_chooser.Enable(False)
        else:
            self.use_chooser.SetValue(1)
            self.use_sys_colour.SetValue(0)
            self.use_null_colour.SetValue(0)
            try: self.colour_chooser.SetValue(misc.string_to_colour(value))
            except: pass
            self.sys_colour.Enable(False)
            self.colour_chooser.Enable(True)

    def __set_properties(self):
        self.SetTitle(_("Select widget colour"))
        self.use_sys_colour.SetValue(1)
        self.sys_colour.SetSelection(0)
        self.ok.SetDefault()
        self.use_chooser.SetValue(0)
        self.colour_chooser.Enable(False)

    def __do_layout(self):
        sizer_1 = wx.BoxSizer(wx.VERTICAL)
        sizer_3 = wx.StdDialogButtonSizer()
        sizer_2 = wx.BoxSizer(wx.VERTICAL)
        sizer_2.Add(self.use_null_colour, 0, wx.LEFT|wx.RIGHT|wx.TOP|wx.EXPAND, 5)
        sizer_2.Add(wx.StaticLine(self.panel_1, -1), 0, wx.ALL|wx.EXPAND, 5)
        sizer_2.Add(self.use_sys_colour, 0, wx.LEFT|wx.RIGHT|wx.TOP|wx.EXPAND, 5)
        sizer_2.Add(self.sys_colour, 0, wx.ALL|wx.EXPAND, 5)
        sizer_2.Add(self.sys_colour_panel, 0, wx.ALL|wx.EXPAND, 5)
        sizer_2.Add(wx.StaticLine(self.panel_1, -1), 0, wx.ALL|wx.EXPAND, 5)
        sizer_2.Add(self.use_chooser, 0, wx.LEFT|wx.RIGHT|wx.TOP|wx.EXPAND, 5)
        self.panel_1.SetAutoLayout(1)
        self.panel_1.SetSizer(sizer_2)
        sizer_2.Fit(self.panel_1)
        sizer_2.SetSizeHints(self.panel_1)
        sizer_1.Add(self.panel_1, 0, wx.EXPAND, 0)
        sizer_1.Add(self.colour_chooser, 0, wx.ALL, 5)
        sizer_2.Add(wx.StaticLine(self.panel_1, -1), 0, wx.ALL|wx.EXPAND, 5)
        sizer_3.AddButton(self.ok)
        sizer_3.AddButton(self.cancel)
        sizer_3.Realize()
        sizer_1.Add(sizer_3, 0, wx.ALL|wx.ALIGN_RIGHT, 10)
        self.SetAutoLayout(1)
        self.SetSizer(sizer_1)
        sizer_1.Fit(self)
        sizer_1.SetSizeHints(self)
        self.Layout()
