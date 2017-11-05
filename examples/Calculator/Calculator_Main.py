#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import wx
from Calculator_GUI import CalculatorFrame


class MyFrame(CalculatorFrame):
    def __init__(self, *args, **kwds):
        CalculatorFrame.__init__(self, *args, **kwds)
        # insert more initialization code here
        self.frame_menubar.i_reset.Enable(False)

    def on_execute_button_clicked(self, event):
        # XXX add validation and error reporting!
        value1 = float( self.text_value1.GetValue() )
        value2 = float( self.text_value2.GetValue() )
        operator = self.radiobox_operator.GetSelection() # a number from 0 to 3
        if operator==0:    result = value1 + value2
        elif operator==1:  result = value1 - value2
        elif operator==2:  result = value1 * value2
        elif operator==3:  result = value1 / value2
        self.text_result.AppendText("%s\n"%result)
        self.frame_menubar.i_reset.Enable(True)
        event.Skip()

    def on_reset_button_clicked(self, event):
        self.text_result.Clear()
        self.frame_menubar.i_reset.Enable(False)   # cleared already
        event.Skip()

    def on_menu_File_Reset(self, event):
        self.text_result.Clear()
        self.frame_menubar.i_reset.Enable(False)   # cleared already

    def on_menu_File_Exit(self, event):
        self.Close()


class MyApp(wx.App):
    def OnInit(self):
        self.frame = MyFrame(None, wx.ID_ANY, "")
        self.SetTopWindow(self.frame)
        self.frame.Show()
        return True


if __name__ == "__main__":
    app = MyApp(0)
    app.MainLoop()
