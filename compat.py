"""
Compatibility code to run with different versions of wxPython

@copyright: 2013-2015 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

def SizerItem_SetWindow(item, window):
    """\
    wx.SizerItem.SetWindow() is deprecated since wxPython 2.9 use
    wx.SizerItem.AssignWindow() instead.

    @param item:   Instance of wx.SizerItem
    @param window: Instance of wx.Window
    """
    if wx.VERSION[:2] >= (2, 9):
        item.AssignWindow(window)
    else:
        item.SetWindow(window)
