"""
Compatibility code to run with different versions of wxPython

@copyright: 2013-2015 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx


SizerItem_SetWindow = None


def SizerItem_AssignWindow(item, window):
    """\
    Wrapper for wx.SizerItem.SetWindow() resp. wx.SizerItem.AssignWindow()

    wx.SizerItem.SetWindow() is deprecated since wxPython 2.9 use
    wx.SizerItem.AssignWindow() instead.

    Depending on the wxPython version L{SizerItem_SetWindow28()} or
    L{SizerItem_AssignWindow()} will be used.

    @param item:   Instance of wx.SizerItem
    @param window: Instance of wx.Window

    @see: L{SizerItem_SetWindow()}
    @see: L{SizerItem_SetWindow28()}
    @see: L{SizerItem_AssignWindow()}
    """
    item.AssignWindow(window)


def SizerItem_SetWindow28(item, window):
    """\
    Wrapper for wx.SizerItem.SetWindow() resp. wx.SizerItem.AssignWindow()

    wx.SizerItem.SetWindow() is deprecated since wxPython 2.9 use
    wx.SizerItem.AssignWindow() instead.

    Depending on the wxPython version L{SizerItem_SetWindow28()} or
    L{SizerItem_AssignWindow()} will be used.

    @param item:   Instance of wx.SizerItem
    @param window: Instance of wx.Window

    @see: L{SizerItem_SetWindow()}
    @see: L{SizerItem_SetWindow28()}
    @see: L{SizerItem_AssignWindow()}
    """
    item.SetWindow(window)


# Set different functions depending on the active wxPython version
if wx.VERSION[:2] >= (2, 9):
    SizerItem_SetWindow = SizerItem_AssignWindow
else:
    SizerItem_SetWindow = SizerItem_SetWindow28
