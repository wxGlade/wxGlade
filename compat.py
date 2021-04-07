"""
Compatibility code to run with different versions of wxPython

@copyright: 2013-2016 Carsten Grohmann
@copyright: 2017-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


try:
    # Python 2
    basestring = basestring
    unicode = unicode
    import cPickle as pickle
    PYTHON2 = True
    PYTHON3 = False
except:
    # Python 3
    basestring = (bytes,str)
    unicode = str
    import pickle
    PYTHON2 = False
    PYTHON3 = True

import wx



version = wx.VERSION_STRING[:3]  # Version string of major dot minor version number
if version.startswith("4."): version = "3." + version[2:]
version = (int(version[0]), int(version[2]) ) # major,minor


GridSizer_GetRows = None
GridSizer_GetCols = None
SizerItem_SetWindow = None
wxWindow_IsEnabled = None

def SizerItem_AssignWindow(item, window):
    """
    Wrapper for wxSizerItem.SetWindow() resp. wxSizerItem.AssignWindow()

    wxSizerItem.SetWindow() is deprecated since wxPython 2.9 use wxSizerItem.AssignWindow() instead.

    Depending on the wxPython version SizerItem_SetWindow28() or SizerItem_AssignWindow() will be used.

    item:   Instance of wxSizerItem
    window: Instance of wxWindow

    see: SizerItem_SetWindow(), SizerItem_SetWindow28(), SizerItem_AssignWindow()
    """
    item.AssignWindow(window)


def SizerItem_SetWindow28(item, window):
    """
    Wrapper for wxSizerItem.SetWindow() resp. wxSizerItem.AssignWindow()

    wxSizerItem.SetWindow() is deprecated since wxPython 2.9 use wxSizerItem.AssignWindow() instead.

    Depending on the wxPython version SizerItem_SetWindow28() or SizerItem_AssignWindow() will be used.

    item:   Instance of wxSizerItem
    window: Instance of wxWindow

    see: SizerItem_SetWindow(), SizerItem_SetWindow28(), SizerItem_AssignWindow()"""
    item.SetWindow(window)


def SizerItem_SetSizer(item, sizer):
    """
    Wrapper for wxSizerItem.SetSizer() resp. wxSizerItem.AssignSizer()

    wxSizerItem.SetSizer() is deprecated since wxPython 2.9 use wxSizerItem.AssignSizer() instead.

    item:  Instance of wxSizerItem
    sizer: Instance of wxSizer"""
    # don't use for now, as this causes crashes in e.g. change_sizer
    #if hasattr(item, 'AssignSizer'):
        #item.AssignSizer(sizer)
    #else:
    item.SetSizer(sizer)

def SizerItem_SetSizerPhoenix(item, sizer):
    if sizer is None: item.DetachSizer()
    item.AssignSizer(sizer)


def GridSizer_GetRows28(sizer):
    """
    Wrapper for wxGridSizer.GetRows()

    With wx3 wxGridSizer.GetRows() and wxGridSizer.GetCols() "returns zero
    if the sizer is automatically adjusting the number of rows depending
    on number of its children."

    @param sizer: Instance of wxGridSizer or a derived class
    @return: Number of rows specified for this sizer
    @rtype:  int
    """
    return sizer.GetRows()


def GridSizer_GetRows3(sizer):
    """
    Wrapper for wxGridSizer.GetRows()

    With wx3 wxGridSizer.GetRows() and wxGridSizer.GetCols() "returns zero
    if the sizer is automatically adjusting the number of rows depending
    on number of its children."

    @param sizer: Instance of wxGridSizer or a derived class
    @return: Number of rows specified for this sizer
    @rtype:  int
    """
    return sizer.GetEffectiveRowsCount()


def GridSizer_GetCols28(sizer):
    """
    Wrapper for wxGridSizer.GetColws()

    With wx3 wxGridSizer.GetRows() and wxGridSizer.GetCols() "returns zero
    if the sizer is automatically adjusting the number of rows depending
    on number of its children."

    @param sizer: Instance of wxGridSizer or a derived class
    @return: Number of columns specified for this sizer
    @rtype:  int
    """
    return sizer.GetCols()


def GridSizer_GetCols3(sizer):
    """
    Wrapper for wxGridSizer.GetColws()

    With wx3 wxGridSizer.GetRows() and wxGridSizer.GetCols() "returns zero
    if the sizer is automatically adjusting the number of rows depending
    on number of its children."

    @param sizer: Instance of wxGridSizer or a derived class
    @return: Number of columns specified for this sizer
    @rtype:  int
    """
    return sizer.GetEffectiveColsCount()


def wxWindow_IsEnabled28(item):
    """\
    Wrapper for wxWindow.IsEnabled()

    With wx3 "wxWindow::IsEnabled() now returns false if a window parent
    (and not necessarily the window itself) is disabled, new function
    IsThisEnabled() with the same behaviour as old IsEnabled() was added."

    For instance: a parent window can be disabled during a modal dialog is
    shown.

    @param item:  Instance of wxWindow
    @rtype: bool
    """
    return item.IsEnabled()


def wxWindow_IsThisEnabled(item):
    """\
    Wrapper for wxWindow.IsEnabled()

    With wx3 "wxWindow::IsEnabled() now returns false if a window parent
    (and not necessarily the window itself) is disabled, new function
    IsThisEnabled() with the same behaviour as old IsEnabled() was added."

    For instance: a parent window can be disabled during a modal dialog is
    shown.

    @param item:  Instance of wxWindow

    @rtype: bool
    """
    return item.IsThisEnabled()

def _Destroy(widget):
    if widget: widget.Destroy()

if hasattr(wx.Window, "DestroyLater"):
    def DestroyLater(widget):
        widget.Hide()
        if hasattr(widget, "DestroyLater"):
            widget.DestroyLater()
        else:
            wx.CallAfter(_Destroy, widget)
else:
    def DestroyLater(widget):
        widget.Hide()
        wx.CallAfter(_Destroy, widget)

def wxWindow_SendSizeEventToParent28(item, flags=0):
    """\
    Implementation of wxWindow.SendSizeEventToParent for wxPython 2.8

    @param item:  Instance of wxWindow
    @param flags: flags parameter for SendSizeEvent (ignored)
    """
    parent = item.GetParent()
    if parent and not parent.IsBeingDeleted():
        parent.SendSizeEvent()


def wxWindow_SendSizeEventToParent3(item, flags=0):
    """\
    Wrapper of wxWindow.SendSizeEventToParent

    @param item:  Instance of wxWindow
    @param flags: flags parameter for SendSizeEvent
    """
    item.SendSizeEventToParent(flags)


# Set different functions depending on the active wxPython version
if wx.VERSION[:2] >= (2, 9):
    GridSizer_GetRows = GridSizer_GetRows3
    GridSizer_GetCols = GridSizer_GetCols3
    SizerItem_SetWindow = SizerItem_AssignWindow
    wxWindow_IsEnabled = wxWindow_IsThisEnabled
    wxWindow_SendSizeEventToParent = wxWindow_SendSizeEventToParent3
else:
    GridSizer_GetRows = GridSizer_GetRows28
    GridSizer_GetCols = GridSizer_GetCols28
    SizerItem_SetWindow = SizerItem_SetWindow28
    wxWindow_IsEnabled = wxWindow_IsEnabled28
    wxWindow_SendSizeEventToParent = wxWindow_SendSizeEventToParent28


import wx.grid

if len(wx.VERSION)==5:
    # wxPython Classic
    IS_CLASSIC = True
    IS_PHOENIX = False

    EVT_GRID_CELL_CHANGE = wx.grid.EVT_GRID_CELL_CHANGE
    wx_SystemSettings_GetFont = wx.SystemSettings_GetFont
    wx_SystemSettings_GetColour = wx.SystemSettings_GetColour
    wx_ArtProviderPush = wx.ArtProvider.PushProvider
    wx_ArtProvider_GetBitmap = wx.ArtProvider_GetBitmap
    wx_ToolTip_SetDelay = wx.ToolTip_SetDelay
    try:
        wx_ToolTip_SetAutoPop = wx.ToolTip_SetAutoPop
    except AttributeError:
        # this one is not essential
        def wx_ToolTip_SetAutoPop(delay):
            pass
    wx_Tree_InsertItemBefore = wx.TreeCtrl.InsertItemBefore
    def SetToolTip(c, s):
        c.SetToolTipString(s)
    wx_EmptyBitmap = wx.EmptyBitmap
    wx_EmptyImage = wx.EmptyImage
    wx_EmptyIcon = wx.EmptyIcon
    wx_NamedColour = wx.NamedColour
    def ConvertPixelsToDialog(widget, size):
        return widget.ConvertPixelSizeToDialog(size)

    ListCtrl_SetStringItem    = wx.ListCtrl.SetStringItem
    ListCtrl_InsertStringItem = wx.ListCtrl.InsertStringItem
    ListCtrl_InsertImageItem = wx.ListCtrl.InsertImageItem

    def SetCursor(window, cursor):
        window.SetCursor(wx.StockCursor(cursor))
        
    BRUSHSTYLE_BDIAGONAL_HATCH = wx.BDIAGONAL_HATCH
    BRUSHSTYLE_CROSSDIAG_HATCH = wx.CROSSDIAG_HATCH
    BRUSHSTYLE_FDIAGONAL_HATCH = wx.FDIAGONAL_HATCH
else:
    # wxPython Phoenix
    IS_CLASSIC = False
    IS_PHOENIX = True

    EVT_GRID_CELL_CHANGE = wx.grid.EVT_GRID_CELL_CHANGED # uses CHANGING and CHANGED now; we only need the later
    wx_SystemSettings_GetFont = wx.SystemSettings.GetFont
    wx_SystemSettings_GetColour = wx.SystemSettings.GetColour
    wx_ArtProviderPush = wx.ArtProvider.Push
    wx_ArtProvider_GetBitmap = wx.ArtProvider.GetBitmap
    wx_ToolTip_SetDelay = wx.ToolTip.SetDelay
    wx_ToolTip_SetAutoPop = wx.ToolTip.SetAutoPop
    wx_Tree_InsertItemBefore = wx.TreeCtrl.InsertItem # overloaded: index or item
    SizerItem_SetSizer = SizerItem_SetSizerPhoenix    # uses AssignSizer
    def SetToolTip(c, s):
        c.SetToolTip(s)
    wx_EmptyBitmap = wx.Bitmap
    wx_EmptyImage = wx.Image
    wx_EmptyIcon = wx.Icon
    wx_NamedColour = wx.Colour
    def ConvertPixelsToDialog(widget, size):
        return widget.ConvertPixelsToDialog(size)

    ListCtrl_SetStringItem    = wx.ListCtrl.SetItem
    ListCtrl_InsertStringItem = wx.ListCtrl.InsertItem
    ListCtrl_InsertImageItem = wx.ListCtrl.InsertItem

    def SetCursor(window, cursor):
        window.SetCursor(wx.Cursor(cursor))

    BRUSHSTYLE_BDIAGONAL_HATCH = wx.BRUSHSTYLE_BDIAGONAL_HATCH
    BRUSHSTYLE_CROSSDIAG_HATCH = wx.BRUSHSTYLE_CROSSDIAG_HATCH
    BRUSHSTYLE_FDIAGONAL_HATCH = wx.BRUSHSTYLE_FDIAGONAL_HATCH
