# misc.py: Miscellaneus stuff, used in many parts of wxGlade
# 
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: Python 2.2 license (see license.txt)

from wxPython.wx import *

if wxPlatform == '__WXMSW__':
    class wxGladeRadioButton(wxRadioButton):
        """
        custom wxRadioButton class which tries to implement a better
        GetBestSize than the default one for WXMSW (mostly copied from
        wxCheckBox::DoGetBestSize in checkbox.cpp)
        """
        __radio_size = None
        def GetBestSize(self):
            if not self.__radio_size:
                dc = wxScreenDC()
                dc.SetFont(wxSystemSettings_GetSystemFont(
                    wxSYS_DEFAULT_GUI_FONT))
                self.__radio_size = (3*dc.GetCharHeight())/2
            label = self.GetLabel()
            if label:
                w, h = self.GetTextExtent(label)
                w += self.__radio_size + self.GetCharWidth()
                if h < self.__radio_size: h = self.__radio_size
            else: w = h = self.__radio_size;
            return w, h

    # end of class wxGladeRadioButton
else: wxGladeRadioButton = wxRadioButton

#---------------------  Selection Markers  ----------------------------------

class SelectionTag(wxPanel):
    """\
    This is one of the small black squares that appear at the corners of the
    active widgets
    """
    def __init__(self, parent, pos=None):
        kwds = { 'size': (7, 7) }
        if pos: kwds['position'] = pos
        wxPanel.__init__(self, parent, -1, **kwds)
        self.SetBackgroundColour(wxBLACK)
        self.Hide()

# end of class SelectionTag

class SelectionMarker:
    """\
    Collection of the 4 SelectionTagS for each widget
    """
    def __init__(self, owner, parent, visible=False):
        self.visible = visible
        self.owner = owner
        self.parent = parent
        if wxPlatform == '__WXMSW__': self.parent = owner
        self.tags = [ SelectionTag(self.parent) for i in range(4) ]
        self.update()
        if visible:
            for t in self.tags: t.Show()

    def update(self, event=None):
        if self.owner is self.parent: x, y = 0, 0
        else: x, y = self.owner.GetPosition()
        w, h = self.owner.GetClientSize()
        def position(j):
            if not j: return x, y            # top-left
            elif j == 1: return x+w-7, y     # top-right
            elif j == 2: return x+w-7, y+h-7 # bottom-right
            else: return x, y+h-7            # bottom-left
        for i in range(len(self.tags)):
            self.tags[i].SetPosition(position(i))
        if event: event.Skip()

    def Show(self, visible):
        self.visible = visible
        for tag in self.tags: tag.Show(visible)

    def Destroy(self):
        for tag in self.tags: tag.Destroy()
        self.tags = None

    def Reparent(self, parent):
        self.parent = parent
        for tag in self.tags: tag.Reparent(parent)

# end of class SelectionMarker

#----------------------------------------------------------------------------

import common
_encode = common._encode_from_xml

def color_to_string(color):
    """\
    returns the hexadecimal string representation of the given color:
    for example: wxWHITE ==> #ffffff
    """
    import operator
    return '#' + reduce(operator.add, map(lambda s: '%02x' % s, color.Get()))

def string_to_color(color):
    """\
    returns the wxColour which corresponds to the given
    hexadecimal string representation:
    for example: #ffffff ==> wxColour(255, 255, 255)
    """
    if len(color) != 7: raise ValueError
    return apply(wxColour, [int(color[i:i+2], 16) for i in range(1, 7, 2)])

    
def get_toplevel_parent(obj):
    if not isinstance(obj, wxWindow): window = obj.widget
    else: window = obj
    while window and not window.IsTopLevel():
        window = window.GetParent()
    return window


if wxPlatform == '__WXGTK__':
    # default wxMenu seems to have probles with SetTitle on GTK
    class wxGladePopupMenu(wxMenu):
        def __init__(self, title):
            wxMenu.__init__(self)
            self.TITLE_ID = wxNewId()
            self.Append(self.TITLE_ID, title)
            self.AppendSeparator()

        def SetTitle(self, title):
            self.SetLabel(self.TITLE_ID, title)

else: wxGladePopupMenu = wxMenu
