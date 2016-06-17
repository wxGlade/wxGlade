"""
Miscellaneous stuff, used in many parts of wxGlade

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import config

import StringIO
import logging
import os
import re
import types
import wx

focused_widget = None
"""\
If not None, this is the currently selected widget - This is different from
tree.WidgetTree.cur_widget because it takes into account also SizerSlot
objects

this is an implementation hack, used to handle keyboard shortcuts for
popup menus properly (for example, to ensure that the object to remove is
the currently highlighted one, ecc...)
"""

use_menu_icons = None

currently_under_mouse = None
"""\
If not None, this is the SizerSlot wich has the "mouse focus": this is used
to restore the mouse cursor if the user cancelled the addition of a widget
"""

_get_xpm_bitmap_re = re.compile(r'"(?:[^"]|\\")*"')

_item_bitmaps = {}


class wxMSWRadioButton(wx.RadioButton):
    """\
    Custom wxRadioButton class which tries to implement a better
    GetBestSize than the default one for WXMSW (mostly copied from
    wxCheckBox::DoGetBestSize in checkbox.cpp)
    """
    __radio_size = None

    def GetBestSize(self):
        if not self.__radio_size:
            dc = wx.ScreenDC()
            dc.SetFont(wx.SystemSettings_GetFont(
                wx.SYS_DEFAULT_GUI_FONT))
            self.__radio_size = (3*dc.GetCharHeight())/2
        label = self.GetLabel()
        if label:
            w, h = self.GetTextExtent(label)
            w += self.__radio_size + self.GetCharWidth()
            if h < self.__radio_size:
                h = self.__radio_size
        else:
            w = h = self.__radio_size
        return w, h

# end of class wxGladeRadioButton


class wxGTKGladePopupMenu(wx.Menu):
    """\
    Default wxMenu seems to have probles with SetTitle on GTK
    """

    def __init__(self, title):
        wx.Menu.__init__(self)
        self.TITLE_ID = wx.NewId()
        item = self.Append(self.TITLE_ID, title)
        self.AppendSeparator()
        font = item.GetFont()
        font.SetWeight(wx.BOLD)
        item.SetFont(wx.Font(font.GetPointSize(), font.GetFamily(),
                             font.GetStyle(), wx.BOLD))

    def SetTitle(self, title):
        self.SetLabel(self.TITLE_ID, title)

if wx.Platform == '__WXMSW__':
    wxGladeRadioButton = wxMSWRadioButton
else:
    wxGladeRadioButton = wx.RadioButton


if wx.Platform == '__WXGTK__':
    wxGladePopupMenu = wxGTKGladePopupMenu
else:
    wxGladePopupMenu = wx.Menu


class SelectionTag(wx.Window):
    """\
    This is one of the small blue squares that appear at the corners of the
    active widgets
    """

    def __init__(self, parent):
        kwds = {'size': (7, 7)}
        wx.Window.__init__(self, parent, wx.ID_ANY, **kwds)
        self.SetBackgroundColour(wx.BLUE)
        self.Hide()

# end of class SelectionTag


class SelectionMarker(object):
    """\
    Collection of the 4 SelectionTagS for each widget
    """

    def __init__(self, owner, parent, visible=False):
        self.visible = visible
        self.owner = owner
        self.parent = parent
        if wx.Platform == '__WXMSW__':
            self.parent = owner
        self.tag_pos = None
        self.tags = None
        self.update()
        if visible:
            for t in self.tags:
                t.Show()

    def update(self, event=None):
        if self.owner is self.parent:
            x, y = 0, 0
        else:
            x, y = self.owner.GetPosition()
        w, h = self.owner.GetClientSize()

        def position(j):
            if not j:
                return x, y          # top-left
            elif j == 1:
                return x+w-7, y      # top-right
            elif j == 2:
                return x+w-7, y+h-7  # bottom-right
            else:
                return x, y+h-7      # bottom-left

        self.tag_pos = [position(i) for i in range(4)]
        if self.visible:
            if not self.tags:
                self.tags = [SelectionTag(self.parent) for i in range(4)]
            for i in range(4):
                self.tags[i].SetPosition(self.tag_pos[i])
        if event:
            event.Skip()

    def Show(self, visible):
        if self.visible != visible:
            self.visible = visible
            if self.visible:
                if not self.tags:
                    self.tags = [SelectionTag(self.parent) for i in range(4)]
                for i in range(4):
                    self.tags[i].SetPosition(self.tag_pos[i])
                    self.tags[i].Show()
            else:
                for tag in self.tags:
                    tag.Destroy()
                self.tags = None

    def Destroy(self):
        if self.tags:
            for tag in self.tags:
                tag.Destroy()
            self.tags = None

    def Reparent(self, parent):
        self.parent = parent
        if self.tags:
            for tag in self.tags:
                tag.Reparent(parent)

# end of class SelectionMarker


class UnicodeStringIO(object):
    """\
    Wrapper class to store data in Unicode
    """

    def __init__(self, encoding=None):
        self.out = StringIO.StringIO()
        self.encoding = encoding

    def write(self, data):
        if self.encoding is not None and isinstance(data, types.UnicodeType):
            data = data.encode(self.encoding)
        self.out.write(data)

    def getvalue(self):
        return self.out.getvalue()

# end of class UnicodeStringIO


class AsciiStringIO(StringIO.StringIO):
    """\
    Wrapper class to store data in ASCII

    @ivar isUnicode: True if the conversion to ASCII has failed at least
                     one time
    @type isUnicode: bool
    """

    def __init__(self, buf=''):
        StringIO.StringIO.__init__(self, buf)
        self.isUnicode = isinstance(buf, types.UnicodeType)

    def write(self, s):
        if not s:
            return

        if not isinstance(s, types.StringTypes):
            s = str(s)

        if isinstance(s, types.UnicodeType):
            try:
                s = s.encode('ascii')
            except UnicodeEncodeError:
                self.isUnicode = True
        StringIO.StringIO.write(self, s)

# end of class AsciiStringIO


def bound(number, lower, upper):
    return min(max(lower, number), upper)


def capitalize(string):
    """\
    Return string with first character capitalised. Some acronym like
    XML, XRC.

    @param string: String to convert
    @type string:  str

    @note: Be carefully it possibly breaks i18n.
    @rtype: str
    """
    # Don't capitalise those terms
    if string.upper() in ['XML', 'XRC', 'URL']:
        return string.upper()

    return string.capitalize()


def color_to_string(color):
    """\
    returns the hexadecimal string representation of the given colour

    Example::
        >>> color_to_string(wx.Colour(255, 255, 255))
       '#ffffff'

    @rtype: str
    """
    import operator
    return '#' + reduce(operator.add, ['%02x' % bound(c, 0, 255) for c in
                                       color.Get()])


def string_to_color(color):
    """\
    Returns the wxColour which corresponds to the given
    hexadecimal string representation

    Example::
        >>> string_to_color("#ffffff")
        wx.Colour(255, 255, 255)

    @rtype: wx.Colour
    """
    if len(color) != 7:
        raise ValueError
    return apply(wx.Colour, [int(color[i:i + 2], 16) for i in range(1, 7, 2)])


def format_for_version(version):
    """\
    Return the version information in C{for_version} in a string.

    Example::
        >>> format_for_version((2, 8))
        '2.8'

    @rtype: str
    @see: L{wxglade.codegen.BaseLangCodeWriter.for_version}
    """
    return '%s.%s' % version


def format_supported_by(version):
    """\
    Return formatted version string.

    Example::
        >>> format_supported_by('wx3')
        '3'
        >>> format_supported_by('wx28')
        '2.8'

    @param version: Version as specified in L{config.widget_config}
    @type version:  str

    @rtype: str

    @see: L{wcodegen.BaseWidgetWriter.is_widget_supported()}
    @see: L{config.widget_config}
    """
    assert isinstance(version, str)

    if len(version) == 3:
        formatted = '%s' % version[2]
    elif len(version) == 4:
        formatted = '%s.%s' % (version[2], version[3])
    else:
        raise ValueError(_('Unknown version format for "%s"') %
                         repr(version))
    return formatted


def get_toplevel_parent(obj):
    if not isinstance(obj, wx.Window):
        window = obj.widget
    else:
        window = obj
    while window and not window.IsTopLevel():
        window = window.GetParent()
    return window


def get_toplevel_widget(widget):
    from edit_windows import EditBase, TopLevelBase
    from edit_sizers import Sizer
    if isinstance(widget, Sizer):
        widget = widget.window
    assert isinstance(widget, EditBase), \
        _("EditBase or SizerBase object needed")
    while widget and not isinstance(widget, TopLevelBase):
        widget = widget.parent
    return widget


def check_wx_version(major, minor=0, release=0, revision=0):
    """\
    returns True if the current wxPython version is at least
    major.minor.release
    """
    return wx.VERSION[:-1] >= (major, minor, release, revision)


def append_item(menu, id, text, xpm_file_or_artid=None):
    global use_menu_icons
    if use_menu_icons is None:
        use_menu_icons = config.preferences.use_menu_icons
    item = wx.MenuItem(menu, id, text)
    if wx.Platform == '__WXMSW__':
        path = 'msw/'
    else:
        path = 'gtk/'
    path = os.path.join(config.icons_path, path)
    if use_menu_icons and xpm_file_or_artid is not None:
        bmp = None
        if not xpm_file_or_artid.startswith('wxART_'):
            try:
                bmp = _item_bitmaps[xpm_file_or_artid]
            except KeyError:
                f = os.path.join(path, xpm_file_or_artid)
                if os.path.isfile(f):
                    bmp = _item_bitmaps[xpm_file_or_artid] = \
                        wx.Bitmap(f, wx.BITMAP_TYPE_XPM)
                else:
                    bmp = None
        else:
            # xpm_file_or_artid is an id for wx.ArtProvider
            bmp = wx.ArtProvider.GetBitmap(
                xpm_file_or_artid, wx.ART_MENU, (16, 16))
        if bmp is not None:
            try:
                item.SetBitmap(bmp)
            except AttributeError:
                pass
    menu.AppendItem(item)


def _remove():
    global focused_widget
    if focused_widget is not None:
        focused_widget.remove()
        focused_widget = None


def _cut():
    global focused_widget
    if focused_widget is not None:
        try:
            focused_widget.clipboard_cut()
        except AttributeError:
            pass
        else:
            focused_widget = None


def _copy():
    if focused_widget is not None:
        try:
            focused_widget.clipboard_copy()
        except AttributeError:
            pass


def _paste():
    if focused_widget is not None:
        try:
            focused_widget.clipboard_paste()
        except AttributeError:
            pass


accel_table = [
    (0, wx.WXK_DELETE, _remove),
    (wx.ACCEL_CTRL, ord('C'), _copy),
    (wx.ACCEL_CTRL, ord('X'), _cut),
    (wx.ACCEL_CTRL, ord('V'), _paste),
]
"""\
accelerator table to enable keyboard shortcuts for the popup menus of the
various widgets (remove, cut, copy, paste)
"""


def _reverse_dict(src):
    """\
    Returns a dictionary whose keys are 'src' values and values 'src' keys.
    """
    ret = {}
    for key, val in src.iteritems():
        ret[val] = key
    return ret


#-----------------------------------------------------------------------------
# helper functions to work with a Unicode-enabled wxPython
#-----------------------------------------------------------------------------


def streq(s1, s2):
    """\
    Returns True if the strings or unicode objects s1 and s2 are equal, i.e.
    contain the same text. Appropriate encoding/decoding are performed to
    make the comparison

    @rtype: bool
    """
    try:
        return s1 == s2
    except UnicodeError:
        if isinstance(s1, types.UnicodeType):
            s1 = s1.encode(common.app_tree.app.encoding)
        else:
            s2 = s2.encode(common.app_tree.app.encoding)
        return s1 == s2


def wxstr(s, encoding=None):
    """\
    Converts the object s to str or unicode, according to what wxPython
    expects.

    @rtype: str
    """
    if encoding is None:
        if common.app_tree is None:
            return str(s)
        else:
            encoding = common.app_tree.app.encoding
    if wx.USE_UNICODE:
        if not isinstance(s, types.UnicodeType):
            return unicode(str(s), encoding)
        else:
            return s
    else:
        if isinstance(s, types.UnicodeType):
            return s.encode(encoding)
        else:
            return str(s)


def design_title(title):
    return _('Design - <%s>') % title


def get_xpm_bitmap(path):
    bmp = wx.NullBitmap
    if not os.path.exists(path):
        if '.zip' in path:
            import zipfile
            archive, name = path.split('.zip', 1)
            archive += '.zip'
            if name.startswith(os.sep):
                name = name.split(os.sep, 1)[1]
            if zipfile.is_zipfile(archive):
                # extract the XPM lines...
                try:
                    data = zipfile.ZipFile(archive).read(name)
                    data = [d[1:-1] for d in _get_xpm_bitmap_re.findall(data)]
##                     logging.debug("DATA:")
##                     for d in data:
##                         logging.debug(d)
                    bmp = wx.BitmapFromXPMData(data)
                except:
                    logging.exception(_('Internal Error'))
                    bmp = wx.NullBitmap
    else:
        bmp = wx.Bitmap(path, wx.BITMAP_TYPE_XPM)
    return bmp


def get_relative_path(path, for_preview=False):
    """\
    Get an absolute path relative to the current output directory (where the
    code is generated).
    """
    if os.path.isabs(path):
        return path
    p = common.app_tree.app.output_path
    if for_preview:
        p = getattr(common.app_tree.app, 'real_output_path', u'')
    d = os.path.dirname(p)
    if d:
        path = os.path.join(d, path)
    else:
        path = os.path.abspath(path)
    return path
