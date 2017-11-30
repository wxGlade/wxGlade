"""
Miscellaneous stuff, used in many parts of wxGlade

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common, config, compat
import logging, os, re
import wx


use_menu_icons = None

# the SizerSlot which has the "mouse focus"; used to restore the mouse cursor if the user cancelled adding a widget
currently_under_mouse = None

_get_xpm_bitmap_re = re.compile(r'"(?:[^"]|\\")*"')
_item_bitmaps = {}

focused_widget = None  # the currently selected widget in GUI mode (for tree and property_panel)

def set_focused_widget(widget, force=False):
    if not config.use_gui: return
    # set focused widget; tell tree and property panel
    global focused_widget
    if focused_widget:
        focused_widget.update_view(selected=False)
    focused_widget = widget
    common.app_tree.set_current_widget(widget)
    common.property_panel.set_widget(widget, force)
    if widget and widget.widget:
        # ensure that it is visible and selection is displayed, if applicable
        show_widget(widget)
        widget.update_view(selected=True)


def show_widget(widget):
    # ensure that notebook pages are selected such that widget is visible
    if not widget.widget: return
    while True:
        if not widget.node or not widget.node.parent: break  # Application.node is None
        parent = widget.node.parent.widget
        if parent.klass=="wxNotebook":
            # a wiget under a wxNotebook without a panel: select page
            if parent.widget and widget in parent.pages:
                parent.widget.SetSelection( parent.pages.index(widget) )

        widget = parent  # go up one level


class wxMSWRadioButton(wx.RadioButton):
    """Custom wxRadioButton class which tries to implement a better GetBestSize than the default one for WXMSW
    (mostly copied from wxCheckBox::DoGetBestSize in checkbox.cpp)"""
    __radio_size = None

    def GetBestSize(self):
        if not self.__radio_size:
            dc = wx.ScreenDC()
            dc.SetFont(compat.wx_SystemSettings_GetFont(wx.SYS_DEFAULT_GUI_FONT))
            self.__radio_size = (3*dc.GetCharHeight())//2
        label = self.GetLabel()
        if label:
            w, h = self.GetTextExtent(label)
            w += self.__radio_size + self.GetCharWidth()
            if h < self.__radio_size:
                h = self.__radio_size
        else:
            w = h = self.__radio_size
        return w, h


class wxGTKGladePopupMenu(wx.Menu):
    "Default wxMenu seems to have probles with SetTitle on GTK"

    def __init__(self, title):
        wx.Menu.__init__(self)
        self.TITLE_ID = wx.NewId()
        item = self.Append(self.TITLE_ID, title)
        self.AppendSeparator()
        font = item.GetFont()
        font.SetWeight(wx.BOLD)
        item.SetFont( wx.Font(font.GetPointSize(), font.GetFamily(), font.GetStyle(), wx.BOLD) )

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
    "This is one of the small blue squares that appear at the corners of the active widgets"
    def __init__(self, parent):
        kwds = {'size': (7, 7)}
        wx.Window.__init__(self, parent, wx.ID_ANY, **kwds)
        self.SetBackgroundColour(wx.BLUE)
        self.Hide()


class SelectionMarker(object):
    "Collection of the 4 SelectionTagS for each widget"
    def __init__(self, owner, parent, visible=False):
        self.visible = visible
        self.owner = owner
        self.parent = parent
        if wx.Platform == '__WXMSW__': self.parent = owner
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
        self.tag_pos = [(x,y),          (x+w-7, y),  # top-left,     top-right
                        (x+w-7, y+h-7), (x, y+h-7)]  # bottom-right, bottom-left
        if self.visible:
            if not self.tags:
                self.tags = [SelectionTag(self.parent) for i in range(4)]
            for i,pos in enumerate(self.tag_pos):
                self.tags[i].SetPosition(pos)
        if event:
            event.Skip()

    def Show(self, visible):
        if self.visible != visible:
            self.visible = visible
            if self.visible:
                if not self.tags:
                    self.tags = [SelectionTag(self.parent) for i in range(4)]
                for i,pos in enumerate(self.tag_pos):
                    self.tags[i].SetPosition(pos)
                    self.tags[i].Show()
            else:
                for tag in self.tags:
                    tag.Destroy()
                self.tags = None

    def Destroy(self):
        if self.tags is None: return
        for tag in self.tags:
            tag.Destroy()
        self.tags = None

    def Reparent(self, parent):
        self.parent = parent
        if self.tags is None: return
        for tag in self.tags:
            tag.Reparent(parent)


def bound(number, lower, upper):
    return min(max(lower, number), upper)


def capitalize(string):
    """Return string with first character capitalised. Some acronym like XML, XRC.
    @note: Be carefully it possibly breaks i18n."""
    # Don't capitalise those terms
    if string.upper() in ['XML', 'XRC', 'URL']:
        return string.upper()
    return string.capitalize()


def color_to_string(color):
    "returns the hexadecimal string representation of the given colour '#RRGGBB'"
    if color is None: return None
    return '#%.2x%.2x%.2x'%(color.Red(), color.Green(), color.Blue())

def string_to_color(color):
    "Hex string to wxColour instance; e.g. string_to_color('#ffffff') -> wx.Colour(255, 255, 255)"
    if len(color)==7: return wx.Colour( *[int(color[i:i + 2], 16) for i in range(1, 7, 2)] )
    if len(color)==9: return wx.Colour( *[int(color[i:i + 2], 16) for i in range(1, 9, 2)] )
    raise ValueError


def format_for_version(version):
    "Return the version information in a string; e.g. format_for_version((2, 8)) -> '2.8'"
    # see: L{wxglade.codegen.BaseLangCodeWriter.for_version}
    return '%s.%s' % version

def format_supported_by(version):
    "Return formatted version string; e.g. 'wx3' -> '3' or 'wx28' -> '2.8'"
    # see config.widget_config, wcodegen.BaseWidgetWriter.is_widget_supported(), config.widget_config
    assert version.startswith("wx")
    if len(version) == 3: return '%s' % version[2]
    if len(version) == 4: return '%s.%s' % (version[2], version[3])
    raise ValueError(_('Unknown version format for "%s"') % repr(version))


def get_toplevel_parent(obj):
    if not isinstance(obj, wx.Window):
        obj = obj.widget
    while obj and not obj.IsTopLevel():
        obj = obj.GetParent()
    return obj


def get_toplevel_widget(widget):
    from edit_windows import EditBase, TopLevelBase
    from edit_sizers import Sizer, SizerSlot
    if isinstance(widget, Sizer):
        widget = widget.window
    assert isinstance(widget, (EditBase,SizerSlot)), _("EditBase or SizerBase object needed")
    while widget and not isinstance(widget, TopLevelBase):
        widget = widget.parent
    return widget


def check_wx_version(major, minor=0, release=0, revision=0):
    "returns True if the current wxPython version is at least major.minor.release"
    return wx.VERSION[:-1] >= (major, minor, release, revision)



########################################################################################################################
# error/warning/info messages
def error_message(msg, title="Error", display_traceback=False):
    wx.MessageBox( _(msg), _(title), wx.OK | wx.CENTRE | wx.ICON_ERROR )

def info_message(msg, title="Information"):
    wx.MessageBox( _(msg), _(title), wx.OK | wx.CENTRE | wx.ICON_INFORMATION )

def warning_message(msg, title="Warning"):
    wx.MessageBox( _(msg), _(title), wx.OK | wx.CENTRE | wx.ICON_WARNING )


########################################################################################################################
# menu helpers

def append_menu_item(menu, id, text, xpm_file_or_artid=None): # XXX change: move id to the end of the argument list?
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
        if not xpm_file_or_artid.startswith(b'wxART_'):
            try:
                bmp = _item_bitmaps[xpm_file_or_artid]
            except KeyError:
                f = os.path.join(path, xpm_file_or_artid)
                if os.path.isfile(f):
                    bmp = _item_bitmaps[xpm_file_or_artid] = wx.Bitmap(f, wx.BITMAP_TYPE_XPM)
                else:
                    bmp = None
        else:
            # xpm_file_or_artid is an id for wx.ArtProvider
            bmp = wx.ArtProvider.GetBitmap( xpm_file_or_artid, wx.ART_MENU, (16, 16) )
        if bmp is not None:
            try:
                item.SetBitmap(bmp)
            except AttributeError:
                pass
    if compat.IS_CLASSIC:
        menu.AppendItem(item)
    else:
        menu.Append(item)
    return item


def bind_menu_item(widget, item, func, *args, **kwargs):
    "Bind a menu handler with immediate callback"
    def handler(event):
        func( *args, **kwargs )
    widget.Bind(wx.EVT_MENU, handler, item )

def bind_menu_item_after(widget, item, func, *args, **kwargs):
    "Bind a menu handler with later callback (via wxCallAfter)"
    widget.Bind(wx.EVT_MENU, exec_after(func, *args, **kwargs), item )


def exec_after(func, *args, **kwargs):
    "Execute the function away from calling context forward to the event handling mechanism of wxWidgets."
    return lambda e: wx.CallAfter(func, *args, **kwargs)


########################################################################################################################
# key handlers
import clipboard


def restore_focus(func):
    "try to restore the input focus"
    def wrapper(*args, **kwargs):
        focus = common.app_tree.FindFocus()
        ret = func(*args, **kwargs)
        if focus: focus.SetFocus()
        return ret
    wrapper.__name__ = func.__name__
    return wrapper


def _can_remove():
    global focused_widget
    if focused_widget is None or not hasattr(focused_widget, "remove"): return False
    if focused_widget.klass=="sizerslot" and hasattr(focused_widget, "sizer"):
        sizer = focused_widget.sizer
        if sizer.is_virtual() or focused_widget.sizer._IS_GRIDBAG:
            wx.Bell()
            return False
    return True

@restore_focus
def _remove():
    global focused_widget
    if not _can_remove(): return
    previous_focus = focused_widget
    focused_widget.remove()
    if focused_widget==previous_focus:
        # usually, remove() should set the focus to the empty slot or the parent of the widget, if not clear it here
        focused_widget = None

@restore_focus
def _cut():
    global focused_widget
    if not _can_remove(): return
    clipboard.cut(focused_widget)
    #focused_widget = None

def _copy():
    if focused_widget is None: return
    clipboard.copy(focused_widget)

@restore_focus
def _paste():
    if focused_widget is None: return
    if not hasattr(focused_widget, "clipboard_paste"):
        wx.Bell()
        return
    clipboard.paste(focused_widget)

@restore_focus
def _insert():
    global focused_widget
    if not focused_widget: return
    if not hasattr(focused_widget, "sizer") or not hasattr(focused_widget, "pos"): return
    if focused_widget.sizer._IS_GRIDBAG: return
    method = getattr(focused_widget.sizer, "insert_slot", None)
    if method: method(focused_widget.pos)

@restore_focus
def _add():
    global focused_widget
    if not focused_widget: return
    method = getattr(focused_widget, "add_slot", None)
    if not method:
        if not hasattr(focused_widget, "sizer"): return
        method = getattr(focused_widget.sizer, "add_slot", None)
    if method: method()

def _cancel():
    if not common.adding_widget: return
    common.adding_widget = common.adding_sizer = False
    common.widget_to_add = None


# accelerator table to enable keyboard shortcuts for the popup menus of the various widgets (remove, cut, copy, paste)
accel_table = [
    (0,                            wx.WXK_DELETE, _remove, ()),
    (wx.ACCEL_CTRL,                ord('C'),      _copy, ()),
    (wx.ACCEL_CTRL,                ord('X'),      _cut, ()),
    (wx.ACCEL_CTRL,                ord('V'),      _paste, ()),
    (wx.ACCEL_CTRL,                ord('Z'),      (common, "history","undo"), "focused_widget"),
    (wx.ACCEL_CTRL,                ord('Y'),      (common, "history","redo"), "focused_widget"),
    (wx.ACCEL_CTRL,                ord('R'),      (common, "history","repeat"), "focused_widget"),
    (wx.ACCEL_CTRL,                ord('I'),      _insert, ()),
    (wx.ACCEL_CTRL,                ord('A'),      _add, ()),
    (0,                            wx.WXK_ESCAPE, _cancel, ()),
    (0,                            wx.WXK_F2,     (common,"palette","show_tree"),            ()),
    (0,                            wx.WXK_F3,     (common,"palette","show_props_window"),    ()),
    (0,                            wx.WXK_F4,     (common,"palette","raise_all"),            ()),
    (0,                            wx.WXK_F5,     (common,"palette","preview"),              ()),
    (0,                            wx.WXK_F6,     (common,"palette","show_design_window"),   ()),
    (wx.ACCEL_CTRL,                ord('S'),      (common,"palette","save_app"),             ()),
    (wx.ACCEL_CTRL,                ord('G'),      (common,"app_tree","app","generate_code"), ()),
]

def on_key_down_event(event, is_filter=False):
    "centralized handler for Ctrl+C/X/V or Del key"
    evt_flags = 0
    if event.ControlDown(): evt_flags |= wx.ACCEL_CTRL
    if event.ShiftDown():   evt_flags |= wx.ACCEL_SHIFT
    evt_key = event.GetKeyCode()
    print("on_key_down_event", evt_flags, evt_key)
    if not is_filter:
        if focused_widget and evt_key in (wx.WXK_UP, wx.WXK_DOWN):
            obj = event.GetEventObject()
            toplevel_obj = obj.GetTopLevelParent()
            if toplevel_obj!=common.app_tree.GetParent() and toplevel_obj!=common.palette and toplevel_obj!=common.property_panel:
                # must be a design window
                navigate( evt_key==wx.WXK_UP )
                return

    for flags, key, function, args in accel_table:
        if evt_flags != flags or evt_key != key:
            continue
        if isinstance(function, tuple):
            # turn tuple of (modulename,...) into a function
            obj = function[0]
            for i in range(1,len(function)):
                obj = getattr(obj, function[i])
            function = obj
        if isinstance(args, str):
            args = (globals()[args],)
        wx.CallAfter(function, *args)
        return True
    # not handled
    if not is_filter:
        event.Skip()
    else:
        return False

if config.use_gui and hasattr(wx, "EventFilter") and not config.testing:
    # for filtering key down events
    class PreviewEventFilter(wx.EventFilter):
        def FilterEvent(self, event):
            t = event.GetEventType()
            if t == wx.EVT_KEY_DOWN.typeId:
                handled = on_key_down_event(event, is_filter=True)
                if handled:
                    return self.Event_Processed
            # Continue processing the event normally as well.
            return self.Event_Skip
    preview_event_filter = PreviewEventFilter()
else:
    preview_event_filter = None



def navigate(up):
    # must be a design window
    focused_item = focused_widget.node.item
    if up:
        item = common.app_tree.GetPrevSibling(focused_item)
        if not item:
            # no upper sibling -> go up
            item = common.app_tree.GetItemParent(focused_item)
            #item = common.app_tree.GetFirstChild(focused_item)
        else:
            # go down again
            while common.app_tree.ItemHasChildren(item):
                item = common.app_tree.GetLastChild(item)
    else:
        if common.app_tree.ItemHasChildren(focused_item):
            item, token = common.app_tree.GetFirstChild(focused_item)
        else:
            item = common.app_tree.GetNextSibling(focused_item)
            if not item:
                item = focused_item
                while True:
                    parent = common.app_tree.GetItemParent(item)
                    if not parent: return
                    if common.app_tree.ItemHasChildren(parent):
                        item = common.app_tree.GetNextSibling(parent)
                        if common.app_tree._GetItemData( common.app_tree.GetNextSibling(parent) ):
                            break
                    item = parent

    widget = getattr(common.app_tree._GetItemData(item), "widget", None)
    if not widget: return
    set_focused_widget(widget)



def _reverse_dict(src):
    "Returns a dictionary whose keys are 'src' values and values 'src' keys."
    ret = {}
    for key, val in getattr(src, "iteritems", src.items)(): # Python 2/3
        ret[val] = key
    return ret

########################################################################################################################
# key handlers
if wx.Platform == '__WXMAC__':
    # Mac has no Ctrl-Click -> use Shift-Click
    def event_modifier_copy(event):
        return event.ShiftDown()
else:
    def event_modifier_copy(event):
        return event.ControlDown()



#-----------------------------------------------------------------------------
# helper functions to work with a Unicode-enabled wxPython
#-----------------------------------------------------------------------------


def wxstr(s, encoding=None):
    "Converts the object s to str or unicode, according to what wxPython expects"
    if encoding is None:
        if common.app_tree is None:
            return str(s)
        encoding = common.app_tree.app.encoding
    if isinstance(s, compat.unicode): return s
    s = str(s)
    if isinstance(s, compat.unicode): return s
    return compat.unicode(s, encoding)


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
                    bmp = wx.BitmapFromXPMData(data)
                except:
                    logging.exception(_('Internal Error'))
                    bmp = wx.NullBitmap
    else:
        bmp = wx.Bitmap(path, wx.BITMAP_TYPE_XPM)
    return bmp


def get_absolute_path(path, for_preview=False):
    "Get an absolute path relative to the current output directory (where the code is generated)."
    if os.path.isabs(path):
        return path
    p = common.app_tree.app.output_path
    if for_preview:
        p = getattr(common.app_tree.app, 'real_output_path', u'')
    if not os.path.isabs(p):
        # a path relative to the application filename
        appdir = os.path.dirname(common.app_tree.app.filename)
        p = os.path.abspath( os.path.join(appdir, p) )

    if not os.path.isdir(p): p = os.path.dirname(p)

    return os.path.abspath( os.path.join(p, path) )
