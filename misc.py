"""
Miscellaneous stuff, used in many parts of wxGlade

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common, config, compat
import logging, os, re, time
import wx



_get_xpm_bitmap_re = re.compile(r'"(?:[^"]|\\")*"')
_item_bitmaps = {}


# handle currently focused widget ######################################################################################

focused_widget = None  # the currently selected widget in GUI mode (for tree and property_panel)
_next_focused_widget = None  # for delayed setting, to ensure that only the last call has an effect
focused_time = 0.0  # sometimes, widgets ignore focus related events during a dead time of e.g. 50ms after setting focus

flush_functions = []  # these will be called before the focused widget is set; currently only used by new_properties


def set_focused_widget(widget, force=False, delayed=False):
    if not config.use_gui: return
    global focused_widget, _next_focused_widget, focused_time

    for f in flush_functions:
        f()

    if delayed:
        _next_focused_widget = widget
        wx.CallAfter(_set_focused_widget, widget, force)
        return

    _next_focused_widget = None  # cancel pending delayed call

    # set focused widget; tell tree and property panel
    if focused_widget:
        focused_widget.update_view(selected=False)
        set_focus = focused_widget.WX_CLASS in ("wxSpinCtrl", "wxSpinCtrlDouble")
    else:
        set_focus = False
    focused_widget = widget
    common.app_tree.set_current_widget(widget)
    common.property_panel.set_widget(widget, force)
    if common.history: common.history.set_widget(widget)
    common.main.set_widget(widget)  # to update menu and toolbar
    if common.shell:
        common.shell.txt_path.SetValue( widget and widget.get_path() or "" )

    focused_time = time.time()
    if widget and widget.widget:
        # ensure that it is visible and selection is displayed, if applicable
        show_widget(widget)
        widget.update_view(selected=True)
        # set focus in Design window to move away from certain widgets
        if set_focus and hasattr(widget.widget, "HasFocus") and not widget.widget.HasFocus():
            widget.widget.SetFocus()


def _set_focused_widget(widget, force=False):
    if not widget is _next_focused_widget:
        # another call was made inbetween
        return
    set_focused_widget(widget, force)


def rebuild_tree(widget=None, recursive=True, focus=True, freeze=False):
    # re-build tree control for the widget and it's children; set focus to it; called after creation or modification
    common.root.saved = False
    common.app_tree.build(widget, recursive, freeze)
    if focus and widget is not None:
        set_focused_widget(widget, force=widget==common.root)


def show_widget(widget):
    # ensure that notebook pages are selected such that widget is visible
    if not widget.widget: return
    while True:
        if not widget.parent: break  # Application.node is None
        parent = widget.parent
        if parent.__class__.__name__=="EditNotebook":
            # a widget under a wxNotebook without a panel: select page
            if parent.widget and widget in parent.children:
                parent.widget.SetSelection( parent.children.index(widget) )

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
        assert isinstance(self.parent, wx.Window)
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
                if not self.parent: return
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
    # see: wxglade.codegen.BaseLangCodeWriter.for_version
    return '%s.%s' % version

def format_supported_by(version):
    "Return formatted version string; e.g. 'wx3' -> '3' or 'wx28' -> '2.8'"
    # see config.widget_config, wcodegen.BaseWidgetWriter.is_widget_supported(), config.widget_config
    assert version.startswith("wx")
    if len(version) == 3: return '%s' % version[2]
    if len(version) == 4: return '%s.%s' % (version[2], version[3])
    raise ValueError(_('Unknown version format for "%s"') % repr(version))


def get_toplevel_parent(obj):
    if isinstance(obj, wx.Sizer):
        obj = obj.ContainingWindow
    elif not isinstance(obj, wx.Window):
        obj = obj.widget
    if hasattr(obj, "ContainingWindow"):
        obj = obj.ContainingWindow
    while obj and not obj.IsTopLevel():
        obj = obj.GetParent()
    return obj


def get_toplevel_widget(widget):
    from edit_windows import EditBase, TopLevelBase
    from edit_sizers import SizerSlot
    if widget.IS_SIZER: widget = widget.window
    assert isinstance(widget, (EditBase,SizerSlot)), _("EditBase or SizerBase object needed")
    while widget and not isinstance(widget, TopLevelBase):
        widget = widget.parent
    return widget


def check_wx_version_at_least(major, minor=0, release=0, revision=0):
    "returns True if the current wxPython version is at least major.minor.release"
    return wx.VERSION[:-1] >= (major, minor, release, revision)



########################################################################################################################
# error/warning/info messages
def error_message(msg, title="Error", display_traceback=False):
    if config.use_gui:
        wx.MessageBox( _(msg), _(title), wx.OK | wx.CENTRE | wx.ICON_ERROR )
    else:
        logging.error(msg)

def info_message(msg, title="Information"):
    if config.use_gui:
        wx.MessageBox( _(msg), _(title), wx.OK | wx.CENTRE | wx.ICON_INFORMATION )
    else:
        logging.info(msg)

def warning_message(msg, title="Warning"):
    if config.use_gui:
        wx.MessageBox( _(msg), _(title), wx.OK | wx.CENTRE | wx.ICON_WARNING )
    else:
        logging.warning(msg)


########################################################################################################################
# menu helpers

def append_menu_item(menu, id, text, file_or_artid=None, **kwargs): # XXX change: move id to the end of the argument list?
    if compat.IS_CLASSIC and "helpString" in kwargs:
        kwargs["help"] = kwargs["helpString"]
        del kwargs["helpString"]
    item = wx.MenuItem(menu, id, text, **kwargs)
    if file_or_artid is not None:
        path = 'msw/'  if wx.Platform == '__WXMSW__'  else  'gtk/'
        path = os.path.join(config.icons_path, path)
        bmp = None
        if not isinstance(file_or_artid, bytes) or not file_or_artid.startswith(b'wxART_'):
            try:
                bmp = _item_bitmaps[file_or_artid]
            except KeyError:
                f = os.path.join(path, file_or_artid)
                if os.path.isfile(f):
                    bmp = _item_bitmaps[file_or_artid] = wx.Bitmap(f)
                else:
                    bmp = None
        else:
            # file_or_artid is an id for wx.ArtProvider
            bmp = wx.ArtProvider.GetBitmap( file_or_artid, wx.ART_MENU, (16, 16) )
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


def _can_remove():
    global focused_widget
    if focused_widget is None or not hasattr(focused_widget, "remove"): return False
    if focused_widget.IS_SLOT:
        # XXX change this later on, to remove e.g. notebook pages, but not when parent.CHILDREN is an int
        if focused_widget.parent.CHILDREN == -1: return True
        if focused_widget.parent.WX_CLASS in ("wxNotebook",): return True
        if not focused_widget.parent.IS_SIZER or focused_widget.sizer._IS_GRIDBAG:
            wx.Bell()
            return False
    return True


def _remove():
    global focused_widget
    if not _can_remove(): return
    previous_focus = focused_widget
    focused_widget.remove()
    if focused_widget==previous_focus:
        # usually, remove() should set the focus to the empty slot or the parent of the widget, if not clear it here
        focused_widget = None

def _cut():
    global focused_widget
    if not _can_remove():
        wx.Bell()
        return
    clipboard.cut(focused_widget)
    #focused_widget = None

def _copy():
    if focused_widget is None or focused_widget.IS_SLOT:
        wx.Bell()
        return
    clipboard.copy(focused_widget)

def _paste():
    if focused_widget is None: return
    if not hasattr(focused_widget, "clipboard_paste"):
        wx.Bell()
        return
    clipboard.paste(focused_widget)

def _insert():
    global focused_widget
    if not focused_widget: return
    parent = focused_widget.parent
    if not parent or not parent.IS_SIZER: return
    if parent._IS_GRIDBAG: return
    #method = getattr(focused_widget.sizer, "insert_slot", None)
    method = getattr(parent, "insert_slot", None)
    if method: method(focused_widget.index)

def _add():
    global focused_widget
    if not focused_widget: return
    method = getattr(focused_widget, "add_slot", None)
    if not method:
        if not hasattr(focused_widget, "sizer"): return
        method = getattr(focused_widget.sizer, "add_slot", None)
    if method: method()


currently_under_mouse = None  # set when the mouse enters a sizer slot widget; see edit_base.Slot/SizeSlot

def _cancel():
    if not common.adding_widget: return
    common.adding_widget = common.adding_sizer = False
    common.widget_to_add = None
    if currently_under_mouse is not None:
        currently_under_mouse.SetCursor(wx.STANDARD_CURSOR)
    common.app_tree.SetCursor(wx.STANDARD_CURSOR)
    compat.SetToolTip(common.app_tree, "")
    common.main.user_message("Canceled")

def drop():
    global focused_widget
    method = None
    if not focused_widget: return
    if not focused_widget.check_drop_compatibility():
        wx.Bell()
        return
    if focused_widget.IS_SLOT:
        method = getattr(focused_widget, "on_drop_widget", None)
    elif common.adding_sizer:
        method = getattr(focused_widget, "drop_sizer", None)
    #if not method:
        #if not hasattr(focused_widget, "sizer"): return
        #method = getattr(focused_widget.sizer, "add_slot", None)
    if method: method(None, False)  # don't reset, but continue adding


def navigate(up):
    # move up or down in tree
    focus = focused_widget
    if not focus:
        # get from Tree widget
        item = common.app_tree.GetFocusedItem()
        focus = common.app_tree._GetItemData(item)
    if focus is None: return
    siblings = [focus]  if focus.IS_ROOT else  focus.parent.get_all_children()
    if not siblings: return
    idx = siblings.index(focus)
    if up:
        if idx>0:
            focus = siblings[idx-1]
            children = focus.get_all_children()
            if children: focus = children[-1]
        else:
            # no upper sibling -> go up
            focus = focus.parent
    else:
        # down: look for children
        children = focus.get_all_children()
        if children:
            # go to first child
            focus = children[0]
        else:
            if idx+1<len(siblings):
                # go to next sibling
                focus = siblings[idx+1]
            else:
                # go up one or more levels
                while True:
                    if not focus.parent: return
                    siblings = focus.parent.get_all_children()
                    if siblings:
                        idx = siblings.index(focus)
                        if idx+1<len(siblings):
                            focus = siblings[idx+1]
                            break
                    focus = focus.parent

    if focus: set_focused_widget(focus)


# accelerator tables to enable keyboard shortcuts for the popup menus of the various widgets (remove, cut, copy, paste)
# only for the editing windows:
accel_table_editors = {
    ("",  wx.WXK_DELETE):(_remove, ()),
    ("C", ord('C')):     (_copy,   ()),
    ("C", ord('X')):     (_cut,    ()),
    ("C", ord('V')):     (_paste,  ()),
    ("C", ord('I')):     (_insert, ()),
    ("C", ord('A')):     (_add,    ()),

    ("", wx.WXK_UP):     (navigate,  (True, )),
    ("", wx.WXK_DOWN):   (navigate,  (False,)),

    ("",  wx.WXK_ESCAPE):(_cancel, ()),

    ("", wx.WXK_RETURN): (drop,    ()),
}

if wx.Platform == "__WXMAC__":
    # on Windows this one would go up in the hierarchy when in the Tree control
    accel_table_editors["", wx.WXK_BACK] = (_remove, ())


# for the palette window
accel_table_editors_palette = {
    ("",  wx.WXK_ESCAPE):(_cancel, ())
}

# for all windows
accel_table = {
    ("C", ord('Z')):     ((common, "history","undo"), "focused_widget"),
    ("C", ord('Y')):     ((common, "history","redo"), "focused_widget"),
    ("C", ord('R')):     ((common, "history","repeat"), "focused_widget"),

    ("",  wx.WXK_F2):    ((common,"main","show_tree"),            ()),
    ("",  wx.WXK_F3):    ((common,"main","show_props_window"),    ()),
    ("",  wx.WXK_F4):    ((common,"main","show_palette"),         ()),
    ("",  wx.WXK_F5):    ((common,"main","preview"),              ()),
    ("",  wx.WXK_F6):    ((common,"main","show_design_window"),   ()),
    ("",  wx.WXK_F7):    ((common,"main","create_shell_window"),  ()),

    ("",  wx.WXK_F8):    ((common,"main","show_props_window"),    ("Common",)),
    ("C", ord('M')):     ((common,"main","show_props_window"),    ("Common",)),

    ("",  wx.WXK_F9):    ((common,"main","show_props_window"),    ("Layout",)),
    ("C", ord('L')):     ((common,"main","show_props_window"),    ("Layout",)),

    ("",  wx.WXK_F10):   ((common,"main","show_props_window"),    ("Widget",)),
    ("C", ord('W')):     ((common,"main","show_props_window"),    ("Widget",)),

    ("",  wx.WXK_F11):   ((common,"main","show_props_window"),    ("Events",)),
    ("C", ord('E')):     ((common,"main","show_props_window"),    ("Events",)),

    ("",  wx.WXK_F12):   ((common,"main","show_props_window"),    ("Code",)),
    ("C", ord('D')):     ((common,"main","show_props_window"),    ("Code",)),  # -> 'O'? and use 'D' for Design window?

    ("C", ord('P')):     ((common,"main","pin_design_window"),    ()),

    ("A", ord('1')):     ((common,"main","switch_layout"),        (0,)),
    ("A", ord('2')):     ((common,"main","switch_layout"),        (1,)),
    ("A", ord('3')):     ((common,"main","switch_layout"),        (2,)),

    ("C", ord('S')):     ((common,"main","save_app"),             ()),
    ("C", ord('G')):     ((common,"root","generate_code"), ()),

    ("C", ord('N')):     ((common,"main","new_app"),              ()), 
    ("C", ord('O')):     ((common,"main","open_app"),             ()),
    ("C", ord('Q')):     ((common,"main","Close"),                ()),
}


def handle_key_event(event, window_type, window=None):
    "centralized key event handler; called from EVT_CHAR_HOOK handlers"
    # window_type: "design", "preview", "editor", "tree"    "properties", "palette" or None
    evt_key = event.GetKeyCode()

    tables = [accel_table]  # for all windows
    if window_type in ("editor", "design", "tree"):
        tables += [accel_table_editors]
    elif window_type == "palette":
        tables.append( accel_table_editors_palette )

    evt_flags = []
    if event.ControlDown(): evt_flags.append("C")
    if event.ShiftDown():   evt_flags.append("S")
    if event.AltDown():     evt_flags.append("A")
    evt_flags = "".join(evt_flags)

    handler = None
    for table in tables:
        handler = table.get( (evt_flags,evt_key) )
        if handler: break
    if not handler:
        event.Skip()
        return

    function, args = handler
    if isinstance(function, tuple):
        # turn tuple of (modulename,...) into a function
        obj = function[0]
        for i in range(1,len(function)):
            obj = getattr(obj, function[i])
        function = obj
    if isinstance(args, str):
        args = (globals()[args],)
    wx.CallAfter(function, *args)

# for key handler in palette window:
palette_hotkeys = {"S":"Sizers"}  # key to section name


def _reverse_dict(src):
    "Returns a dictionary whose keys are 'src' values and values 'src' keys."
    ret = {}
    for key, val in getattr(src, "iteritems", src.items)(): # Python 2/3
        ret[val] = key
    return ret


import contextlib
@contextlib.contextmanager
def dummy_contextmanager():
    # used like 'with parent and parent.frozen() or dummy_contextmanager()
    yield


if wx.Platform == '__WXMAC__':
    # on Mac OS we need to disable STAY_ON_TOP when a dialog is to be shown
    @contextlib.contextmanager
    def disable_stay_on_top(widget):
        # used like 'with parent and parent.frozen() or dummy_contextmanager()
        restore = False
        tl = get_toplevel_parent(widget)
        if isinstance(tl, wx.Frame) and tl.GetWindowStyle() & wx.STAY_ON_TOP:
            tl.ToggleWindowStyle(wx.STAY_ON_TOP)
            restore = True
        yield
        if restore:
            tl.ToggleWindowStyle(wx.STAY_ON_TOP)
else:
    # on other platforms, we don't need it
    @contextlib.contextmanager
    def disable_stay_on_top(widget):
        yield
    

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
        encoding = common.root.encoding
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
                    bmp = wx.BitmapFromXPMData(data)  # XXX
                except:
                    logging.exception(_('Internal Error'))
                    bmp = wx.NullBitmap
    else:
        bmp = wx.Bitmap(path)
    return bmp


def get_absolute_path(path, for_preview=False):
    "Get an absolute path relative to the current output directory (where the code is generated)."
    if os.path.sep!="\\":
        path = path.replace("\\", os.path.sep)
    if os.path.isabs(path):
        return path
    p = common.root.output_path
    if for_preview:
        p = getattr(common.root, 'real_output_path', u'')
    if os.path.sep!="\\":
        p = p.replace("\\", os.path.sep)
    if not os.path.isabs(p):
        # a path relative to the application filename
        appdir = os.path.dirname(common.root.filename)
        p = os.path.abspath( os.path.join(appdir, p) )

    if not os.path.isdir(p): p = os.path.dirname(p)

    return os.path.abspath( os.path.join(p, path) )


def get_relative_path(filename):
    # returns a relative path if filename is inside the project directory
    if filename:
        project_path = common.root.filename
        if project_path:
            project_path = os.path.abspath( os.path.dirname(project_path) )
            if filename.startswith(project_path):
                filename = "./" + os.path.relpath(filename, project_path)
        if os.path.sep=="\\":
            filename = filename.replace("\\", "/")
    return filename


def RelativeFileSelector(message="Select the file", *args, **kwargs):
    # returns a relative path if the selected file is inside the project directory
    return get_relative_path( wx.FileSelector(_(message), *args, **kwargs) )
