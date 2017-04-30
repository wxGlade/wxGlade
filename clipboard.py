"""
Support for cut & paste of wxGlade widgets

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging, os.path
import compat, common, misc
import edit_sizers

import wx


# Format used by wxGlade for the clipboard.
try:
    DataFormat = wx.CustomDataFormat
except AttributeError: # Phoenix
    DataFormat = wx.DataFormat


widget_data_format = DataFormat("wxglade.widget")  # a serialized widget
sizer_data_format  = DataFormat("wxglade.sizer")   # a serialized sizer
window_data_format = DataFormat("wxglade.window")  # a toplevel window


_current_drag_source = None
def set_drag_source(widget=None):
    # to be called when a drag operation starts or ends
    global _current_drag_source
    _current_drag_source = widget


def begin_drag(window, widget):
    do = get_data_object(widget)
    set_drag_source(widget)

    if isinstance(widget, edit_sizers.Sizer):
        msg = "Move sizer to empty or populated slot to insert, to a sizer to append; hold Ctrl to copy"
    elif widget._is_toplevel:
        msg = "Move window to application object; hold Ctrl to copy"
    else:
        msg = "Move control to empty or populated slot to insert, to a sizer to append; hold Ctrl to copy"
    common.palette.user_message( msg )

    drop_source = wx.DropSource(window)
    drop_source.SetData(do)
    drop_source.DoDragDrop(True)
    set_drag_source(None)


class DropTarget(wx.DropTarget):
    # widget drag & drop support; for tree and also for the design window
    def __init__(self, window, toplevel=False):
        wx.DropTarget.__init__(self)
        self.window = window  # window should have methods: check_drop_compatibility, drop
        self._create_data_objects(toplevel)
        self.SetDataObject(self.data_object)
        self._last_check = None  # will be set to x,y,result if a compatibility check was done
        self.fmt = None  # the received format

    def _create_data_objects(self, toplevel=False):
        data_objects = {}
        data_object = wx.DataObjectComposite()
        formats = [widget_data_format, sizer_data_format]
        if toplevel: formats.append(window_data_format)
        for fmt in formats:
            do = wx.CustomDataObject(fmt)
            data_objects[fmt.GetId()] = do
            data_object.Add(do)

        self.data_objects = data_objects
        self.data_object  = data_object

    def _get_received_format(self):
        if self.fmt is None:
            # unfortunately, there seems to be no way to identify the data format without actually receiving the data
            self.GetData()
            self.fmt = self.data_object.GetReceivedFormat().GetId()
        return self.fmt

    def _check_compatibility(self, x,y):
        # check whether the dragged item is compatible to the widget at position (x,y)
        widget = self.window.find_widget_by_pos(x,y)
        if widget is None: return False

        if _current_drag_source is not None:
            # drag within application: avoid dragging of an item on itself or it's child
            if widget.node is _current_drag_source.node:            return False
            if widget.node.has_ancestor(_current_drag_source.node): return False
            return widget.check_compatibility(_current_drag_source, report=False)

        # drag from outside
        fmt = self._get_received_format().split(".")[-1]
        return widget.check_compatibility(None, fmt, report=False)

    def OnDragOver(self, x,y, default):
        # continuously called while the mouse is over the target should return the desired operation or wx.DragNone
        # check only if position changed
        if self._last_check and x==self._last_check[0] and y==self._last_check[1]:
            compatible = self._last_check[2]
        else:
            compatible = self._check_compatibility(x, y)
            self._last_check = (x,y,compatible)
        return compatible and default or wx.DragNone

    def OnData(self, x,y,default):
        compatible = self._check_compatibility(x,y)
        if not compatible: return wx.DragCancel

        dst_widget = self.window.find_widget_by_pos(x,y)

        dst_node = dst_widget.node

        if _current_drag_source:
            src_widget = _current_drag_source  # was set in begin_drag
            src_node = src_widget.node

            copy = (default==wx.DragCopy)
            if not copy and _current_drag_source is misc.focused_widget:
                if hasattr(_current_drag_source, "parent"):
                    misc.set_focused_widget(_current_drag_source.parent)
                elif hasattr(_current_drag_source, "window"):  # a sizer
                    misc.set_focused_widget(_current_drag_source.window)

        if compatible=="AddSlot":
            # dropped on a sizer -> add slot
            dst_widget._add_slot()
            dst_widget.layout()
            dst_widget = dst_widget.children[-1].item # the slot
        elif compatible=="Slot":
            # insert a slot or fill empty slot
            pos = dst_widget.pos
            dst_widget.sizer._insert_slot(pos)
            dst_widget = dst_widget.sizer.children[pos].item # the slot

        if not hasattr(dst_widget, "clipboard_paste"):
            return wx.DragCancel

        # use cut and paste functionality from clipboard to do the actual work #########################################
        fmt = self._get_received_format()
        data = self.data_objects[fmt].GetData()  # the data as string
        self.fmt = None

        if _current_drag_source and not copy:
            src_widget.remove()

        dst_widget.clipboard_paste(None, data)
        common.app_tree.expand(dst_node)
        return default

    def OnLeave(self):
        self.fmt = None


def get_data_object(widget):
    # build the XML string
    xml_unicode = compat.StringIO()
    widget.node.write(xml_unicode, 0)
    flag = option = border = None
    flag = widget.properties.get("flag")
    if flag is not None: flag = flag.get_string_value()
    proportion = getattr(widget, "proportion", 0)
    border  = getattr(widget, "border", 0)
    data = widget2clipboard( proportion, flag, border, xml_unicode.getvalue() )
    # make a data object
    if isinstance(widget, edit_sizers.Sizer):
        do = wx.CustomDataObject(sizer_data_format)
    elif getattr(widget, "_is_toplevel", False):
        do = wx.CustomDataObject(window_data_format)
    else:
        do = wx.CustomDataObject(widget_data_format)
    do.SetData(data)
    return do


def widget2clipboard(option, flag, border, xml_unicode):
    """Pickle all parameter to store them as a string in the clipboard.
    
    option, flag, border: widget layout properties
    xml_unicode: XML representation of this widget"""

    clipboard_data = compat.pickle.dumps((option, flag, border, xml_unicode))
    return clipboard_data


def clipboard2widget(clipboard_data):
    """Convert widget data prepared in widget2clipboard() back to single values.

    Returns a list [option (proportions), flag, border and widget in XML representation]"""

    option, flag, border, xml_unicode = compat.pickle.loads(clipboard_data)

    # remove the dirt at the end of XML representation
    bound = xml_unicode.rfind('>') + 1
    xml_unicode = xml_unicode[:bound]

    # option, flag and border are integers.
    if option is not None: option = int(option)
    if border is not None: border = int(border)

    return option, flag, border, xml_unicode


def copy(widget):
    "Store a widget copy into the clipboard; returns True on success"
    if not wx.TheClipboard.Open(): return False
    try:
        wdo = get_data_object(widget)
        return wx.TheClipboard.SetData(wdo)
    finally:
        wx.TheClipboard.Close()



def cut(widget):
    "Store a copy of self into the clipboard and delete the widget; returns True on success"
    if copy(widget):
        widget.remove()
        return True
    else:
        return False


def paste(parent, sizer, pos, clipboard_data=None):
    """Copies a widget (and all its children) from the clipboard to the given
    destination (parent, sizer and position inside the sizer). Returns True on success.

    parent: Parent widget of the widget to add
    sizer: Sizer to place widget in
    pos: Position inside the sizer"""
    if clipboard_data is None:
        if wx.TheClipboard.Open():
            try:
                if wx.TheClipboard.IsSupported(widget_data_format):
                    data_object = wx.CustomDataObject(widget_data_format)
                    if not wx.TheClipboard.GetData(data_object):
                        logging.debug(_("Data can't be copied from clipboard."))
                        return False
                else:
                    wx.MessageBox( _("The clipboard doesn't contain wxGlade widget data."),
                                   _("Information"), wx.OK | wx.CENTRE | wx.ICON_INFORMATION )
                    return False
            finally:
                wx.TheClipboard.Close()
        else:
            logging.info(_("Clipboard can't be opened."))
            return False
        clipboard_data = data_object.GetData()

    option, flag, border, xml_unicode = clipboard2widget( clipboard_data )
    if xml_unicode:
        import xml_parse
        try:
            wx.BeginBusyCursor()
            # widget representation is still unicode, but parser need UTF8
            xml_utf8 = xml_unicode.encode('utf8')
            parser = xml_parse.ClipboardXmlWidgetBuilder(parent, sizer, pos, option, flag, border)
            parser.parse_string(xml_utf8)
            return True  # Widget hierarchy pasted.
        finally:
            wx.EndBusyCursor()
    return False  # There's nothing to paste.


def check(*formats):
    "check whether wxglade formats are on the clipboard"
    if not wx.TheClipboard.IsOpened():
        try:
            wx.TheClipboard.Open()
            if "widget" in formats and wx.TheClipboard.IsSupported(widget_data_format): return True
            if "window" in formats and wx.TheClipboard.IsSupported(window_data_format): return True
            if "sizer"  in formats and wx.TheClipboard.IsSupported(sizer_data_format):  return True
        finally:
            wx.TheClipboard.Close()
    return False


# D&D support (thanks to Chris Liechti)
class FileDropTarget(wx.FileDropTarget):
    def __init__(self, parent):
        wx.FileDropTarget.__init__(self)
        self.parent = parent

    def OnDropFiles(self, x, y, filenames):
        if len(filenames) > 1:
            wx.MessageBox( _("Please only drop one file at a time"), "wxGlade", wx.ICON_ERROR )
            return False
        elif filenames:
            path = filenames[0]
            if os.path.exists(path) and self.parent.ask_save():
                self.parent._open_app(path)
                self.parent.cur_dir = os.path.dirname(path)
            return True
        return False

