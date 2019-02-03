"""
Support for cut & paste of wxGlade widgets

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2016-2018 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging, sys, os.path
import compat, common, config, misc
import edit_sizers

import wx


# Format used by wxGlade for the clipboard.
if compat.IS_CLASSIC:
    DataFormat = wx.CustomDataFormat
else:
    DataFormat = wx.DataFormat


widget_data_format = DataFormat("wxglade.widget")  # a serialized widget
sizer_data_format  = DataFormat("wxglade.sizer")   # a serialized sizer
window_data_format = DataFormat("wxglade.window")  # a toplevel window


_current_drag_source = None  # reference to drag start; used when dragging within application

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
    common.main.user_message( msg )

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
        if widget is None:
            return (False, "No widget found")

        if not widget.is_sizer and not widget._is_toplevel and getattr(widget,"sizer",None):  # for a toplevel window, sizer is the child
            if widget.sizer._IS_GRIDBAG and not isinstance(widget, edit_sizers.SizerSlot):
                # for GridBagSizer we have cells, so we don't shift items
                return (False, "Can only paste into empty slots")

        if _current_drag_source is not None:
            # drag within application: avoid dragging of an item on itself or it's child
            if widget.node is _current_drag_source.node:            return (False, "Can't paste item on itself")
            if widget.node.has_ancestor(_current_drag_source.node): return (False, "Can't paste item into itself")
            return widget.check_compatibility(_current_drag_source)

        # drag from outside
        fmt = self._get_received_format().split(".")[-1]
        return widget.check_compatibility(None, fmt)

    def OnDragOver(self, x,y, default):
        # continuously called while the mouse is over the target should return the desired operation or wx.DragNone
        # check only if position changed
        if not self._last_check or x!=self._last_check[0] or y!=self._last_check[1]:
            self._last_check = (x,y, self._check_compatibility(x, y)[0] )
        return self._last_check[2] and default or wx.DragNone

    def OnData(self, x,y,default):
        compatible, message = self._check_compatibility(x,y)
        if not compatible: return wx.DragCancel

        dst_widget = self.window.find_widget_by_pos(x,y)

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
            dst_widget = dst_widget.children[-1] # the slot
        elif compatible=="Slot":
            # insert a slot or fill empty slot
            pos = dst_widget.pos
            dst_widget.sizer._insert_slot(pos)
            dst_widget = dst_widget.sizer.children[pos] # the slot

        if not hasattr(dst_widget, "clipboard_paste"):
            return wx.DragCancel

        # use cut and paste functionality from clipboard to do the actual work #########################################
        fmt = self._get_received_format()
        data = self.data_objects[fmt].GetData()  # the data as string
        self.fmt = None
        if wx.Platform=="__WXMAC__":
            # delay action, as otherwise there will be a segmentation fault; 50ms seems to be enough
            wx.CallLater(50, self._OnData, _current_drag_source, src_widget, dst_widget, data, copy)
        else:
            wx.CallAfter(self._OnData, _current_drag_source, src_widget, dst_widget, data, copy)

        return default

    def _OnData(self, drag_source, src_widget, dst_widget, data, copy):
        if drag_source and not copy:
            with src_widget.frozen():
                src_widget.remove()
                dst_widget.clipboard_paste(data)
        else:
            dst_widget.clipboard_paste(data)

        common.app_tree.expand(dst_widget.node)

    def OnLeave(self):
        self.fmt = None


def get_data_object(widget):
    data = dump_widget(widget)
    # make a data object
    if isinstance(widget, edit_sizers.Sizer):
        do = wx.CustomDataObject(sizer_data_format)
    elif getattr(widget, "_is_toplevel", False):
        do = wx.CustomDataObject(window_data_format)
    else:
        do = wx.CustomDataObject(widget_data_format)
    do.SetData(data)
    cdo = wx.DataObjectComposite()
    cdo.Add(do)
    if widget.name: cdo.Add(wx.TextDataObject(widget.name), True)  # the widget name as text, preferred
    return cdo


def dump_widget(widget):
    "build the XML string and pickle it together with the layout properties"
    xml_unicode = []
    widget.node.write(xml_unicode, 0)
    flag = option = span = border = None
    flag = widget.properties.get("flag")
    if flag is not None: flag = flag.get_string_value()
    proportion = getattr(widget, "proportion", 0)
    span = getattr(widget, "span", (1,1))
    border  = getattr(widget, "border", 0)
    return widget2clipboard( proportion, span, flag, border, "".join(xml_unicode) )


def widget2clipboard(option, span, flag, border, xml_unicode):
    """Pickle all parameter to store them as a string in the clipboard.
    
    option, flag, border: widget layout properties
    xml_unicode: XML representation of this widget"""
    clipboard_data = compat.pickle.dumps((option, span, flag, border, xml_unicode))
    return clipboard_data


def clipboard2widget(clipboard_data):
    """Convert widget data prepared in widget2clipboard() back to single values.

    Returns a list [option (proportions), flag, border and widget in XML representation]"""
    if isinstance(clipboard_data, memoryview) and sys.version_info[0]<3:
        clipboard_data = clipboard_data.tobytes()
    option, span, flag, border, xml_unicode = compat.pickle.loads(clipboard_data)

    # remove the dirt at the end of XML representation
    bound = xml_unicode.rfind('>') + 1
    xml_unicode = xml_unicode[:bound]

    # option, flag and border are integers.
    if option is not None: option = int(option)
    if border is not None: border = int(border)

    return option, span, flag, border, xml_unicode


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


#def paste(parent, sizer, pos, clipboard_data=None):
def paste(widget):
    """Copies a widget (and all its children) from the clipboard to the given
    destination (parent, sizer and position inside the sizer). Returns True on success."""
    error = None
    if not wx.TheClipboard.Open():
        misc.error_message( "Clipboard can't be opened." )
        return False

    try:
        data_object = None
        for fmt in [widget_data_format,sizer_data_format,window_data_format]:
            if wx.TheClipboard.IsSupported(fmt):
                data_object = wx.CustomDataObject(fmt)
                break
        if data_object is None:
            misc.info_message( "The clipboard doesn't contain wxGlade widget data." )
            return False
        if not wx.TheClipboard.GetData(data_object):
            misc.error_message( "Data can't be copied from clipboard." )
            return False
    finally:
        wx.TheClipboard.Close()
    format_name = data_object.GetFormat().GetId().split(".")[1]  # e.g. 'wxglade.widget' -> 'widget'
    compatible, message = widget.check_compatibility(None, format_name)
    if not compatible:
        wx.Bell()
        if message:
            misc.error_message(message)
        return False
    if not widget.clipboard_paste(data_object.GetData()):
        misc.error_message("Paste failed")


def _paste(parent, sizer, pos, clipboard_data):
    "parse XML and insert widget"
    option, span, flag, border, xml_unicode = clipboard2widget( clipboard_data )
    if not xml_unicode: return False
    import xml_parse
    try:
        wx.BeginBusyCursor()
        # widget representation is still unicode, but parser need UTF8
        xml_utf8 = xml_unicode.encode('utf8')
        parser = xml_parse.ClipboardXmlWidgetBuilder(parent, sizer, pos, option, span, flag, border)
        with parent and parent.frozen() or misc.dummy_contextmanager():
            parser.parse_string(xml_utf8)
        common.app_tree.saved = False
        return True  # Widget hierarchy pasted.
    except xml_parse.XmlParsingError:
        if config.debugging: raise
        return False
    finally:
        wx.EndBusyCursor()


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
