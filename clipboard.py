"""
Support for cut & paste of wxGlade widgets

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import logging, os.path
import compat

import wx


# Format used by wxGlade for the clipboard.
try:
    widget_data_format = wx.CustomDataFormat("wxglade_widget")
except AttributeError:
    widget_data_format = wx.DataFormat("wxglade_widget")


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


def get_copy(widget):
    xml_unicode = compat.StringIO()
    widget.node.write(xml_unicode, 0)
    flag = option = border = None
    flag = widget.properties.get("flag")
    if flag is not None: flag = flag.get_string_value()
    #if isinstance(widget, edit_windows.ManagedBase):
        ## an element in a sizer
        
    #else: # a sizer   XXX old style
        #flag = widget.esm_border.get_string_style()
    #option = widget.get_option()
    #border = widget.get_border()
    proportion = getattr(widget, "proportion", 0)
    border  = getattr(widget, "border", 0)
    clipboard_data = widget2clipboard( proportion, flag, border, xml_unicode.getvalue() )
    return clipboard_data


def copy(widget):
    "Store a widget copy into the clipboard; returns True on success"
    if wx.TheClipboard.Open():
        try:
            clipboard_data = get_copy(widget)
            wdo = wx.CustomDataObject(widget_data_format)
            wdo.SetData(clipboard_data)
            if not wx.TheClipboard.SetData(wdo):
                logging.debug(_("Data can't be copied to clipboard."))
                return False
            return True
        finally:
            wx.TheClipboard.Close()
    else:
        logging.info(_("Clipboard can't be opened."))
        return False


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


# D&D support (thanks to Chris Liechti)
class FileDropTarget(wx.FileDropTarget):
    def __init__(self, parent):
        wx.FileDropTarget.__init__(self)
        self.parent = parent

    def OnDropFiles(self, x, y, filenames):
        if len(filenames) > 1:
            wx.MessageBox( _("Please only drop one file at a time"), "wxGlade", wx.ICON_ERROR )
        elif filenames:
            path = filenames[0]
            if os.path.exists(path) and self.parent.ask_save():
                self.parent._open_app(path)
                self.parent.cur_dir = os.path.dirname(path)

