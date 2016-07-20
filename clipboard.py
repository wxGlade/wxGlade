"""
Support for cut & paste of wxGlade widgets

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import cPickle
import logging
import StringIO
import wx


# Format used by wxGlade for the clipboard.
widget_data_format = wx.CustomDataFormat("wxglade_widget")


def widget2clipboard(option, flag, border, xml_unicode):
    """\
    Pickle all parameter to store them as a string in the clipboard.

    @param option: Widget layout proportions
    @type option:  str
    @param flag: Widget flags / styles
    @type flag:  str
    @param border: Widget border
    @type border:  str
    @param xml_unicode: XML representation of this widget
    @type xml_unicode: Unicode
    @return: Pickled parameters
    @rtype:  str

    @see: L{clipboard2widget()}
    """
    clipboard_data = cPickle.dumps((option, flag, border, xml_unicode))
    return clipboard_data


def clipboard2widget(clipboard_data):
    """\
    Convert widget data prepared in L{widget2clipboard()} back to single
    values.

    The values are option (proportions), flag, border and widget in XML
    representation. They will be returned in a list.

    @param clipboard_data: Widget data prepared in L{widget2clipboard()}
    @type clipboard_data:  str

    @rtype: list[int, str, int, str]

    @see: L{widget2clipboard()}
    """
    option, flag, border, xml_unicode = cPickle.loads(clipboard_data)

    # remove the dirt at the end of XML representation
    bound = xml_unicode.rfind('>') + 1
    xml_unicode = xml_unicode[:bound]

    # option, flag and border are integers.
    option = int(option)
    border = int(border)

    return option, flag, border, xml_unicode


def copy(widget):
    """\
    Store a widget copy into the clipboard

    @param widget: Widget to copy

    @return: True on success
    @rtype: bool
    """
    if wx.TheClipboard.Open():
        try:
            xml_unicode = StringIO.StringIO()
            widget.node.write(xml_unicode, 0)
            flag = widget.esm_border.get_string_style()
            option = widget.get_option()
            border = widget.get_border()
            clipboard_data = widget2clipboard(
                option, flag, border, xml_unicode.getvalue())
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
    """\
    Store a copy of self into the clipboard and delete the widget.

    @param widget: Widget to copy and delete

    @return: True on success
    @rtype: bool
    @see: L{copy()}
    """
    if copy(widget):
        widget.remove()
        return True
    else:
        return False


def paste(parent, sizer, pos):
    """\
    Copies a widget (and all its children) from the clipboard to the given
    destination (parent, sizer and position inside the sizer).

    @param parent: Parent widget of the widget to add

    @param sizer: Sizer to place widget in
    @type sizer: edit_sizers.edit_sizers.SizerBase | None

    @param pos: Position inside the sizer
    @type pos: int

    @return: True on success
    @rtype: bool
    """
    if wx.TheClipboard.Open():
        try:
            if wx.TheClipboard.IsSupported(widget_data_format):
                data_object = wx.CustomDataObject(widget_data_format)
                if not wx.TheClipboard.GetData(data_object):
                    logging.debug(_("Data can't be copied from clipboard."))
                    return False
            else:
                wx.MessageBox(
                    _("The clipboard doesn't contain wxGlade widget data."),
                    _("Information"),
                    wx.OK | wx.CENTRE | wx.ICON_INFORMATION,
                    )
                return False
        finally:
            wx.TheClipboard.Close()
    else:
        logging.info(_("Clipboard can't be opened."))
        return False

    option, flag, border, xml_unicode = clipboard2widget(
        data_object.GetData())
    if xml_unicode:
        import xml_parse
        try:
            wx.BeginBusyCursor()
            # widget representation is still unicode, but parser need UTF8
            xml_utf8 = xml_unicode.encode('utf8')
            parser = xml_parse.ClipboardXmlWidgetBuilder(
                parent, sizer, pos, option, flag, border)
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
            wx.MessageBox(
                _("Please only drop one file at a time"),
                "wxGlade",
                wx.ICON_ERROR)
        elif filenames:
            path = filenames[0]
            if self.parent.ask_save():
                self.parent._open_app(path)

# end of class FileDropTarget
