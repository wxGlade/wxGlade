"""
wxStaticBitmap objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import math
import wx

import common
import compat
import config
from edit_windows import ManagedBase, EditStylesMixin
from gui_mixins import BitmapMixin
from tree import Tree
from widget_properties import *


class EditStaticBitmap(ManagedBase, EditStylesMixin, BitmapMixin):
    """\
    Class to handle wxStaticBitmap objects
    """

    update_widget_style = False

    def __init__(self, name, parent, id, bmp_file, sizer, pos,
                 property_window, show=True):

        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxStaticBitmap', parent, id, sizer,
                             pos, property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.attribute = True
        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wx.ALL
        self.set_bitmap(bmp_file)

        # initialise properties remaining staff
        access = self.access_functions
        properties = self.properties

        access['bitmap'] = (self.get_bitmap, self.set_bitmap)
        properties['bitmap'] = FileDialogProperty(
            self, 'bitmap', style=wx.FD_OPEN | wx.FD_FILE_MUST_EXIST,
            can_disable=False, label=_("Bitmap"))
        properties['bitmap'].set_tooltip(self.bitmap_tooltip_text)

        access['attribute'] = (self.get_attribute, self.set_attribute)
        properties['attribute'] = CheckBoxProperty(
            self, 'attribute', label=_('Store as attribute'),
            write_always=True)

        access['style'] = (self.get_style, self.set_style)
        properties['style'] = CheckListProperty(
            self, 'style', self.widget_writer)

    def create_widget(self):
        bmp = self.get_preview_obj_bitmap()
        self.widget = wx.StaticBitmap(self.parent.widget, self.id, bmp)
        if wx.Platform == '__WXMSW__':
            def get_best_size():
                bmp = self.widget.GetBitmap()
                if bmp and bmp.Ok():
                    return bmp.GetWidth(), bmp.GetHeight()
                return wx.StaticBitmap.GetBestSize(self.widget)
            self.widget.GetBestSize = get_best_size

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.ScrolledWindow(self.notebook, -1, style=wx.TAB_TRAVERSAL)
        szr = wx.BoxSizer(wx.VERTICAL)
        self.properties['bitmap'].display(panel)
        self.properties['attribute'].display(panel)
        self.properties['style'].display(panel)
        szr.Add(self.properties['bitmap'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['attribute'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['style'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        w, h = panel.GetClientSize()
        self.notebook.AddPage(panel, "Widget")
        self.property_window.Layout()
        panel.SetScrollbars(1, 5, 1, int(math.ceil(h / 5.0)))

    def get_attribute(self):
        return self.attribute

    def set_attribute(self, value):
        self.attribute = int(value)

    def get_bitmap(self):
        return self.bitmap

    def set_bitmap(self, value):
        self.bitmap = value
        if self.widget:
            bmp = self.get_preview_obj_bitmap()
            self.widget.SetBitmap(bmp)
            self.set_size("%s, %s" % tuple(self.widget.GetBestSize()))

# end of class EditStaticBitmap


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditStaticBitmap objects.
    """
    name = 'bitmap_%s' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'bitmap_%s' % number[0]
    bitmap = wx.FileSelector(_("Select the image"))
    static_bitmap = EditStaticBitmap(name, parent, wx.NewId(), bitmap, sizer,
                                     pos, common.property_panel)
    node = Tree.Node(static_bitmap)
    static_bitmap.node = node
    static_bitmap.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditStaticBitmap objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    bitmap = EditStaticBitmap(label, parent, wx.NewId(), '', sizer, pos,
                              common.property_panel)
    sizer.set_item(bitmap.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border)
    node = Tree.Node(bitmap)
    bitmap.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return bitmap


def initialize():
    """\
    initialization function for the module: returns a wx.BitmapButton to be
    added to the main palette.
    """
    common.widgets['EditStaticBitmap'] = builder
    common.widgets_from_xml['EditStaticBitmap'] = xml_builder

    return common.make_object_button('EditStaticBitmap', 'static_bitmap.xpm')
