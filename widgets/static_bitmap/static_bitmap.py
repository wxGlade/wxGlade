"""
wxStaticBitmap objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import common, config
from edit_windows import ManagedBase, EditStylesMixin
from gui_mixins import BitmapMixin
from tree import Node
import new_properties as np


class EditStaticBitmap(ManagedBase, EditStylesMixin, BitmapMixin):
    "Class to handle wxStaticBitmap objects"
    update_widget_style = False
    _PROPERTIES = ["Widget", "bitmap", "attribute", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    ManagedBase.MOVE_PROPERTY(PROPERTIES, "attribute", "name")
    _PROPERTY_LABELS = {"attribute":'Store as attribute'}
    _PROPERTY_HELP = {"attribute":'Store instance as attribute of window class; e.g. self.bitmap_1 = wx.wxStaticBitmap'
                                  '(...)\nWithout this, you can not access the bitmap from your program.'}

    def __init__(self, name, parent, id, bmp_file, sizer, pos):
        ManagedBase.__init__(self, name, 'wxStaticBitmap', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        filedialog_style = wx.FD_OPEN | wx.FD_FILE_MUST_EXIST  # for the following two properties
        self.bitmap    = np.BitmapProperty(bmp_file)
        self.attribute = np.CheckBoxProperty(False, default_value=False)

    def create_widget(self):
        bmp = self.get_preview_obj_bitmap()
        self.widget = wx.StaticBitmap(self.parent.widget, self.id, bmp)
        if wx.Platform == '__WXMSW__':
            def get_best_size():
                bmp = self.widget.GetBitmap()
                if bmp and bmp.IsOk():
                    return bmp.GetWidth(), bmp.GetHeight()
                return wx.StaticBitmap.GetBestSize(self.widget)
            self.widget.GetBestSize = get_best_size

    def properties_changed(self, modified=None):
        "update label (and size if label/stockitem have changed)"
        if not modified or "bitmap" in modified and self.widget:
            bmp = self.get_preview_obj_bitmap(self.bitmap)
            self.widget.SetBitmap(bmp)

            self._set_widget_best_size()

        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)


def builder(parent, sizer, pos, number=[1]):
    "factory function for EditStaticBitmap objects"
    name = 'bitmap_%s' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'bitmap_%s' % number[0]
    bitmap = wx.FileSelector(_("Select the image"))
    with parent.frozen():
        static_bitmap = EditStaticBitmap(name, parent, wx.NewId(), bitmap, sizer, pos)
        static_bitmap.properties["style"].set_to_default()
        static_bitmap.check_defaults()
        node = Node(static_bitmap)
        static_bitmap.node = node
        if parent.widget: static_bitmap.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditStaticBitmap objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    bitmap = EditStaticBitmap(label, parent, wx.NewId(), '', sizer, pos)
    #sizer.set_item(bitmap.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(bitmap)
    bitmap.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return bitmap


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widgets['EditStaticBitmap'] = builder
    common.widgets_from_xml['EditStaticBitmap'] = xml_builder

    return common.make_object_button('EditStaticBitmap', 'static_bitmap.xpm')
