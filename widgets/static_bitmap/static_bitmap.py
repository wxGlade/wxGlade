"""
wxStaticBitmap objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import common, misc
from edit_windows import ManagedBase, EditStylesMixin
from gui_mixins import BitmapMixin
import new_properties as np


class EditStaticBitmap(BitmapMixin, ManagedBase, EditStylesMixin):
    "Class to handle wxStaticBitmap objects"
    update_widget_style = False
    WX_CLASS = 'wxStaticBitmap'
    _PROPERTIES = ["Widget", "bitmap", "attribute", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    ManagedBase.MOVE_PROPERTY(PROPERTIES, "attribute", "name")
    _PROPERTY_LABELS = {"attribute":'Store as attribute'}
    _PROPERTY_HELP = {"attribute":'Store instance as attribute of window class; e.g. self.bitmap_1 = wx.wxStaticBitmap'
                                  '(...)\nWithout this, you can not access the bitmap from your program.'}

    def __init__(self, name, parent, bmp_file, pos):
        ManagedBase.__init__(self, name, 'wxStaticBitmap', parent, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.bitmap    = np.BitmapProperty(bmp_file)
        self.attribute = np.CheckBoxProperty(False, default_value=False)

    def create_widget(self):
        bmp = self.get_preview_obj_bitmap()
        self.widget = wx.StaticBitmap(self.parent_window.widget, self.id, bmp)
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


def builder(parent, pos, bitmap=None):
    "factory function for EditStaticBitmap objects"
    name = common.root.get_next_name('bitmap_%d', parent)
    if bitmap is None:
        bitmap = misc.RelativeFileSelector("Select the image")
        if bitmap is None: return
    with parent.frozen():
        editor = EditStaticBitmap(name, parent, bitmap, pos)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(attrs, parent, pos=None):
    "factory to build EditStaticBitmap objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    return EditStaticBitmap(label, parent, '', pos)


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widget_classes['EditStaticBitmap'] = EditStaticBitmap
    common.widgets['EditStaticBitmap'] = builder
    common.widgets_from_xml['EditStaticBitmap'] = xml_builder

    return common.make_object_button('EditStaticBitmap', 'static_bitmap.xpm')
