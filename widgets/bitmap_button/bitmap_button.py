"""\
wxBitmapButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import common, config, compat
from edit_windows import ManagedBase, EditStylesMixin
from gui_mixins import BitmapMixin
from tree import Node
import new_properties as np


class EditBitmapButton(ManagedBase, EditStylesMixin, BitmapMixin):
    "Class to handle wxBitmapButton objects"

    _PROPERTIES = ["Widget", "bitmap", "disabled_bitmap", "pressed_bitmap", "current_bitmap", "focus_bitmap",
                   "default", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, id, bmp_file, sizer, pos):
        ManagedBase.__init__(self, name, 'wxBitmapButton', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)
        BitmapMixin.__init__(self)

        # initialise instance properties
        self.bitmap          = np.BitmapProperty(bmp_file)
        self.disabled_bitmap = np.BitmapPropertyD("")
        self.pressed_bitmap  = np.BitmapPropertyD(min_version=(3,0))
        self.current_bitmap  = np.BitmapPropertyD(min_version=(3,0))
        self.focus_bitmap    = np.BitmapPropertyD(min_version=(3,0))
        self.default         = np.CheckBoxProperty(False, default_value=False)

    def create_widget(self):
        bmp = self.get_preview_obj_bitmap()
        #try:
        self.widget = wx.BitmapButton(self.parent.widget, self.id, bmp, style=self.style)
        self.widget._bitmap_size = tuple(bmp.Size)
        self._set_preview_bitmaps()#include_bitmap=False)

        #except AttributeError:
            #self.widget = wx.BitmapButton(self.parent.widget, self.id, bmp)

    def properties_changed(self, modified=None):
        "update label (and size if label/stockitem have changed)"
        BitmapMixin._properties_changed(self, modified)
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)


def builder(parent, sizer, pos, number=[1]):
    "factory function for EditBitmapButton objects"
    name = 'bitmap_button_%s' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'bitmap_button_%s' % number[0]
    bitmap = wx.FileSelector(_("Select the image for the button"))
    with parent.frozen():
        button = EditBitmapButton(name, parent, wx.NewId(), bitmap, sizer, pos)
        button.properties["style"].set_to_default()
        button.check_defaults()
        node = Node(button)
        button.node = node
        if parent.widget: button.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditBitmapButton objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    button = EditBitmapButton(label, parent, wx.NewId(), '', sizer, pos)
    if attrs.input_file_version and attrs.input_file_version<(0,9):
        # backwards compatibility
        button.properties["style"].set_to_default()
    #sizer.set_item(button.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(button)
    button.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return button


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditBitmapButton'] = builder
    common.widgets_from_xml['EditBitmapButton'] = xml_builder

    return common.make_object_button('EditBitmapButton', 'bitmap_button.xpm')
