"""\
wxBitmapButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import common, misc
from edit_windows import ManagedBase, EditStylesMixin
from gui_mixins import BitmapMixin
import new_properties as np


class EditBitmapButton(BitmapMixin, ManagedBase, EditStylesMixin):
    "Class to handle wxBitmapButton objects"

    WX_CLASS = "wxBitmapButton"
    _PROPERTIES = ["Widget", "bitmap", "disabled_bitmap", "pressed_bitmap", "current_bitmap", "focus_bitmap",
                   "default", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, index, bmp_file, style=0):
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self, style)
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
        self.widget = wx.BitmapButton(self.parent_window.widget, wx.ID_ANY, bmp, style=self.style)
        self._set_preview_bitmaps()

    def _properties_changed(self, modified, actions):
        "update label (and size if label/stockitem have changed)"
        BitmapMixin._properties_changed(self, modified, actions)
        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)


def builder(parent, index):
    "factory function for EditBitmapButton objects"
    name = parent.toplevel_parent.get_next_contained_name('bitmap_button_%d')
    bitmap = misc.RelativeFileSelector("Select the image for the button")
    with parent.frozen():
        editor = EditBitmapButton(name, parent, index, bitmap)
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory to build EditBitmapButton objects from a XML file"
    editor = EditBitmapButton(name, parent, index, '', 0)
    return editor


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditBitmapButton'] = EditBitmapButton
    common.widgets['EditBitmapButton'] = builder
    common.widgets_from_xml['EditBitmapButton'] = xml_builder

    return common.make_object_button('EditBitmapButton', 'bitmap_button.png')
