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

    def __init__(self, name, parent, bmp_file, pos):
        ManagedBase.__init__(self, name, 'wxBitmapButton', parent, pos)
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
        self.widget = wx.BitmapButton(self.parent_window.widget, self.id, bmp, style=self.style)
        self._set_preview_bitmaps()

    def properties_changed(self, modified=None):
        "update label (and size if label/stockitem have changed)"
        BitmapMixin._properties_changed(self, modified)
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)


def builder(parent, pos):
    "factory function for EditBitmapButton objects"
    name = common.root.get_next_name('bitmap_button_%d', parent)
    bitmap = misc.RelativeFileSelector("Select the image for the button")
    with parent.frozen():
        editor = EditBitmapButton(name, parent, bitmap, pos)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(attrs, parent, pos=None):
    "factory to build EditBitmapButton objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    editor = EditBitmapButton(label, parent, '', pos)
    if attrs.input_file_version and attrs.input_file_version<(0,9):
        # backwards compatibility
        editor.properties["style"].set_to_default()
    return editor


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditBitmapButton'] = EditBitmapButton
    common.widgets['EditBitmapButton'] = builder
    common.widgets_from_xml['EditBitmapButton'] = xml_builder

    return common.make_object_button('EditBitmapButton', 'bitmap_button.xpm')
