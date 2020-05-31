"""\
wxToggleButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2016-2020 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import common, config
from edit_windows import ManagedBase, EditStylesMixin
from gui_mixins import BitmapMixin
import new_properties as np


class EditToggleButton(BitmapMixin, ManagedBase, EditStylesMixin):
    "Class to handle wxToggleButton objects"

    WX_CLASS = 'wxToggleButton'
    _PROPERTIES = ["Widget", "label", "value",
                   "bitmap", "disabled_bitmap", "pressed_bitmap", "current_bitmap", "focus_bitmap",
                   "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    _PROPERTY_LABELS = {"value":"Clicked"}

    def __init__(self, name, parent, pos, label):
        ManagedBase.__init__(self, name, parent, pos)
        EditStylesMixin.__init__(self)

        # initialise instance variable
        self.label = np.TextProperty(label, multiline="grow")
        self.value = np.CheckBoxProperty(False, default_value=False)
        # bitmaps are only for >= 3.0
        self.bitmap          = np.BitmapPropertyD(min_version=(3,0))
        self.disabled_bitmap = np.BitmapPropertyD(min_version=(3,0))
        self.pressed_bitmap  = np.BitmapPropertyD(min_version=(3,0))
        self.current_bitmap  = np.BitmapPropertyD(min_version=(3,0))
        self.focus_bitmap    = np.BitmapPropertyD(min_version=(3,0))

    def create_widget(self):
        self.widget = wx.ToggleButton(self.parent_window.widget, self.id, self.label)
        self.widget.SetValue(self.value)
        self.widget.Bind(wx.EVT_TOGGLEBUTTON, self.on_set_focus, id=self.id)
        BitmapMixin._set_preview_bitmaps(self)

    def properties_changed(self, modified):
        if not modified or "value" in modified and self.widget:
            self.widget.SetValue(self.value)

        if not modified or "label" in modified:
            if self.widget:
                self.widget.SetLabel(self.label)
                self._set_widget_best_size()
            if common.app_tree is not None:
                common.app_tree.refresh(self, refresh_label=True, refresh_image=False)

        BitmapMixin._properties_changed(self, modified)
        self._set_widget_best_size()
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)


def builder(parent, pos):
    "factory function for EditToggleButton objects"
    name = parent.toplevel_parent.get_next_contained_name('button_%d')
    with parent.frozen():
        editor = EditToggleButton(name, parent, pos, name)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, pos):
    "factory to build EditToggleButton objects from a XML file"
    return EditToggleButton(name, parent, pos, '')


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditToggleButton'] = EditToggleButton
    common.widgets['EditToggleButton'] = builder
    common.widgets_from_xml['EditToggleButton'] = xml_builder

    return common.make_object_button('EditToggleButton', 'toggle_button.xpm')
