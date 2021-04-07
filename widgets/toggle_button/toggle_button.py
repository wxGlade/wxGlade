"""\
wxToggleButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
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

    def __init__(self, name, parent, index, label):
        ManagedBase.__init__(self, name, parent, index)
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
        self.widget = wx.ToggleButton(self.parent_window.widget, wx.ID_ANY, self.label, style=self.style)
        self.widget.SetValue(self.value)
        self.widget.Bind(wx.EVT_TOGGLEBUTTON, self.on_set_focus, id=self.widget.GetId())
        BitmapMixin._set_preview_bitmaps(self)

    def _properties_changed(self, modified, actions):
        if not modified or "value" in modified and self.widget:
            self.widget.SetValue(self.value)

        if not modified or "label" in modified:
            if self.widget:
                self.widget.SetLabel(self.label)
            if modified: actions.update(("layout", "label"))

        BitmapMixin._properties_changed(self, modified, actions)
        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)


def builder(parent, index):
    "factory function for EditToggleButton objects"
    name = parent.toplevel_parent.get_next_contained_name('button_%d')
    with parent.frozen():
        editor = EditToggleButton(name, parent, index, name)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory to build EditToggleButton objects from a XML file"
    return EditToggleButton(name, parent, index, '')


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditToggleButton'] = EditToggleButton
    common.widgets['EditToggleButton'] = builder
    common.widgets_from_xml['EditToggleButton'] = xml_builder

    return common.make_object_button('EditToggleButton', 'toggle_button.png')
