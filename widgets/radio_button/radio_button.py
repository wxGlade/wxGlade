"""\
wxRadioButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2020 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, config
from edit_windows import ManagedBase, EditStylesMixin
import new_properties as np
from misc import wxGladeRadioButton


class EditRadioButton(ManagedBase, EditStylesMixin):
    "Class to handle wxRadioButton objects"
    update_widget_style = False

    WX_CLASS = 'wxRadioButton'
    _PROPERTIES = ["Widget", "label", "clicked", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, label, pos):
        ManagedBase.__init__(self, name, 'wxRadioButton', parent, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.label   = np.TextProperty("", multiline="grow")
        self.clicked = np.CheckBoxProperty(False, default_value=False)

    def create_widget(self):
        self.widget = wxGladeRadioButton(self.parent_window.widget, self.id, self.label)
        self.widget.SetValue(self.clicked)
        self.widget.Bind(wx.EVT_CHECKBOX, lambda e: self.widget.SetValue(self.value))

    def _set_label(self):
        if not self.widget: return
        self.widget.SetLabel(self.label)
        if hasattr(self.parent, "set_item_best_size") and not self.properties['size'].is_active():
            self.parent.set_item_best_size(self, size=self.widget.GetBestSize())

    def properties_changed(self, modified):
        resize = False

        if not modified or "label" in modified:
            self._set_label()
            common.app_tree.refresh(self, refresh_label=True, refresh_image=False)

        if not modified or "clicked" in modified and self.widget:
            self.widget.SetValue(self.clicked)

        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)



def builder(parent, pos):
    "factory function for EditRadioButton objects"
    name = parent.toplevel_parent.get_next_contained_name('radio_btn_%d')
    with parent.frozen():
        editor = EditRadioButton(name, parent, name, pos)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(attrs, parent, pos=None):
    "factory to build EditRadioButton objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    return EditRadioButton(label, parent, "", pos)


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widget_classes['EditRadioButton'] = EditRadioButton
    common.widgets['EditRadioButton'] = builder
    common.widgets_from_xml['EditRadioButton'] = xml_builder

    return common.make_object_button('EditRadioButton', 'radio_button.xpm')
