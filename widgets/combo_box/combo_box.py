"""\
wxComboBox objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import common, config, compat
from edit_windows import ManagedBase, EditStylesMixin
import new_properties as np
from ChoicesProperty import *


class EditComboBox(ManagedBase, EditStylesMixin):
    "Class to handle wxComboBox objects"

    WX_CLASS = "wxComboBox"
    _PROPERTIES = ["Widget", "style", "selection", "choices"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    update_widget_style = False

    def __init__(self, name, parent, choices, pos):
        ManagedBase.__init__(self, name, 'wxComboBox', parent, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.selection = np.SpinProperty(-1, val_range=(-1,len(choices)-1), immediate=True )
        self.choices = ChoicesProperty( choices, [(_('Label'), np.GridProperty.STRING)] )

    def create_widget(self):
        choices = [c[0] for c in self.choices]
        selection = self.selection
        self.widget = wx.ComboBox(self.parent_window.widget, self.id, choices=choices)
        self.widget.Bind(wx.EVT_SET_FOCUS, self.on_set_focus)
        self.widget.SetSelection(selection)

    def get_property_handler(self, prop_name):
        if prop_name == 'choices':
            return ChoicesHandler(self)
        return ManagedBase.get_property_handler(self, prop_name)

    def properties_changed(self, modified):  # the same code as for EditChoice and EditCheckListBox
        # self.selection needs to be in range (-1,len(self.choices)-1)
        choices = self.choices
        max_selection = len(choices)-1
        set_selection = False
        if not modified or "choices" in modified:
            # adjust range of selection
            self.properties['selection'].set_range(min(-1,max_selection), max_selection)
            set_selection = True
            if self.widget:
                # update widget
                self.widget.Clear()
                for c in choices: self.widget.Append(c[0])
                if not self.properties['size'].is_active():
                    self.sizer.set_item_best_size(self, size=self.widget.GetBestSize())

        if not modified or "selection" in modified or set_selection:
            set_selection = True
            if self.selection>max_selection:
                self.properties['selection'].set(max_selection)
        if self.widget and set_selection and self.widget.GetSelection()!=self.selection:
            self.widget.SetSelection(self.selection)

        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)


def builder(parent, pos):
    "factory function for EditComboBox objects"
    name = common.root.get_next_name('combo_box_%d', parent)
    with parent.frozen():
        editor = EditComboBox(name, parent, [], pos)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(attrs, parent, pos=None):
    "factory to build EditComboBox objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    return EditComboBox(name, parent, [], pos)


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditComboBox'] = builder
    common.widgets_from_xml['EditComboBox'] = xml_builder

    return common.make_object_button('EditComboBox', 'combo_box.xpm')
