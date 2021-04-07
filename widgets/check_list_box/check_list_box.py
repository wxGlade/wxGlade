"""\
wxCheckListBox objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@copyright: 2015 Franco Bugnano
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common
from edit_windows import ManagedBase, EditStylesMixin
import new_properties as np
from ChoicesProperty import *


class EditCheckListBox(ManagedBase, EditStylesMixin):
    "Class to handle wxCheckListBox objects"
    # XXX maybe inherit from EditChoice
    WX_CLASS = "wxCheckListBox"
    _PROPERTIES = ["Widget", "style", "selection", "choices"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    recreate_on_style_change = True  # otherwise an assertion is triggered

    def __init__(self, name, parent, index, choices):
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.selection = np.SpinProperty(-1, val_range=len(choices)-1, default_value=-1, immediate=True )
        self.choices   = ChoicesProperty( choices, [(_('Label'), np.GridProperty.STRING)] )

    def create_widget(self):
        choices = [c[0] for c in self.choices]
        self.widget = wx.CheckListBox(self.parent_window.widget, wx.ID_ANY, choices=choices, style=self.style)
        self.widget.SetSelection(self.selection)
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)

    def get_property_handler(self, prop_name):
        if prop_name == 'choices':
            return ChoicesHandler(self)
        return ManagedBase.get_property_handler(self, prop_name)

    def _properties_changed(self, modified, actions):
        # self.selection needs to be in range (-1,len(self.choices))
        choices = self.choices
        max_selection = len(choices)-1
        set_selection = False
        if not modified or "choices" in modified:
            # adjust range of selection
            self.properties['selection'].set_range(-1, max_selection)
            if self.selection>max_selection:
                set_selection = True
            if self.widget:
                # update widget
                self.widget.Clear()
                for c in choices: self.widget.Append(c[0])
                actions.add("layout")

        if not modified or "selection" in modified or set_selection:
            if self.selection>max_selection:
                self.properties['selection'].set(max_selection)
            set_selection = True

        if self.widget and set_selection:
            self.widget.SetSelection(self.selection)  # -1 is identical to wx.NOT_FOUND

        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)



def builder(parent, index):
    "factory function for EditCheckListBox objects"
    name = parent.toplevel_parent.get_next_contained_name('check_list_box_%d')
    with parent.frozen():
        editor = EditCheckListBox(name, parent, index, [[u'choice 1']])
        editor.properties["style"].set_to_default()
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory to build EditCheckListBox objects from a XML file"
    return EditCheckListBox(name, parent, index, [])


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditCheckListBox'] = EditCheckListBox
    common.widgets['EditCheckListBox'] = builder
    common.widgets_from_xml['EditCheckListBox'] = xml_builder

    return common.make_object_button('EditCheckListBox', 'list_box.png')
