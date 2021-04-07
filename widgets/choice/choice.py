"""\
wxChoice objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, config
from edit_windows import ManagedBase
import new_properties as np

from ChoicesProperty import *



class EditChoice(ManagedBase):
    "Class to handle wxChoice objects"
    WX_CLASS = "wxChoice"
    _PROPERTIES = ["Widget", "selection", "choices"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, index, choices):
        ManagedBase.__init__(self, name, parent, index)

        # initialise instance properties
        self.selection = np.SpinProperty(0, val_range=(-1,len(choices)-1), immediate=True )
        self.choices = ChoicesProperty( choices, [(_('Label'), np.GridProperty.STRING)] )

    def create_widget(self):
        choices = [c[0] for c in self.choices]
        self.widget = wx.Choice(self.parent_window.widget, wx.ID_ANY, choices=choices)
        self.widget.SetSelection(self.selection)
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)

    def get_property_handler(self, prop_name):
        if prop_name == 'choices':
            return ChoicesHandler(self)
        return ManagedBase.get_property_handler(self, prop_name)

    def _properties_changed(self, modified, actions):
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
                actions.add("layout")

        if not modified or "selection" in modified or set_selection:
            set_selection = True
            if self.selection>max_selection:
                if common.history: common.history.monitor_property( self.properties['selection'] )
                self.properties['selection'].set(max_selection)
        if self.widget and set_selection and self.widget.GetSelection()!=self.selection:
            self.widget.SetSelection(self.selection)

        ManagedBase._properties_changed(self, modified, actions)


def builder(parent, index):
    "factory function for EditChoice objects"
    name = parent.toplevel_parent.get_next_contained_name('choice_%d')
    with parent.frozen():
        editor = EditChoice(name, parent, index, [(u'choice 1',)])
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory to build EditChoice objects from a XML file"
    return EditChoice(name, parent, index, [])


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditChoice'] = EditChoice
    common.widgets['EditChoice'] = builder
    common.widgets_from_xml['EditChoice'] = xml_builder

    return common.make_object_button('EditChoice', 'choice.png')
