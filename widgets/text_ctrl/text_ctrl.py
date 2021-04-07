"""\
wxTextCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import common, config, misc
from edit_windows import ManagedBase, EditStylesMixin
import new_properties as np


class EditTextCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wxTextCtrl objects"

    WX_CLASS = 'wxTextCtrl'
    # we want these pages: Common, Layout, Widget, Events, Code
    _PROPERTIES = ["Widget", "value", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    recreate_on_style_change = True

    def __init__(self, name, parent, index):
        # initialize base classes
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self)

        # initialize instance properties
        self.value = np.TextProperty("", multiline="grow")

        #self.properties["style"].set( self.get_int_style() ) # XXX check whether this makes sense for any control

    def create_widget(self):
        value = self.value
        #if self.style & wx.TE_MULTILINE:
        #    value = value.replace('\\n', '\n') # XXX is this correct? is self.value already with newlines?
        self.widget = wx.TextCtrl(self.parent_window.widget, wx.ID_ANY, value=value, style=self.style)

    def _properties_changed(self, modified, actions):
        if "value" in modified and self.widget:
            self.widget.SetValue(self.value)
        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)



def builder(parent, index):
    "factory function for EditTextCtrl objects"
    name = parent.toplevel_parent.get_next_contained_name('text_ctrl_%d')
    with parent.frozen():
        editor = EditTextCtrl(name, parent, index)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory function to build EditTextCtrl objects from a XML file"
    return EditTextCtrl(name, parent, index)


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditTextCtrl'] = EditTextCtrl
    common.widgets['EditTextCtrl'] = builder
    common.widgets_from_xml['EditTextCtrl'] = xml_builder

    return common.make_object_button('EditTextCtrl', 'text_ctrl.png')
