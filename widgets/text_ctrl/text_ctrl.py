"""\
wxTextCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2018 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import common, config, misc
from edit_windows import ManagedBase, EditStylesMixin
import new_properties as np


class EditTextCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wxTextCtrl objects"

    # we want these pages: Common, Layout, Widget, Events, Code
    _PROPERTIES = ["Widget", "value", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    recreate_on_style_change = True

    def __init__(self, name, parent, pos):
        # initialize base classes
        ManagedBase.__init__(self, name, 'wxTextCtrl', parent, pos)
        EditStylesMixin.__init__(self)

        # initialize instance properties
        self.value = np.TextProperty("", multiline=True)

        #self.properties["style"].set( self.get_int_style() ) # XXX check whether this makes sense for any control

    def create_widget(self):
        value = self.value
        #if self.style & wx.TE_MULTILINE:
        #    value = value.replace('\\n', '\n') # XXX is this correct? is self.value already with newlines?
        self.widget = wx.TextCtrl(self.parent_window.widget, self.id, value=value, style=self.style)

    def properties_changed(self, modified):
        if "value" in modified and self.widget:
            self.widget.SetValue(self.value)
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)



def builder(parent, pos):
    "factory function for EditTextCtrl objects"
    name = common.root.get_next_name('text_ctrl_%d', parent)
    with parent.frozen():
        text = EditTextCtrl(name, parent, pos)
        text.properties["style"].set_to_default()
        text.check_defaults()
        if parent.widget: text.create()
    common.app_tree.insert(text, parent, pos)


def xml_builder(attrs, parent, sizeritem, pos=None):
    "factory function to build EditTextCtrl objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    text = EditTextCtrl(name, parent, pos)
    common.app_tree.insert(text, parent, pos)
    return text


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditTextCtrl'] = builder
    common.widgets_from_xml['EditTextCtrl'] = xml_builder

    return common.make_object_button('EditTextCtrl', 'text_ctrl.xpm')
