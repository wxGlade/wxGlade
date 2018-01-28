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
from tree import Node


class EditTextCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wxTextCtrl objects"

    # we want these pages: Common, Layout, Widget, Events, Code
    _PROPERTIES = ["Widget", "value", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    recreate_on_style_change = True

    def __init__(self, name, parent, id, sizer, pos):
        # initialize base classes
        ManagedBase.__init__(self, name, 'wxTextCtrl', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialize instance properties
        self.value = np.TextProperty("", multiline=True)

        #self.properties["style"].set( self.get_int_style() ) # XXX check whether this makes sense for any control

    def create_widget(self):
        value = self.value
        #if self.style & wx.TE_MULTILINE:
        #    value = value.replace('\\n', '\n') # XXX is this correct? is self.value already with newlines?
        self.widget = wx.TextCtrl(self.parent.widget, self.id, value=value, style=self.style)

    def properties_changed(self, modified):
        if "value" in modified and self.widget:
            self.widget.SetValue(self.value)
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)



def builder(parent, sizer, pos, number=[1]):
    "factory function for EditTextCtrl objects"
    name = 'text_ctrl_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'text_ctrl_%d' % number[0]
    with parent.frozen():
        text = EditTextCtrl(name, parent, wx.NewId(), sizer, pos)
        text.properties["style"].set_to_default()
        text.check_defaults()
        node = Node(text)
        text.node = node
        if parent.widget: text.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory function to build EditTextCtrl objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    text = EditTextCtrl(name, parent, wx.NewId(), sizer, pos)
    #sizer.set_item(text.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(text)
    text.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return text


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditTextCtrl'] = builder
    common.widgets_from_xml['EditTextCtrl'] = xml_builder

    return common.make_object_button('EditTextCtrl', 'text_ctrl.xpm')
