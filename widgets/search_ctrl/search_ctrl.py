"""\
wxSearchCtrl objects

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


class EditSearchCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wxSearchCtrl objects"

    # we want these pages: Common, Layout, Widget, Events, Code
    _PROPERTIES = ["Widget", "value", "descriptive_text", "search_button", "cancel_button", "max_length", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    #recreate_on_style_change = True

    def __init__(self, name, parent, id, sizer, pos):
        # initialize base classes
        ManagedBase.__init__(self, name, 'wxSearchCtrl', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialize instance properties
        self.value = np.TextProperty("")
        self.descriptive_text = np.TextPropertyD("Search", default_value="")
        self.search_button = np.CheckBoxProperty(True, default_value=True)
        self.cancel_button = np.CheckBoxProperty(True, default_value=True)
        self.max_length = np.SpinPropertyD(80, val_range=(1,1000), default_value=-1)

    def create_widget(self):
        value = self.value
        self.widget = wx.SearchCtrl(self.parent.widget, self.id, value=value, style=self.style)
        self.widget.ShowSearchButton(self.search_button)
        self.widget.ShowCancelButton(self.cancel_button)
        if self.properties["descriptive_text"].is_active():
            self.widget.SetDescriptiveText(self.descriptive_text)
        if self.properties["max_length"].is_active():
            self.widget.SetMaxLength(self.max_length)

    def finish_widget_creation(self, sel_marker_parent=None, re_add=True):
        ManagedBase.finish_widget_creation(self, sel_marker_parent, re_add)
        #self.widget.Bind(wx.EVT_SET_FOCUS, self.on_set_focus)
        self.widget.Bind(wx.EVT_CHILD_FOCUS, self.on_set_focus)
        #self.widget.Bind(wx.EVT_TEXT, self.on_set_focus)

    def properties_changed(self, modified):
        if "value" in modified and self.widget:
            self.widget.SetValue(self.value)
        if "search_button" in modified and self.widget:
            self.widget.ShowSearchButton(self.search_button)
        if "cancel_button" in modified and self.widget:
            self.widget.ShowCancelButton(self.cancel_button)
        if "descriptive_text" in modified and self.widget:
            self.widget.SetDescriptiveText(self.descriptive_text)
        if "max_length" in modified and self.widget:
            self.widget.SetMaxLength(self.max_length)
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)



def builder(parent, sizer, pos, number=[1]):
    "factory function for EditSearchCtrl objects"
    name = 'text_ctrl_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'text_ctrl_%d' % number[0]
    with parent.frozen():
        text = EditSearchCtrl(name, parent, wx.NewId(), sizer, pos)
        text.properties["style"].set_to_default()
        text.check_defaults()
        node = Node(text)
        text.node = node
        if parent.widget: text.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory function to build EditSearchCtrl objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    text = EditSearchCtrl(name, parent, wx.NewId(), sizer, pos)
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
    common.widgets['EditSearchCtrl'] = builder
    common.widgets_from_xml['EditSearchCtrl'] = xml_builder

    return common.make_object_button('EditSearchCtrl', 'search_ctrl.xpm')
