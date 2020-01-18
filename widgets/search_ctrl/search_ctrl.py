"""\
wxSearchCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2020 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import common, config, misc
from edit_windows import ManagedBase, EditStylesMixin
import new_properties as np


class EditSearchCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wxSearchCtrl objects"

    WX_CLASS = 'wxSearchCtrl'
    # we want these pages: Common, Layout, Widget, Events, Code
    _PROPERTIES = ["Widget", "value", "descriptive_text", "search_button", "cancel_button", "max_length", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    #recreate_on_style_change = True

    def __init__(self, name, parent, pos):
        # initialize base classes
        ManagedBase.__init__(self, name, 'wxSearchCtrl', parent, pos)
        EditStylesMixin.__init__(self)

        # initialize instance properties
        self.value = np.TextProperty("")
        self.descriptive_text = np.TextPropertyD("Search", default_value="")
        self.search_button = np.CheckBoxProperty(True, default_value=True)
        self.cancel_button = np.CheckBoxProperty(True, default_value=True)
        self.max_length = np.SpinPropertyD(80, val_range=(1,1000), default_value=-1)

    def create_widget(self):
        value = self.value
        self.widget = wx.SearchCtrl(self.parent_window.widget, self.id, value=value, style=self.style)
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



def builder(parent, pos):
    "factory function for EditSearchCtrl objects"
    name = common.root.get_next_name('text_ctrl_%d', parent)
    with parent.frozen():
        editor = EditSearchCtrl(name, parent, pos)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(attrs, parent, pos=None):
    "factory function to build EditSearchCtrl objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    return EditSearchCtrl(name, parent, pos)


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditSearchCtrl'] = EditSearchCtrl
    common.widgets['EditSearchCtrl'] = builder
    common.widgets_from_xml['EditSearchCtrl'] = xml_builder

    return common.make_object_button('EditSearchCtrl', 'search_ctrl.xpm')
