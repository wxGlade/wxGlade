"""\
wxSpinButton objects

based on wxGlade/widgets/spin_ctrl/

@copyright: 2004 D.H. aka crazyinsomniac at users.sourceforge.net
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
from tree import Tree
import common
import compat
import config
from widget_properties import *


class EditSpinButton(ManagedBase, EditStylesMixin):
    """\
    Class to handle wxSpinButton objects
    """

    def __init__(self, name, parent, id, sizer, pos, property_window,
                 show=True):

        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxSpinButton', parent, id, sizer,
                             pos, property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.value = 0
        self.range = (0, 100)  # Default values in wxSpinButton constructor.
        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wx.ALL

        # initialise properties remaining staff
        prop = self.properties
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.access_functions['value'] = (self.get_value, self.set_value)
        self.access_functions['range'] = (self.get_range, self.set_range)
        prop['style'] = CheckListProperty(
            self, 'style', self.widget_writer)
        prop['range'] = TextProperty(
            self, 'range', None, can_disable=True, label=_("range"))
        prop['value'] = SpinProperty(
            self, 'value', None, can_disable=True, label=_("value"))

    def create_widget(self):
        try:
            self.widget = wx.SpinButton(self.parent.widget, self.id,
                                        style=self.get_int_style())
        except AttributeError:
            self.widget = wx.SpinButton(self.parent.widget, self.id)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.ScrolledWindow(self.notebook, -1, style=wx.TAB_TRAVERSAL)
        szr = wx.BoxSizer(wx.VERTICAL)
        prop = self.properties
        prop['range'].display(panel)
        prop['value'].display(panel)
        prop['style'].display(panel)
        szr.Add(prop['range'].panel, 0, wx.EXPAND)
        szr.Add(prop['value'].panel, 0, wx.EXPAND)
        szr.Add(prop['style'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def get_range(self):
        # we cannot return self.range since this would become a "(0, 100)"
        # string, and we don't want the parents
        return "%s, %s" % self.range

    def set_range(self, val):
        try:
            min_v, max_v = map(int, val.split(','))
        except:
            self.properties['range'].set_value(self.get_range())
        else:
            self.range = (min_v, max_v)
            self.properties['value'].set_range(min_v, max_v)
            if self.widget:
                self.widget.SetRange(min_v, max_v)

    def get_value(self):
        return self.value

    def set_value(self, value):
        value = int(value)
        if self.value != value:
            self.value = value
            if self.widget: self.widget.SetValue(self.value)

# end of class EditSpinButton


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditSpinButton objects.
    """
    name = 'spin_button_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'spin_button_%d' % number[0]
    text = EditSpinButton(name, parent, wx.NewId(), sizer, pos,
                        common.property_panel)
    node = Tree.Node(text)
    text.node = node
    text.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory function to build EditSpinButton objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    text = EditSpinButton(name, parent, wx.NewId(), sizer, pos,
                        common.property_panel)
    sizer.set_item(text.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border)
    node = Tree.Node(text)
    text.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return text


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditSpinButton'] = builder
    common.widgets_from_xml['EditSpinButton'] = xml_builder

    return common.make_object_button('EditSpinButton', 'spinbtn.xpm')
