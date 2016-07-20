"""\
wxCalendarCtrl objects

@copyright: 2002-2007 Alberto Griggio
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
from wx.calendar import *


class EditCalendarCtrl(ManagedBase, EditStylesMixin):
    """\
    Class to handle wxCalendarCtrl objects
    """

    def __init__(self, name, parent, id, sizer, pos, property_window,
                 show=True):
        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxCalendarCtrl', parent, id, sizer,
                             pos, property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.default = False
        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wx.ALL

        # initialise properties remaining staff
        self.access_functions['default'] = (self.get_default, self.set_default)
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['default'] = CheckBoxProperty(
            self, 'default', None, label=_("default"))
        self.properties['style'] = CheckListProperty(
            self, 'style', self.widget_writer)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.Panel(self.notebook, -1)
        #self.properties['label'].display(panel)
        self.properties['default'].display(panel)
        self.properties['style'].display(panel)
        szr = wx.BoxSizer(wx.VERTICAL)
        szr.Add(self.properties['default'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['style'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(1)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def create_widget(self):
        try:
            # TODO add all the other parameters for the CalendarCtrl
            # especially style=self.style and the initial date
            self.widget = CalendarCtrl(self.parent.widget, self.id,
                                       style=self.get_int_style())
        except AttributeError:
            self.widget = CalendarCtrl(self.parent.widget, self.id)

    def get_default(self):
        return self.default

    def set_default(self, value):
        self.default = bool(int(value))

# end of class EditCalendarCtrl


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditCalendarCtrl objects.
    """
    label = 'calendar_ctrl_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'calendar_ctrl_%d' % number[0]
    calendar_ctrl = EditCalendarCtrl(label, parent, wx.NewId(), sizer,
                        pos, common.property_panel)
    node = Tree.Node(calendar_ctrl)
    calendar_ctrl.node = node
    calendar_ctrl.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditCalendarCtrl objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    calendar_ctrl = EditCalendarCtrl(label, parent, wx.NewId(), sizer, pos,
                                     common.property_panel, show=False)
    sizer.set_item(calendar_ctrl.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border)
    node = Tree.Node(calendar_ctrl)
    calendar_ctrl.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return calendar_ctrl


def initialize():
    """\
    initialization function for the module.
    @rtype: wxBitmapButton
    @return: an icon to be added to the main palette. 
    """
    common.widgets['EditCalendarCtrl'] = builder
    common.widgets_from_xml['EditCalendarCtrl'] = xml_builder

    return common.make_object_button('EditCalendarCtrl', 'calendar_ctrl.xpm')
