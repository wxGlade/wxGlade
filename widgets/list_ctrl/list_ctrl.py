"""\
wxListCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import math
import wx
from edit_windows import ManagedBase, EditStylesMixin
from tree import Tree
import common
import compat
from widget_properties import *


class EditListCtrl(ManagedBase, EditStylesMixin):
    """\
    Class to handle wxListCtrl objects
    """

    update_widget_style = False

    def __init__(self, name, parent, id, sizer, pos, property_window,
                 show=True, style=wx.LC_REPORT | wx.BORDER_SUNKEN):

        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxListCtrl', parent, id, sizer, pos,
                             property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.style = style

        # initialise properties remaining staff
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['style'] = CheckListProperty(
            self, 'style', self.widget_writer)

    def create_widget(self):
        self.widget = wx.ListCtrl(self.parent.widget, self.id,
                                  style=wx.LC_REPORT | wx.BORDER_SUNKEN)
        # add a couple of columns just for a better appearance (for now)
        self.widget.InsertColumn(0, _('List Control:'))
        self.widget.InsertColumn(1, self.name)
        wx.EVT_LIST_COL_CLICK(self.widget, self.widget.GetId(),
                              self.on_set_focus)

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self, sel_marker_parent=self.widget)

    def set_name(self, name):
        ManagedBase.set_name(self, name)
        if self.widget:
            col = self.widget.GetColumn(1)
            col.SetText(self.name)
            self.widget.SetColumn(1, col)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.ScrolledWindow(self.notebook, -1, style=wx.TAB_TRAVERSAL)
        prop = self.properties
        prop['style'].display(panel)
        szr = wx.BoxSizer(wx.VERTICAL)
        szr.Add(prop['style'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        w, h = panel.GetClientSize()
        self.notebook.AddPage(panel, 'Widget')
        self.property_window.Layout()
        panel.SetScrollbars(1, 5, 1, int(math.ceil(h / 5.0)))

# end of class EditListCtrl


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditListCtrl objects.
    """
    name = 'list_ctrl_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'list_ctrl_%d' % number[0]
    list_ctrl = EditListCtrl(name, parent, wx.NewId(), sizer, pos,
                             common.property_panel)
    node = Tree.Node(list_ctrl)
    list_ctrl.node = node
    list_ctrl.set_option(1)
    list_ctrl.set_style("wxEXPAND")
    list_ctrl.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)
    sizer.set_item(list_ctrl.pos, 1, wx.EXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory function to build EditListCtrl objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    list_ctrl = EditListCtrl(name, parent, wx.NewId(), sizer, pos,
                             common.property_panel, style=0)
    sizer.set_item(list_ctrl.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border)
    node = Tree.Node(list_ctrl)
    list_ctrl.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return list_ctrl


def initialize():
    """\
    initialization function for the module: returns a wx.BitmapButton to be
    added to the main palette.
    """
    common.widgets['EditListCtrl'] = builder
    common.widgets_from_xml['EditListCtrl'] = xml_builder

    return common.make_object_button('EditListCtrl', 'list_ctrl.xpm')
