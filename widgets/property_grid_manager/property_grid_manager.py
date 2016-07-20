"""\
wxPropertyGridManager objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@copyright: 2015 Franco Bugnano
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from wx.propgrid import *
import common
import compat
from edit_windows import ManagedBase, EditStylesMixin
from tree import Tree
from widget_properties import *


class EditPropertyGridManager(ManagedBase, EditStylesMixin):

    def __init__(self, name, parent, id, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxPropertyGridManager objects
        """

        ManagedBase.__init__(self, name, 'wxPropertyGridManager', parent, id, sizer, pos,
                             property_window, show=show)
        EditStylesMixin.__init__(self)

        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['style'] = CheckListProperty(
            self, 'style', self.widget_writer)

    def create_properties(self):
        ManagedBase.create_properties(self)
        ManagedBase.create_properties(self)
        panel = wx.ScrolledWindow(self.notebook, -1, style=wx.TAB_TRAVERSAL)
        szr = wx.BoxSizer(wx.VERTICAL)
        self.properties['style'].display(panel)
        szr.Add(self.properties['style'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        w, h = panel.GetSize()
        from math import ceil
        panel.SetScrollbars(5, 5, int(ceil(w/5.0)), int(ceil(h/5.0)))
        self.notebook.AddPage(panel, 'Widget')

    def create_widget(self):
        self.widget = PropertyGridManager(self.parent.widget, self.id, (200, 200))

        # following two events are to permit select grid from designer frame
        self.widget.Bind(EVT_PG_SELECTED, self.on_set_focus)
        # these are to show the popup menu on right click
        self.widget.Bind(EVT_PG_RIGHT_CLICK, self.popup_menu)

    def get_property_handler(self, name):
        return ManagedBase.get_property_handler(self, name)

# end of class EditPropertyGridManager


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditPropertyGridManager objects.
    """
    label = 'property_grid_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'property_grid_%d' % number[0]
    property_grid_manager = EditPropertyGridManager(label, parent, wx.NewId(), sizer, pos,
                    common.property_panel)
    # A grid should be wx.EXPANDed and 'option' should be 1,
    # or you can't see it.
    property_grid_manager.set_option(1)
    property_grid_manager.esm_border.set_style("wxEXPAND")
    node = Tree.Node(property_grid_manager)
    property_grid_manager.node = node
    property_grid_manager.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditPropertyGridManager objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    property_grid_manager = EditPropertyGridManager(label, parent, wx.NewId(), sizer,
                    pos, common.property_panel, show=False)
    sizer.set_item(property_grid_manager.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border)
    node = Tree.Node(property_grid_manager)
    property_grid_manager.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return property_grid_manager


def initialize():
    """\
    initialization function for the module: returns a wx.BitmapButton to be
    added to the main palette.
    """
    common.widgets['EditPropertyGridManager'] = builder
    common.widgets_from_xml['EditPropertyGridManager'] = xml_builder

    return common.make_object_button('EditPropertyGridManager', 'grid.xpm')

