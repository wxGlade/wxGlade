"""\
wxPropertyGridManager objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@copyright: 2015 Franco Bugnano
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from wx.propgrid import *
import common
from edit_windows import ManagedBase, EditStylesMixin
from tree import Node


class EditPropertyGridManager(ManagedBase, EditStylesMixin):
    "Class to handle wxPropertyGridManager objects"
    _PROPERTIES = ["Widget", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, id, sizer, pos):
        ManagedBase.__init__(self, name, 'wxPropertyGridManager', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

    def create_widget(self):
        self.widget = PropertyGridManager(self.parent.widget, self.id, (200, 200))

        # following two events are to permit select grid from designer frame
        self.widget.Bind(EVT_PG_SELECTED, self.on_set_focus)
        # these are to show the popup menu on right click
        self.widget.Bind(EVT_PG_RIGHT_CLICK, self.popup_menu)

    def get_property_handler(self, name):
        return ManagedBase.get_property_handler(self, name)



def builder(parent, sizer, pos, number=[1]):
    "factory function for EditPropertyGridManager objects"
    label = 'property_grid_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'property_grid_%d' % number[0]
    property_grid_manager = EditPropertyGridManager(label, parent, wx.NewId(), sizer, pos)
    # A grid should be wx.EXPANDed and 'option' should be 1, or you can't see it.
    property_grid_manager.set_option(1)
    property_grid_manager.esm_border.set_style("wxEXPAND")
    node = Node(property_grid_manager)
    property_grid_manager.node = node
    if parent.widget: property_grid_manager.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditPropertyGridManager objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    property_grid_manager = EditPropertyGridManager( label, parent, wx.NewId(), sizer, pos )
    sizer.set_item( property_grid_manager.pos, proportion=sizeritem.proportion, flag=sizeritem.flag,
                    border=sizeritem.border)
    node = Node(property_grid_manager)
    property_grid_manager.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return property_grid_manager


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widgets['EditPropertyGridManager'] = builder
    common.widgets_from_xml['EditPropertyGridManager'] = xml_builder

    return common.make_object_button('EditPropertyGridManager', 'grid.xpm')

