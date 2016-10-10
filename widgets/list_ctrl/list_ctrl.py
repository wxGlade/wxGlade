"""\
wxListCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
from tree import Node
import common


class EditListCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wxListCtrl objects"

    update_widget_style = False

    _PROPERTIES = ["Widget", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, id, sizer, pos, style=wx.LC_REPORT | wx.BORDER_SUNKEN):
        ManagedBase.__init__(self, name, 'wxListCtrl', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)
        if style: self.properties["style"].set(style)

    def create_widget(self):
        self.widget = wx.ListCtrl(self.parent.widget, self.id, style=self.style)
        # add a couple of columns just for a better appearance (for now)
        self.widget.InsertColumn(0, _('List Control:'))
        self.widget.InsertColumn(1, self.name)
        wx.EVT_LIST_COL_CLICK(self.widget, self.widget.GetId(), self.on_set_focus)

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self, sel_marker_parent=self.widget)

    def _set_name(self):
        if not self.widget: return
        col = self.widget.GetColumn(1)
        col.SetText(self.name)
        self.widget.SetColumn(1, col)

    def properties_changed(self, modified):
        ManagedBase.properties_changed(self, modified)
        if not modified or "name" in modified:
            self._set_name()



def builder(parent, sizer, pos, number=[1]):
    "factory function for EditListCtrl objects"
    name = 'list_ctrl_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'list_ctrl_%d' % number[0]
    list_ctrl = EditListCtrl(name, parent, wx.NewId(), sizer, pos)
    node = Node(list_ctrl)
    list_ctrl.node = node
    list_ctrl.properties["proportion"].set(1)
    list_ctrl.properties["flag"].set("wxEXPAND")
    if parent.widget: list_ctrl.create()
    common.app_tree.insert(node, sizer.node, pos-1)
    sizer.set_item(list_ctrl.pos, 1, wx.EXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory function to build EditListCtrl objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    list_ctrl = EditListCtrl(name, parent, wx.NewId(), sizer, pos, style=0)
    sizer.set_item(list_ctrl.pos, proportion=sizeritem.proportion, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(list_ctrl)
    list_ctrl.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return list_ctrl


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widgets['EditListCtrl'] = builder
    common.widgets_from_xml['EditListCtrl'] = xml_builder

    return common.make_object_button('EditListCtrl', 'list_ctrl.xpm')
