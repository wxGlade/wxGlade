"""\
wxTreeCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
from tree import Node
import common, config


class EditTreeCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wx.TreeCtrl objects"

    update_widget_style = False

    _PROPERTIES = ["Widget", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, id, sizer, pos, style=wx.TR_HAS_BUTTONS|wx.BORDER_SUNKEN):
        ManagedBase.__init__(self, name, 'wxTreeCtrl', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        if style: self.properties["style"].set(style)
        self._item_with_name = None  # a Tree item for visualization

    def create_widget(self):
        self.widget = wx.TreeCtrl(self.parent.widget, self.id, style=self.style) # wx.TR_HAS_BUTTONS|wx.BORDER_SUNKEN)
        # add a couple of items just for a better appearance
        root = self.widget.AddRoot(_(' Tree Control:'))
        self._item_with_name = self.widget.AppendItem(root, ' ' + self.name)
        self.widget.AppendItem(self._item_with_name, _(' on wxGlade version %s') % config.version )
        self.widget.Expand(root)
        self.widget.Expand(self._item_with_name)

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self, sel_marker_parent=self.widget)

    def _set_name(self):
        if not self.widget or not self._item_with_name: return
        self.widget.SetItemText(self._item_with_name, ' ' + self.name)

    def properties_changed(self, modified):
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)
        if not modified or "name" in modified:
            self._set_name()



def builder(parent, sizer, pos, number=[1]):
    "factory function for EditTreeCtrl objects"
    name = 'tree_ctrl_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'tree_ctrl_%d' % number[0]
    with parent.frozen():
        tree_ctrl = EditTreeCtrl(name, parent, wx.NewId(), sizer, pos)
        tree_ctrl.properties["style"].set_to_default()
        node = Node(tree_ctrl)
        tree_ctrl.node = node
        tree_ctrl.properties["proportion"].set(1)
        tree_ctrl.properties["flag"].set("wxEXPAND")
        if parent.widget: tree_ctrl.create()
    common.app_tree.insert(node, sizer.node, pos-1)
    #sizer.set_item(tree_ctrl.pos, 1, wx.EXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory function to build EditTreeCtrl objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    tree_ctrl = EditTreeCtrl(name, parent, wx.NewId(), sizer, pos, style=0)
    #sizer.set_item(tree_ctrl.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(tree_ctrl)
    tree_ctrl.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return tree_ctrl


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widgets['EditTreeCtrl'] = builder
    common.widgets_from_xml['EditTreeCtrl'] = xml_builder

    return common.make_object_button('EditTreeCtrl', 'tree_ctrl.xpm')
