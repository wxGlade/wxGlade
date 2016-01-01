"""\
wxTreeCtrl objects

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


class EditTreeCtrl(ManagedBase, EditStylesMixin):
    """\
    Class to handle wx.TreeCtrl objects
    """

    update_widget_style = False

    def __init__(self, name, parent, id, sizer, pos, property_window,
                 show=True, style=wx.TR_HAS_BUTTONS|wx.BORDER_SUNKEN):

        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxTreeCtrl', parent, id, sizer, pos,
                             property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.style = style
        self._item_with_name = None

        # initialise properties remaining staff
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['style'] = CheckListProperty(
            self, 'style', self.widget_writer)

    def create_widget(self):
        self.widget = wx.TreeCtrl(self.parent.widget, self.id,
                                 style=wx.TR_HAS_BUTTONS|wx.BORDER_SUNKEN)
        # add a couple of items just for a better appearance
        root = self.widget.AddRoot(_(' Tree Control:'))
        self._item_with_name = self.widget.AppendItem(root, ' ' + self.name)
        self.widget.AppendItem(self._item_with_name,
                               _(' on wxGlade %s') % config.version)
        self.widget.Expand(root)
        self.widget.Expand(self._item_with_name)

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self, sel_marker_parent=self.widget)

    def set_name(self, name):
        ManagedBase.set_name(self, name)
        if self.widget and self._item_with_name:
            self.widget.SetItemText(self._item_with_name, ' ' + self.name)

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
        self.notebook.AddPage(panel, _('Widget'))
        self.property_window.Layout()
        import math
        panel.SetScrollbars(1, 5, 1, int(math.ceil(h/5.0)))

# end of class EditTreeCtrl


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditTreeCtrl objects.
    """
    name = 'tree_ctrl_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'tree_ctrl_%d' % number[0]
    tree_ctrl = EditTreeCtrl(name, parent, wx.NewId(), sizer, pos,
                             common.property_panel)
    node = Tree.Node(tree_ctrl)
    tree_ctrl.node = node
    tree_ctrl.set_option(1)
    tree_ctrl.set_style("wxEXPAND")
    tree_ctrl.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)
    sizer.set_item(tree_ctrl.pos, 1, wx.EXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory function to build EditTreeCtrl objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    tree_ctrl = EditTreeCtrl(name, parent, wx.NewId(), sizer, pos,
                             common.property_panel, style=0)
    sizer.set_item(tree_ctrl.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border)
    node = Tree.Node(tree_ctrl)
    tree_ctrl.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return tree_ctrl


def initialize():
    """\
    initialization function for the module: returns a wx.BitmapButton to be
    added to the main palette.
    """
    common.widgets['EditTreeCtrl'] = builder
    common.widgets_from_xml['EditTreeCtrl'] = xml_builder

    return common.make_object_button('EditTreeCtrl', 'tree_ctrl.xpm')
