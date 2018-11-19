"""\
wxTreeCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
import common, config


class EditTreeCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wx.TreeCtrl objects"

    update_widget_style = False

    _PROPERTIES = ["Widget", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, sizer, pos, style=wx.TR_HAS_BUTTONS|wx.BORDER_SUNKEN):
        ManagedBase.__init__(self, name, 'wxTreeCtrl', parent, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        if style: self.properties["style"].set(style)
        self._item_with_name = None  # a Tree item for visualization

    def create_widget(self):
        self.widget = wx.TreeCtrl(self.parent_window.widget, self.id, style=self.style) # wx.TR_HAS_BUTTONS|wx.BORDER_SUNKEN)
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



def builder(parent, pos):
    "factory function for EditTreeCtrl objects"
    name = common.root.get_next_name('tree_ctrl_%d', parent)
    with parent.frozen():
        tree_ctrl = EditTreeCtrl(name, parent, pos)
        tree_ctrl.properties["style"].set_to_default()
        tree_ctrl.properties["proportion"].set(1)
        tree_ctrl.properties["flag"].set("wxEXPAND")
        if parent.widget: tree_ctrl.create()
    common.app_tree.insert(tree_ctrl, parent, pos)
    #sizer.set_item(tree_ctrl.pos, 1, wx.EXPAND)


def xml_builder(attrs, parent, sizeritem, pos=None):
    "factory function to build EditTreeCtrl objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    tree_ctrl = EditTreeCtrl(name, parent, pos, style=0)
    common.app_tree.insert(tree_ctrl, parent, pos)
    return tree_ctrl


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widgets['EditTreeCtrl'] = builder
    common.widgets_from_xml['EditTreeCtrl'] = xml_builder

    return common.make_object_button('EditTreeCtrl', 'tree_ctrl.xpm')
