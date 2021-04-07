"""\
wxTreeCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
import common, config


class EditTreeCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wx.TreeCtrl objects"

    update_widget_style = False

    WX_CLASS = 'wxTreeCtrl'
    _PROPERTIES = ["Widget", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, index, style=wx.TR_HAS_BUTTONS|wx.BORDER_SUNKEN):
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self, style)
        self._item_with_name = None  # a Tree item for visualization

    def create_widget(self):
        self.widget = wx.TreeCtrl(self.parent_window.widget, wx.ID_ANY, style=self.style) # wx.TR_HAS_BUTTONS|wx.BORDER_SUNKEN)
        # add a couple of items just for a better appearance
        root = self.widget.AddRoot(_(' Tree Control:'))
        self._item_with_name = self.widget.AppendItem(root, ' ' + self.name)
        self.widget.AppendItem(self._item_with_name, _(' on wxGlade version %s') % config.version )
        self.widget.Expand(root)
        self.widget.Expand(self._item_with_name)

    def finish_widget_creation(self, level):
        ManagedBase.finish_widget_creation(self, level, sel_marker_parent=self.widget)

    def _set_name(self):
        if not self.widget or not self._item_with_name: return
        self.widget.SetItemText(self._item_with_name, ' ' + self.name)

    def _properties_changed(self, modified, actions):
        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)
        if not modified or "name" in modified:
            self._set_name()


def builder(parent, index):
    "factory function for EditTreeCtrl objects"
    name = parent.toplevel_parent.get_next_contained_name('tree_ctrl_%d')
    with parent.frozen():
        editor = EditTreeCtrl(name, parent, index)
        editor.properties["proportion"].set(1)
        editor.properties["flag"].set("wxEXPAND")
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory function to build EditTreeCtrl objects from a XML file"
    return EditTreeCtrl(name, parent, index, style=0)


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widget_classes['EditTreeCtrl'] = EditTreeCtrl
    common.widgets['EditTreeCtrl'] = builder
    common.widgets_from_xml['EditTreeCtrl'] = xml_builder

    return common.make_object_button('EditTreeCtrl', 'tree_ctrl.png')
