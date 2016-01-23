"""\
wxGauge objects

@copyright: 2002-2007 Alberto Griggio
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common
import compat
import wcodegen
from edit_windows import ManagedBase, EditStylesMixin
from tree import Tree
from widget_properties import *


class EditGauge(ManagedBase, EditStylesMixin):
    """\
    Class to handle wxGauge objects
    """

    def __init__(self, name, parent, id, style, sizer, pos,
                 property_window, show=True):

        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxGauge', parent, id, sizer,
                             pos, property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.set_style(style)
        self.range = 10

        # initialise properties remaining staff
        prop = self.properties
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.access_functions['range'] = (self.get_range, self.set_range)
        prop['style'] = CheckListProperty(
            self, 'style', self.widget_writer)
        prop['range'] = SpinProperty(self, 'range', None, label=_("range"))

    def create_widget(self):
        self.widget = wx.Gauge(self.parent.widget, self.id, self.range,
                               style=self.get_int_style())

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.ScrolledWindow(self.notebook, -1, style=wx.TAB_TRAVERSAL)
        prop = self.properties
        szr = wx.BoxSizer(wx.VERTICAL)
        prop['range'].display(panel)
        prop['style'].display(panel)
        szr.Add(prop['range'].panel, 0, wx.EXPAND)
        szr.Add(prop['style'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def get_range(self):
        return self.range

    def set_range(self, val):
        self.range = int(val)
        self.properties['range'].set_value(self.range)
        if self.widget: self.widget.SetRange(self.range)

# end of class EditGauge

editor_class = EditGauge
editor_icon = 'gauge.xpm'
editor_name = 'EditGauge'
editor_style = ''

dlg_title = _('wxGauge')
box_title = _('Orientation')
choices = 'wxGA_HORIZONTAL|wxGA_VERTICAL'
tmpl_label = 'gauge'


def builder(parent, sizer, pos, number=[1]):
    """\
    Factory function for editor objects from GUI.
    """
    dialog = wcodegen.WidgetStyleSelectionDialog(
            dlg_title, box_title, choices)
    res = dialog.ShowModal()
    style = dialog.get_selection()
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    label = '%s_%d' % (tmpl_label, number[0])
    while common.app_tree.has_name(label):
        number[0] += 1
        label = '%s_%d' % (tmpl_label, number[0])
    widget = editor_class(label, parent, wx.ID_ANY, style, sizer, pos,
                          common.property_panel)
    node = Tree.Node(widget)
    widget.node = node
    widget.set_style("wxEXPAND")
    widget.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    Factory to build editor objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    widget = editor_class(name, parent, wx.ID_ANY, editor_style, sizer,
                          pos, common.property_panel)
    sizer.set_item(widget.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border)
    node = Tree.Node(widget)
    widget.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return widget


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets[editor_name] = builder
    common.widgets_from_xml[editor_name] = xml_builder
    return common.make_object_button(editor_name, editor_icon)
