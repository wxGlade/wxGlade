"""\
wxStaticLine objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common
import compat
import wcodegen
from edit_windows import ManagedBase, EditStylesMixin
from tree import Tree
from widget_properties import *


class EditStaticLine(ManagedBase, EditStylesMixin):

    def __init__(self, name, parent, id, style, sizer, pos,
                 property_window, show=True):
        """\
        Class to handle wxStaticLine objects
        """
        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxStaticLine', parent, id, sizer,
                             pos, property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.set_style(style)
        self.attribute = True

        # initialise properties remaining staff
        access = self.access_functions
        properties = self.properties

        access['style'] = (self.get_string_style, self.set_style)
        access['attribute'] = (self.get_attribute, self.set_attribute)

        properties['style'] = HiddenProperty(self, 'style', style)
        properties['attribute'] = CheckBoxProperty(
            self, 'attribute', label=_('Store as attribute'),
            write_always=True)
        self.removed_p = self.properties['font']

    def create_widget(self):
        self.widget = wx.StaticLine(self.parent.widget, self.id,
                                    style=self.get_int_style())
        wx.EVT_LEFT_DOWN(self.widget, self.on_set_focus)

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self)
        self.sel_marker.Reparent(self.parent.widget)
        del self.properties['font']

    def create_properties(self):
        ManagedBase.create_properties(self)
        if self.removed_p.panel:
            self.removed_p.panel.Hide()
        panel = wx.Panel(self.notebook, -1)
        szr = wx.BoxSizer(wx.VERTICAL)
        self.properties['attribute'].display(panel)
        szr.Add(self.properties['attribute'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def __getitem__(self, key):
        if key != 'font':
            return ManagedBase.__getitem__(self, key)
        return lambda: "", lambda v: None

    def set_attribute(self, v):
        self.attribute = int(v)

    def get_attribute(self):
        return self.attribute

# end of class EditStaticLine


editor_class = EditStaticLine
editor_icon = 'static_line.xpm'
editor_name = 'EditStaticLine'
editor_style = ''

dlg_title = _('wxStaticLine')
box_title = _('Orientation')
choices = 'wxLI_HORIZONTAL|wxLI_VERTICAL'
tmpl_label = 'static_line'


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for editor objects from GUI.
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
