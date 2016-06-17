"""\
wxListBox objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common
import compat
import misc
from edit_windows import ManagedBase, EditStylesMixin
from tree import Tree
from widget_properties import *
from ChoicesProperty import *


class EditListBox(ManagedBase, EditStylesMixin):
    """\
    Class to handle wxListBox objects
    """

    def __init__(self, name, parent, id, choices, sizer, pos, property_window,
                 show=True):

        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxListBox', parent, id, sizer,
                             pos, property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.selection = 0
        self.choices = choices

        # initialise properties remaining staff
        self.access_functions['choices'] = (self.get_choices, self.set_choices)
        self.properties['choices'] = ChoicesProperty(
            self, 'choices', None, [(_('Label'), GridProperty.STRING)],
            len(choices), label=_('choices'))
        self.access_functions['selection'] = (self.get_selection,
                                              self.set_selection)
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['selection'] = SpinProperty(self, 'selection', None,
                                                    r=(0, len(choices)-1), label=_('selection'))
        self.properties['style'] = CheckListProperty(
            self, 'style', self.widget_writer)

    def create_widget(self):
        self.widget = wx.ListBox(self.parent.widget, self.id,
                                 choices=self.choices)
        self.set_selection(self.selection)
        wx.EVT_LEFT_DOWN(self.widget, self.on_set_focus)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.ScrolledWindow(self.notebook, -1, style=wx.TAB_TRAVERSAL)
        szr = wx.BoxSizer(wx.VERTICAL)
        self.properties['choices'].display(panel)
        self.properties['style'].display(panel)
        self.properties['selection'].display(panel)
        szr.Add(self.properties['style'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['selection'].panel, 0, wx.EXPAND)
        ch = self.properties['choices'].panel
        ch.SetSize((ch.GetSize()[0]-20, 200))
        szr.Add(self.properties['choices'].panel, 1, wx.ALL|wx.EXPAND, 5)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        w, h = panel.GetSize()
        from math import ceil
        panel.SetScrollbars(5, 5, int(ceil(w/5.0)), int(ceil(h/5.0)))
        self.notebook.AddPage(panel, 'Widget')
        self.properties['choices'].set_col_sizes([-1])

    def get_property_handler(self, prop_name):
        if prop_name == 'choices':
            return ChoicesHandler(self)
        return ManagedBase.get_property_handler(self, prop_name)

    def get_choices(self):
        return zip(self.choices)

    def set_choices(self, values):
        self.choices = [ misc.wxstr(v[0]) for v in values ]
        self.properties['selection'].set_range(0, len(self.choices)-1)
        if self.widget:
            self.widget.Clear()
            for c in self.choices: self.widget.Append(c)
            if not self.properties['size'].is_active():
                self.sizer.set_item(self.pos, size=self.widget.GetBestSize())
            self.widget.SetSelection(
                int(self.properties['selection'].get_value()))

    def get_selection(self):
        return self.selection

    def set_selection(self, value):
        value = int(value)
        if value != self.selection:
            self.selection = value
            if self.widget:
                self.widget.SetSelection(value)

# end of class EditListBox


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditListBox objects.
    """
    name = 'list_box_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'list_box_%d' % number[0]
    list_box = EditListBox(name, parent, wx.NewId(),
                           [u'choice 1', ], sizer, pos,
                           common.property_panel)
    node = Tree.Node(list_box)
##     sizer.set_item(pos, size=list_box.GetBestSize())
    list_box.node = node
    list_box.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditListBox objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    list_box = EditListBox(name, parent, wx.NewId(), [], sizer, pos,
                           common.property_panel)
    sizer.set_item(list_box.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border)
    node = Tree.Node(list_box)
    list_box.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return list_box


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditListBox'] = builder
    common.widgets_from_xml['EditListBox'] = xml_builder

    return common.make_object_button('EditListBox', 'list_box.xpm')
