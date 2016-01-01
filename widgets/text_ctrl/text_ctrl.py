"""\
wxTextCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import math
import wx

from edit_windows import ManagedBase, EditStylesMixin
from tree import Tree
import common
import compat
import config
import misc
from widget_properties import *


class EditTextCtrl(ManagedBase, EditStylesMixin):
    """\
    Class to handle wxTextCtrl objects
    """

    def __init__(self, name, parent, id, sizer, pos, property_window,
                 show=True):

        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxTextCtrl', parent, id, sizer, pos,
                             property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.value = ""
        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wx.ALL

        # initialise properties remaining staff
        prop = self.properties
        self.access_functions['value'] = (self.get_value, self.set_value)
        self.access_functions['style'] = (self.get_style, self.set_style)
        prop['value'] = TextProperty(self, 'value', None,
                                     multiline=True, label=_("value"))
        prop['style'] = CheckListProperty(self, 'style', self.widget_writer)

    def create_widget(self):
        value = self.value
        if 'wxTE_MULTILINE' in self.get_string_style():
            value = value.replace('\\n', '\n')
        self.widget = wx.TextCtrl(self.parent.widget, self.id, value=value,
                                  style=self.get_int_style())

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.ScrolledWindow(self.notebook, -1, style=wx.TAB_TRAVERSAL)
        prop = self.properties
        prop['value'].display(panel)
        prop['style'].display(panel)
        szr = wx.BoxSizer(wx.VERTICAL)
        szr.Add(prop['value'].panel, 0, wx.EXPAND)
        szr.Add(prop['style'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, _('Widget'))
        panel.SetScrollbars(
            1, 5, 1, int(math.ceil(panel.GetClientSize()[1]/5.0)))

    def get_value(self):
        return self.value

    def set_value(self, value):
        value = misc.wxstr(value)
        if not misc.streq(value, self.value):
            self.value = value
            if 'wxTE_MULTILINE' in self.get_string_style():
                value = value.replace('\\n', '\n')
            if self.widget:
                self.widget.SetValue(value)

    def _set_widget_style(self):
        # Quote from wxWidgets documentation about changing styles
        # dynamically:
        #
        # Note that alignment styles (wxTE_LEFT, wxTE_CENTRE and wxTE_RIGHT)
        # can be changed dynamically after control creation on wxMSW and
        # wxGTK. wxTE_READONLY, wxTE_PASSWORD and wrapping styles can be
        # dynamically changed under wxGTK but not wxMSW. The other styles can
        # be only set during control creation.
        if self.widget:
            old_style = self.widget.GetWindowStyleFlag()
            new_style = self.get_int_style()
            if old_style != new_style:
                focused = misc.focused_widget is self
                self.sel_marker.Destroy()
                # hide old frame, create_widget() creates a new one
                self.widget.Hide()
                self.create_widget()
                if not self.properties['size'].is_active():
                    self.widget.SetSize(self.widget.GetBestSize())
                self.finish_widget_creation()
                self.sizer.layout()
                if focused:
                    misc.focused_widget = self
                    self.sel_marker.Show(True)

# end of class EditTextCtrl


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditTextCtrl objects.
    """
    name = 'text_ctrl_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'text_ctrl_%d' % number[0]
    text = EditTextCtrl(name, parent, wx.NewId(), sizer, pos,
                        common.property_panel)
    node = Tree.Node(text)
    text.node = node
    text.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory function to build EditTextCtrl objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    text = EditTextCtrl(name, parent, wx.NewId(), sizer, pos,
                        common.property_panel)
    sizer.set_item(text.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border)
    node = Tree.Node(text)
    text.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return text


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditTextCtrl'] = builder
    common.widgets_from_xml['EditTextCtrl'] = xml_builder

    return common.make_object_button('EditTextCtrl', 'text_ctrl.xpm')
