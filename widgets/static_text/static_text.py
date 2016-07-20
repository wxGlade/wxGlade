"""\
wxStaticText objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import wx.lib.stattext
import common
import compat
import config
import misc
from edit_windows import ManagedBase, EditStylesMixin
from tree import Tree
from widget_properties import *


class EditStaticText(ManagedBase, EditStylesMixin):
    """\
    Class to handle wxStaticText objects
    """

    def __init__(self, name, parent, id, label, sizer, pos, property_window,
                 show=True):

        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxStaticText', parent, id, sizer,
                             pos, property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.label = label
        self.attribute = True
        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wx.ALL

        # initialise properties remaining staff
        self.access_functions['label'] = (self.get_label, self.set_label)
        self.access_functions['style'] = (self.get_style, self.set_style)

        def set_attribute(v):
            self.attribute = int(v)
        self.access_functions['attribute'] = (lambda : self.attribute,
                                              set_attribute)

        prop = self.properties
        prop['label'] = TextProperty(
            self, 'label', None, multiline=True, label=_('label'))
        prop['style'] = CheckListProperty(self, 'style', self.widget_writer)
        prop['attribute'] = CheckBoxProperty(
            self, 'attribute', None, _('Store as attribute'),
            write_always=True)

    def create_widget(self):
        label = self.label.replace('\\n', '\n')
        self.widget = wx.lib.stattext.GenStaticText(
            self.parent.widget, self.id, label)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.Panel(self.notebook, -1)
        szr = wx.BoxSizer(wx.VERTICAL)
        self.properties['label'].display(panel)
        self.properties['style'].display(panel)
        self.properties['attribute'].display(panel)
        szr.Add(self.properties['label'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['style'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['attribute'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, _('Widget'))

    def get_label(self): return self.label

    def set_label(self, value):
        value = misc.wxstr(value)
        if not misc.streq(value, self.label):
            self.label = value
            if self.widget:
                self.widget.SetLabel(value.replace('\\n', '\n'))
                if not self.properties['size'].is_active():
                    self.sizer.set_item(self.pos,
                                        size=self.widget.GetBestSize())

# end of class EditStaticText


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditStaticText objects.
    """
    label = u'label_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = u'label_%d' % number[0]
    static_text = EditStaticText(label, parent, wx.NewId(), label, sizer,
                                 pos, common.property_panel)
    node = Tree.Node(static_text)
    static_text.node = node
    static_text.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditStaticText objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    static_text = EditStaticText(label, parent, wx.NewId(), "", sizer, pos,
                                 common.property_panel)
    sizer.set_item(static_text.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border)
    node = Tree.Node(static_text)
    static_text.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return static_text


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditStaticText'] = builder
    common.widgets_from_xml['EditStaticText'] = xml_builder

    return common.make_object_button('EditStaticText', 'static_text.xpm')
