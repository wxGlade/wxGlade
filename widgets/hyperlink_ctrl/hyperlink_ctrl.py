"""\
wxHyperlinkCtrl objects

@copyright: 2012-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import wx
import common
import compat
import config
import misc
from edit_windows import ManagedBase, EditStylesMixin
from tree import Tree
from widget_properties import *


class EditHyperlinkCtrl(ManagedBase, EditStylesMixin):
    """\
    Class to handle wxHyperlinkCtrl objects
    """

    def __init__(self, name, parent, id, label, sizer, pos, property_window,
                 show=True):

        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxHyperlinkCtrl', parent, id, sizer,
                             pos, property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.attribute = True
        self.label = label
        self.url = ""
        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wx.ALL

        # initialise properties remaining staff
        self.access_functions['label'] = (self.get_label, self.set_label)
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.access_functions['url'] = (self.get_url, self.set_url)
        self.access_functions['attribute'] = (self.get_attribute,
                                              self.set_attribute)

        self.properties['label'] = TextProperty(
            self, 'label', label=_('label'))
        self.properties['label'].set_tooltip(_("Label of the hyperlink"))

        self.properties['url'] = TextProperty(self, 'url', label=_('url'))
        self.properties['url'].set_tooltip(
            _("URL associated with the given label"))

        self.properties['style'] = CheckListProperty(
            self, 'style', self.widget_writer)

        self.properties['attribute'] = CheckBoxProperty(
            self, 'attribute', label=_('Store as attribute'),
            write_always=True)

    def create_widget(self):
        label = self.label.replace('\\n', '\n')
        self.widget = wx.HyperlinkCtrl(
            self.parent.widget, self.id, label, self.url)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.Panel(self.notebook, -1)
        szr = wx.BoxSizer(wx.VERTICAL)
        self.properties['label'].display(panel)
        self.properties['style'].display(panel)
        self.properties['url'].display(panel)
        self.properties['attribute'].display(panel)
        szr.Add(self.properties['label'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['url'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['style'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['attribute'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, _('Widget'))

    def get_attribute(self):
        return self.attribute

    def set_attribute(self, value):
        self.attribute = int(value)

    def get_label(self):
        return self.label

    def set_label(self, value):
        value = misc.wxstr(value)
        if not misc.streq(value, self.label):
            self.label = value
            if self.widget:
                self.widget.SetLabel(value.replace('\\n', '\n'))
                if not self.properties['size'].is_active():
                    self.sizer.set_item(self.pos,
                                        size=self.widget.GetBestSize())

    def get_url(self):
        return self.url

    def set_url(self, url):
        self.url = url

# end of class EditHyperlinkCtrl


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditHyperlinkCtrl objects.
    """
    name = u'hyperlink_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = u'hyperlink_%d' % number[0]
    hyperlink_ctrl = EditHyperlinkCtrl(name, parent, wx.NewId(), name,
                                       sizer, pos, common.property_panel)
    node = Tree.Node(hyperlink_ctrl)
    hyperlink_ctrl.node = node
    hyperlink_ctrl.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditHyperlinkCtrl objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    hyperlink_ctrl = EditHyperlinkCtrl(name, parent, wx.NewId(),
                                 "", sizer, pos,
                                 common.property_panel)
    sizer.set_item(hyperlink_ctrl.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border)
    node = Tree.Node(hyperlink_ctrl)
    hyperlink_ctrl.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return hyperlink_ctrl


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditHyperlinkCtrl'] = builder
    common.widgets_from_xml['EditHyperlinkCtrl'] = xml_builder

    return common.make_object_button(
        'EditHyperlinkCtrl', 'hyperlink_ctrl.xpm')
