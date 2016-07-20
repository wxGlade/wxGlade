"""\
wxRadioButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
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
from misc import wxGladeRadioButton


class EditRadioButton(ManagedBase, EditStylesMixin):
    """\
    Class to handle wxRadioButton objects
    """

    update_widget_style = False

    def __init__(self, name, parent, id, label, sizer, pos, property_window,
                 show=True):

        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxRadioButton', parent, id, sizer,
                             pos, property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.label = label
        self.value = 0  # if nonzero, che radio button is selected
        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wx.ALL

        # initialise properties remaining staff
        self.access_functions['label'] = (self.get_label, self.set_label)
        self.access_functions['clicked'] = (self.get_value, self.set_value)
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['label'] = TextProperty(self, 'label', None,
                                                multiline=True, label=_("label"))
        self.properties['clicked'] = CheckBoxProperty(self, 'clicked', None,
                                                      _('Clicked'))
        self.properties['style'] = CheckListProperty(
            self, 'style', self.widget_writer)

    def create_widget(self):
        label = self.label.replace('\\n', '\n')
        self.widget = wxGladeRadioButton(self.parent.widget, self.id,
                                         label)
        try:
            self.widget.SetValue(self.value)
        except AttributeError:
            raise

        wx.EVT_CHECKBOX(self.widget, self.id,
                        lambda e: self.widget.SetValue(self.value))

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.Panel(self.notebook, -1)
        szr = wx.BoxSizer(wx.VERTICAL)
        self.properties['label'].display(panel)
        self.properties['clicked'].display(panel)
        self.properties['style'].display(panel)
        szr.Add(self.properties['label'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['clicked'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['style'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, _('Widget'))

    def get_label(self):
        return self.label

    def get_value(self):
        return self.value

    def set_label(self, value):
        value = misc.wxstr(value)
        if not misc.streq(value, self.label):
            self.label = value
            if self.widget:
                self.widget.SetLabel(value.replace('\\n', '\n'))
                if not self.properties['size'].is_active():
                    self.sizer.set_item(self.pos,
                                        size=self.widget.GetBestSize())

    def set_value(self, value):
        self.value = int(value)
        if self.widget: self.widget.SetValue(self.value)

# end of class EditRadioButton


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditRadioButton objects.
    """
    label = u'radio_btn_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = u'radio_btn_%d' % number[0]
    radio = EditRadioButton(label, parent, wx.NewId(), label, sizer, pos,
                            common.property_panel)
    node = Tree.Node(radio)
    radio.node = node
    radio.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditRadioButton objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    radio = EditRadioButton(label, parent, wx.NewId(), "",
                            sizer, pos, common.property_panel)
    sizer.set_item(radio.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border)
    node = Tree.Node(radio)
    radio.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return radio


def initialize():
    """\
    initialization function for the module: returns a wx.BitmapButton to be
    added to the main palette.
    """
    common.widgets['EditRadioButton'] = builder
    common.widgets_from_xml['EditRadioButton'] = xml_builder

    return common.make_object_button('EditRadioButton', 'radio_button.xpm')
