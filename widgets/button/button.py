"""
wxButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import config
import common
import compat
import misc
from edit_windows import ManagedBase, EditStylesMixin
from tree import Tree
from widget_properties import *
from button_stockitems import *


class EditButton(ManagedBase, EditStylesMixin):
    """\
    Class to handle wxButton objects
    """

    def __init__(self, name, parent, id, label, sizer, pos, property_window,
                 show=True):

        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxButton', parent, id, sizer, pos,
                             property_window, show=show)
        EditStylesMixin.__init__(self)

        # initialise instance variables
        self.label = label
        self.default = False
        self.stockitem = "None"
        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wx.ALL

        # initialise properties remaining staff
        self.access_functions['label'] = (self.get_label, self.set_label)
        self.properties['label'] = TextProperty(self, 'label', None,
                                                multiline=True)
        self.access_functions['stockitem'] = (self.get_stockitem,
                                              self.set_stockitem)
        self.access_functions['default'] = (self.get_default, self.set_default)
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['default'] = CheckBoxProperty(
            self, 'default', None, label=_("Default"))
        self.properties['default'].tooltip = \
            _("This sets the button to be the default "
              "item for the panel or dialog box.")

        #Get the list of items, and add a 'None'
        choices = ButtonStockItems.stock_ids.keys()
        choices.sort()
        choices[:0] = ['None']
        self.properties['stockitem'] = ComboBoxProperty(
            self, 'stockitem', choices, can_disable=True,
            label=_("Stock item"))
        self.properties['stockitem'].tooltip = \
            _("Standard IDs for button identifiers")
        self.properties['style'] = CheckListProperty(
            self, 'style', self.widget_writer)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.Panel(self.notebook, -1)
        self.properties['label'].display(panel)
        self.properties['stockitem'].display(panel)
        self.properties['default'].display(panel)
        self.properties['style'].display(panel)
        szr = wx.BoxSizer(wx.VERTICAL)
        szr.Add(self.properties['label'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['stockitem'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['default'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['style'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
        if self.stockitem != "None":
            s = common.app_tree.app.saved
            self.set_stockitem(self.stockitem)
            common.app_tree.app.saved = s

    def get_label(self):
        return self.label

    def set_label(self, value):
        value = misc.wxstr(value)
        if not misc.streq(value, self.label):
            if self.widget:
                self.widget.SetLabel(value.replace('\\n', '\n'))
                if not self.properties['size'].is_active():
                    self.sizer.set_item(self.pos,
                                        size=self.widget.GetBestSize())
            self.label = value

    def create_widget(self):
        label = self.label.replace('\\n', '\n')
        try:
            self.widget = wx.Button(self.parent.widget, self.id, label,
                                    style=self.get_int_style())
        except AttributeError:
            self.widget = wx.Button(self.parent.widget, self.id, label)

    def get_default(self):
        return self.default

    def set_default(self, value):
        self.default = bool(int(value))

    def get_stockitem(self):
        return self.stockitem

    def set_stockitem(self, value):
        self.stockitem = misc.wxstr(value)
        if self.stockitem != "None":
            l = ButtonStockItems.stock_ids[self.stockitem]
            self.set_label(l)
            self.properties['label'].set_value(l)
            if self.properties['label'].panel is not None:
                self.properties['label'].text.Enable(False)
            self.window_id = "wxID_" + self.stockitem
            self.properties['id'].set_value(self.window_id)
            self.properties['id'].toggle_active(False)
        else:
            if self.properties['label'].panel is not None:
                self.properties['label'].text.Enable(True)

# end of class EditButton


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditButton objects.
    """
    name = u'button_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = u'button_%d' % number[0]
    button = EditButton(name, parent, wx.NewId(), name, sizer, pos,
                        common.property_panel)
    node = Tree.Node(button)
    button.node = node
    button.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditButton objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    button = EditButton(label, parent, wx.NewId(), '', sizer, pos,
                        common.property_panel, show=False)
    sizer.set_item(button.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border)
    node = Tree.Node(button)
    button.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return button


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditButton'] = builder
    common.widgets_from_xml['EditButton'] = xml_builder

    return common.make_object_button('EditButton', 'button.xpm')
