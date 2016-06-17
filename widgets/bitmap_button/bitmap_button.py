"""\
wxBitmapButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import config
import common
import compat
from edit_windows import ManagedBase, EditStylesMixin
from gui_mixins import BitmapMixin
from tree import Tree
from widget_properties import *


class EditBitmapButton(ManagedBase, EditStylesMixin, BitmapMixin):
    """\
    Class to handle wxBitmapButton objects
    """

    def __init__(self, name, parent, id, bmp_file, sizer, pos,
                 property_window, show=True):

        # Initialise parent classes
        ManagedBase.__init__(self, name, 'wxBitmapButton', parent, id, sizer,
                             pos, property_window, show=show)
        EditStylesMixin.__init__(self)
        BitmapMixin.__init__(self)

        # initialise instance variables
        self.default = False
        self.disabled_bitmap = ""
        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wx.ALL
        self.set_bitmap(bmp_file)

        # initialise properties remaining staff
        access = self.access_functions
        properties = self.properties

        access['bitmap'] = (self.get_bitmap, self.set_bitmap)
        properties['bitmap'] = FileDialogProperty(
            self, 'bitmap', style=wx.FD_OPEN | wx.FD_FILE_MUST_EXIST,
            can_disable=False, label=_("bitmap"))
        properties['bitmap'].set_tooltip(self.bitmap_tooltip_text)

        access['default'] = (self.get_default, self.set_default)
        properties['default'] = CheckBoxProperty(
            self, 'default', label=_("Default"))

        access['disabled_bitmap'] = (self.get_disabled_bitmap,
                                     self.set_disabled_bitmap)
        properties['disabled_bitmap'] = FileDialogProperty(
            self, 'disabled_bitmap',
            style=wx.FD_OPEN | wx.FD_FILE_MUST_EXIST,
            label=_("Disabled bitmap"))
        properties['disabled_bitmap'].set_tooltip(self.bitmap_tooltip_text)

        access['style'] = (self.get_style, self.set_style)
        properties['style'] = CheckListProperty(
            self, 'style', self.widget_writer)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.Panel(self.notebook, -1)
        self.properties['bitmap'].display(panel)
        self.properties['disabled_bitmap'].display(panel)
        self.properties['default'].display(panel)
        self.properties['style'].display(panel)
        szr = wx.BoxSizer(wx.VERTICAL)
        szr.Add(self.properties['bitmap'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['disabled_bitmap'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['default'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['style'].panel, 0, wx.EXPAND)
        panel.SetAutoLayout(True)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def get_bitmap(self):
        return self.bitmap

    def set_bitmap(self, value):
        self.bitmap = value
        if self.widget:
            bmp = self.get_preview_obj_bitmap()
            self.widget.SetBitmapLabel(bmp)
            self.widget.SetBitmapSelected(bmp)
            self.widget.SetBitmapFocus(bmp)
            self.set_size("%s, %s" % tuple(self.widget.GetBestSize()))

    def get_disabled_bitmap(self):
        return self.disabled_bitmap

    def set_disabled_bitmap(self, value):
        self.disabled_bitmap = value
        if self.widget:
            bmp = self.get_preview_obj_bitmap(self.disabled_bitmap)
            self.widget.SetBitmapDisabled(bmp)
            self.set_size("%s, %s" % tuple(self.widget.GetBestSize()))

    def create_widget(self):
        bmp = self.get_preview_obj_bitmap()
        try:
            self.widget = wx.BitmapButton(self.parent.widget, self.id, bmp,
                                          style=self.get_int_style())
        except AttributeError:
            self.widget = wx.BitmapButton(self.parent.widget, self.id, bmp)

    def get_default(self):
        return self.default

    def set_default(self, value):
        self.default = bool(int(value))

# end of class EditBitmapButton


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditBitmapButton objects.
    """

    name = 'bitmap_button_%s' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'bitmap_button_%s' % number[0]
    bitmap = wx.FileSelector(_("Select the image for the button"))
    button = EditBitmapButton(name, parent, wx.NewId(), bitmap, sizer, pos,
                              common.property_panel)
    node = Tree.Node(button)
    button.node = node
    button.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditBitmapButton objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    button = EditBitmapButton(label, parent, wx.NewId(), '', sizer, pos,
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
    common.widgets['EditBitmapButton'] = builder
    common.widgets_from_xml['EditBitmapButton'] = xml_builder

    return common.make_object_button('EditBitmapButton', 'bitmap_button.xpm')
