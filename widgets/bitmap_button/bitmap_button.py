"""\
wxBitmapButton objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx

import common, config
from edit_windows import ManagedBase, EditStylesMixin
from gui_mixins import BitmapMixin
from tree import Node
import new_properties as np


class EditBitmapButton(ManagedBase, EditStylesMixin, BitmapMixin):
    "Class to handle wxBitmapButton objects"

    _PROPERTIES = ["Widget", "bitmap", "disabled_bitmap", "default", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    _PROPERTY_HELP = {"bitmap": BitmapMixin.bitmap_tooltip_text,
                      "disabled_bitmap": BitmapMixin.bitmap_tooltip_text}

    def __init__(self, name, parent, id, bmp_file, sizer, pos):
        ManagedBase.__init__(self, name, 'wxBitmapButton', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)
        BitmapMixin.__init__(self)

        # initialise instance properties
        filedialog_style = wx.FD_OPEN | wx.FD_FILE_MUST_EXIST  # for the following two properties
        self.bitmap          = np.FileNameProperty(bmp_file, style=filedialog_style)
        self.disabled_bitmap = np.FileNamePropertyD("", default_value="", style=filedialog_style)
        self.default         = np.CheckBoxProperty(False)

        if config.preferences.default_border:
            self.border.set( config.preferences.default_border_size )
            self.flag.set( wx.ALL )


    def create_widget(self):
        bmp = self.get_preview_obj_bitmap()
        #try:
        self.widget = wx.BitmapButton(self.parent.widget, self.id, bmp, style=self.style)
        if self.disabled_bitmap:
            bmp = self.get_preview_obj_bitmap(self.disabled_bitmap)
            self.widget.SetBitmapDisabled(bmp)

        #except AttributeError:
            #self.widget = wx.BitmapButton(self.parent.widget, self.id, bmp)

    def properties_changed(self, modified=None):
        "update label (and size if label/stockitem have changed)"
        if self.widget:
            resize = False
            if not modified or "bitmap" in modified:
                bmp = self.get_preview_obj_bitmap(self.bitmap)
                self.widget.SetBitmapLabel(bmp)
                self.widget.SetBitmapSelected(bmp)
                self.widget.SetBitmapFocus(bmp)
                resize = True
            if not modified or "disabled_bitmap" in modified:
                bmp = self.get_preview_obj_bitmap(self.disabled_bitmap)
                self.widget.SetBitmapDisabled(bmp)
                resize = True

            if resize: self._set_widget_best_size()
            #size_p = self.properties["size"]
            #if resize and size_p.get()=="-1, -1":
                #self.sizer.set_item(self.pos, size=self.widget.GetBestSize())
                #if not size_p.is_active():
                    #size_p.set( self.widget.GetBestSize() )

        ManagedBase.properties_changed(self, modified)


def builder(parent, sizer, pos, number=[1]):
    "factory function for EditBitmapButton objects"
    name = 'bitmap_button_%s' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'bitmap_button_%s' % number[0]
    bitmap = wx.FileSelector(_("Select the image for the button"))
    button = EditBitmapButton(name, parent, wx.NewId(), bitmap, sizer, pos)
    node = Node(button)
    button.node = node
    if parent.widget: button.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditBitmapButton objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    button = EditBitmapButton(label, parent, wx.NewId(), '', sizer, pos)
    sizer.set_item(button.pos, proportion=sizeritem.proportion, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(button)
    button.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return button


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditBitmapButton'] = builder
    common.widgets_from_xml['EditBitmapButton'] = xml_builder

    return common.make_object_button('EditBitmapButton', 'bitmap_button.xpm')
