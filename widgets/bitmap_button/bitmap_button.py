# bitmap_button.py: wxBitmapButton objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: Python 2.2 license (see license.txt)

from wxPython.wx import *
import common, misc, os
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *

_bmp_types = {
    '.bmp' : wxBITMAP_TYPE_BMP,
    '.gif' : wxBITMAP_TYPE_GIF,
    '.xpm' : wxBITMAP_TYPE_XPM,
    '.jpg' : wxBITMAP_TYPE_JPEG,
    '.jpeg': wxBITMAP_TYPE_JPEG,
    '.png' : wxBITMAP_TYPE_PNG,
    '.pcx' : wxBITMAP_TYPE_PCX
    }

_bmp_str_types = {
    '.bmp' : 'wxBITMAP_TYPE_BMP',
    '.gif' : 'wxBITMAP_TYPE_GIF',
    '.xpm' : 'wxBITMAP_TYPE_XPM',
    '.jpg' : 'wxBITMAP_TYPE_JPEG',
    '.jpeg': 'wxBITMAP_TYPE_JPEG',
    '.png' : 'wxBITMAP_TYPE_PNG',
    '.pcx' : 'wxBITMAP_TYPE_PCX'
    }

class EditBitmapButton(ManagedBase):
    def __init__(self, name, parent, id, bmp_file, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxBitmapButton objects
        """
        ManagedBase.__init__(self, name, 'wxBitmapButton', parent, id, sizer,
                             pos, property_window, show=show)
        self.set_bitmap(str(bmp_file))
        # bitmap property
        self.access_functions['bitmap'] = (self.get_bitmap, self.set_bitmap)
        self.properties['bitmap'] = FileDialogProperty(self, 'bitmap', None,
                                                       style=wxOPEN |
                                                       wxFILE_MUST_EXIST,
                                                       can_disable=False)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        self.properties['bitmap'].display(panel)
        szr = wxBoxSizer(wxVERTICAL)
        szr.Add(self.properties['bitmap'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def get_bitmap(self):
        return self.bitmap

    def set_bitmap(self, value):
        type = self.guess_type(value)
        if type is None:
            self.bitmap = ''
        else:
            self.bitmap = value
        if self.widget:
            bmp = self.load_bitmap(type)
            self.widget.SetBitmapLabel(bmp)
            self.widget.set_size("%s, %s" % tuple(self.widget.GetBestSize()))

    def create_widget(self):
        bmp = self.load_bitmap(self.guess_type(self.bitmap))
        self.widget = wxBitmapButton(self.parent.widget, self.id, bmp)

    def load_bitmap(self, type):
        if self.bitmap:
            return wxBitmap(os.path.abspath(self.bitmap), type)
        else:
            return wxNullBitmap

    def guess_type(self, filename):
        return _bmp_types.get(os.path.splitext(str(filename))[1], None)

# end of class EditBitmapButton
        

def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditBitmapButton objects.
    """
    
    name = 'bitmap_button_%s' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'bitmap_button_%s' % number[0]
    bitmap = wxFileSelector("Select the image for the button")
    button = EditBitmapButton(name, parent, wxNewId(), bitmap, sizer, pos,
                              common.property_panel)
    node = Tree.Node(button)
    button.node = node
    button.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos-1)

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditBitmapButton objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: label = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    button = EditBitmapButton(label, parent, wxNewId(), '', sizer, pos,
                              common.property_panel, show=False)
    sizer.set_item(button.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border) #, size=button.GetBestSize())
    node = Tree.Node(button)
    button.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return button


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditBitmapButton'] = builder
    common.widgets_from_xml['EditBitmapButton'] = xml_builder

    return common.make_object_button('EditBitmapButton',
                                     'icons/bitmap_button.xpm')
