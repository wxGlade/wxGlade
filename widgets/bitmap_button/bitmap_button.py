# bitmap_button.py: wxBitmapButton objects
# $Id: bitmap_button.py,v 1.21 2005/05/06 21:48:23 agriggio Exp $
#
# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
import common, misc, os
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *


class EditBitmapButton(ManagedBase):

    events = ['EVT_BUTTON']
    
    def __init__(self, name, parent, id, bmp_file, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxBitmapButton objects
        """
        import config
        ManagedBase.__init__(self, name, 'wxBitmapButton', parent, id, sizer,
                             pos, property_window, show=show)
        self.default = False
        self.set_bitmap(bmp_file)
        # bitmap property
        self.access_functions['bitmap'] = (self.get_bitmap, self.set_bitmap)
        self.properties['bitmap'] = FileDialogProperty(self, 'bitmap', None,
                                                       style=wxOPEN |
                                                       wxFILE_MUST_EXIST,
                                                       can_disable=False)
        self.access_functions['default'] = (self.get_default, self.set_default)
        self.properties['default'] = CheckBoxProperty(self, 'default', None)
        # 2003-08-07: added 'disabled_bitmap' property
        self.disabled_bitmap = ""
        self.access_functions['disabled_bitmap'] = (self.get_disabled_bitmap,
                                                    self.set_disabled_bitmap)
        self.properties['disabled_bitmap'] = FileDialogProperty(
            self, 'disabled_bitmap', None, style=wxOPEN|wxFILE_MUST_EXIST)
        # 2003-09-04 added default_border
        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wxALL

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        self.properties['bitmap'].display(panel)
        self.properties['disabled_bitmap'].display(panel)
        self.properties['default'].display(panel)
        szr = wxBoxSizer(wxVERTICAL)
        szr.Add(self.properties['bitmap'].panel, 0, wxEXPAND)
        szr.Add(self.properties['disabled_bitmap'].panel, 0, wxEXPAND)
        szr.Add(self.properties['default'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def get_bitmap(self):
        return self.bitmap

    def set_bitmap(self, value):
        self.bitmap = value
        if self.widget:
            bmp = self.load_bitmap()
            self.widget.SetBitmapLabel(bmp)
            self.widget.SetBitmapSelected(bmp)
            self.widget.SetBitmapFocus(bmp)
            self.set_size("%s, %s" % tuple(self.widget.GetBestSize()))

    def get_disabled_bitmap(self):
        return self.disabled_bitmap

    def set_disabled_bitmap(self, value):
        self.disabled_bitmap = value
        if self.widget:
            bmp = self.load_bitmap(self.disabled_bitmap)
            self.widget.SetBitmapDisabled(bmp)
            self.set_size("%s, %s" % tuple(self.widget.GetBestSize()))

    def create_widget(self):
        bmp = self.load_bitmap()
        self.widget = wxBitmapButton(self.parent.widget, self.id, bmp)

    def load_bitmap(self, which=None, empty=[None]):
        if which is None: which = self.bitmap
        if which and not ( which.startswith('var:') or
                            which.startswith('code:')
                          ):
            return wxBitmap(which, wxBITMAP_TYPE_ANY)
        else:
            if empty[0] is None:
                empty[0] = wxEmptyBitmap(1, 1)         
            return empty[0]

    def get_default(self):
        return self.default

    def set_default(self, value):
        self.default = bool(int(value))
##         if value and self.widget:
##             self.widget.SetDefault()

# end of class EditBitmapButton
        

def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditBitmapButton objects.
    """
    
    name = 'bitmap_button_%s' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'bitmap_button_%s' % number[0]
    bitmap = misc.FileSelector("Select the image for the button")
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
