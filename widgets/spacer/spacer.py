# spacer.py: spacers to use in sizers
# $Id: spacer.py,v 1.10 2005/05/06 21:48:18 agriggio Exp $
#
# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
import common, misc
from tree import Tree
from widget_properties import *
from edit_windows import ManagedBase

class EditSpacer(ManagedBase):
    def __init__(self, name, parent, id, width, height, sizer, pos,
                 property_window, show=True):
        """\
        Class to handle spacers for sizers
        """
        ManagedBase.__init__(self, name, 'spacer', parent, id, sizer,
                             pos, property_window, show=show)
        self.__size = [width, height]

        self.access_functions['width'] = (self.get_width, self.set_width)
        self.access_functions['height'] = (self.get_height, self.set_height)

        self.properties['width'] = SpinProperty(self, 'width', None)
        self.properties['height'] = SpinProperty(self, 'height', None)

    def create_widget(self):
        self.widget = wxPanel(self.parent.widget, self.id, size=self.__size)
        self.widget.GetBestSize = self.widget.GetSize
        
    def create_properties(self):
        ManagedBase.create_properties(self)
        page = self.notebook.GetPage(1)
        wp = self.properties['width']
        hp = self.properties['height']
        wp.display(page)
        hp.display(page)
        szr = page.GetSizer()
        szr.Insert(0, hp.panel, 0, wxEXPAND)
        szr.Insert(0, wp.panel, 0, wxEXPAND)
        szr.Layout()
        szr.Fit(page)
        import math
        w, h = page.GetClientSize()
        page.SetScrollbars(1, 5, 1, int(math.ceil(h/5.0)))
        common_page = self.notebook.GetPage(0)
        common_page.Hide()
        self.notebook.RemovePage(0)
        self.notebook.SetSelection(0)
        
    def get_width(self):
        return self.__size[0]

    def get_height(self):
        return self.__size[1]

    def set_width(self, value):
        value = int(value)
        self.__size[0] = value
        if self.widget:
            self.widget.SetSize(self.__size)
        self.sizer.set_item(self.pos, size=self.__size)

    def set_height(self, value):
        value = int(value)
        self.__size[1] = value
        if self.widget:
            self.widget.SetSize(self.__size)
        self.sizer.set_item(self.pos, size=self.__size)

    def set_flag(self, value):
        ManagedBase.set_flag(self, value)
        if not (self.get_int_flag() & wxEXPAND):
            self.sizer.set_item(self.pos, size=self.__size)

# end of class EditSpacer
        

def builder(parent, sizer, pos):
    """\
    factory function for EditSpacer objects.
    """
    class Dialog(wxDialog):
        def __init__(self):
            wxDialog.__init__(self, misc.get_toplevel_parent(parent), -1,
                              "Enter size")
            
            self.width = SpinProperty(self, 'width', self)
            self.height = SpinProperty(self, 'height', self)
            self.width.set_value(20)
            self.height.set_value(20)
            
            szr = wxBoxSizer(wxVERTICAL)
            szr.Add(self.width.panel, 0, wxEXPAND)
            szr.Add(self.height.panel, 0, wxEXPAND)
            sz = wxBoxSizer(wxHORIZONTAL)
            sz.Add(wxButton(self, wxID_OK, 'OK'))
            szr.Add(sz, 0, wxALL|wxALIGN_CENTER, 4)
            self.SetAutoLayout(True)
            self.SetSizer(szr)
            szr.Fit(self)
            self.Centre()

        def __getitem__(self, name):
            return (lambda : 0, lambda v: None)

    # end of inner class

    dialog = Dialog()
    dialog.ShowModal()
    name = 'spacer'
    spacer = EditSpacer(name, parent, wxNewId(), dialog.width.get_value(),
                        dialog.height.get_value(), sizer, pos,
                        common.property_panel)
    node = Tree.Node(spacer)
    spacer.node = node
    spacer.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos-1) 
    #sizer.set_item(spacer.pos, size=spacer.GetSize())

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditSpacer objects from an xml file
    """
    from xml_parse import XmlParsingError
    if not sizer or not sizeritem:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    spacer = EditSpacer('spacer', parent, wxNewId(), 1, 1, sizer, pos,
                        common.property_panel, True)
    sizer.set_item(spacer.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border)
    node = Tree.Node(spacer)
    spacer.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return spacer


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditSpacer'] = builder
    common.widgets_from_xml['EditSpacer'] = xml_builder
        
    return common.make_object_button('EditSpacer', 'icons/spacer.xpm')
    
