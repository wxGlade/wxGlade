# spacer.py: spacers to use in sizers
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

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

        self.size = (widht, height)

        self.access_functions['width'] = (self.get_width, self.set_width)
        self.access_functions['height'] = (self.get_height, self.set_height)

        wp = self.properties['width'] = SpinProperty(self, 'width', None)
        hp = self.properties['height'] = SpinProperty(self, 'height', None)

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
        page.SetScrollbars(1, 5, 1, math.ceil(h/5.0))
        common_page = self.notebook.GetPage(0)
        common_page.Hide()
        self.notebook.RemovePage(0)
        self.notebook.SetSelection(0)

    def get_width(self):
        return self.size[0]

    def get_height(self):
        return self.size[1]

    def set_width(self, value):
        value = int(value)
        self.size[0] = value
        if self.widget:
            self.widget.SetSize(self.size)
        self.sizer.set_item(self.pos, size=self.size)

    def set_height(self, value):
        value = int(value)
        self.size[1] = value
        if self.widget:
            self.widget.SetSize(self.size)
        self.sizer.set_item(self.pos, size=self.size)

    def create_widget(self):
        self.widget = wxPanel(self.parent, self.id, size=self.size)
        self.widget.GetBestSize = self.GetSize

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
            szr.Add(self.width, 0, wxEXPAND)
            szr.Add(self.height, 0, wxEXPAND)
            sz = wxBoxSizer(wxHORIZONTAL)
            sz.Add(wxButton(self, wxID_OK, 'OK'))
            szr.Add(sz, 0, wxALL|wxALIGN_CENTER, 4)
            self.SetAutoLayout(True)
            self.SetSizer(szr)
            szr.Fit(self)

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
    common.app_tree.insert(node, sizer.node, pos-1) 
    sizer.set_item(spacer.pos, size=spacer.GetSize())

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

def code_generator(spacer):
    """\
    generates the python code for a spacer
    """
    prop = spacer.properties
    width = prop.get('width', '0')
    height = prop.get('height', '0')
    # we must use the hack in pygen.add_sizeritem (see py_codegen.py)
    spacer.name = '%s, %s' % (width, height)
    return [], [], []


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditSpacer'] = builder
    common.widgets_from_xml['EditSpacer'] = xml_builder
    common.class_names['EditSpacer'] = 'spacer'

    # python code generation functions
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('spacer', code_generator)
        
    return common.make_object_button('EditSpacer', 'icons/spacer.xpm')
    
