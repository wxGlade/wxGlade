# edit_widget.py: base class for EditFoo objects
#
# Copyright (c) 2002-2003 Richard Lawson <richard.lawson@colinx.com>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
import common, misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *


class EditWidget(ManagedBase):
    def __init__(self, name, klass, parent, id, label, sizer, pos, property_window,
 show=True):
        """\
        Class to handle wxFoo objects
        """
        self.label = label
        self.default = False
        ManagedBase.__init__(self, name, klass, parent, id, sizer, pos,
                             property_window, show=show)
        
        # introspect subclass looking for properties
        # and widgets
        self.property_names = []
        attrs = dir(self)
        for attr in attrs:
            prefix = attr[0:4]
            if prefix == 'get_':
                getter = attr
                #print 'found getter ', getter
                # extract the property name
                prefix, name = attr.split('_', 1)
                #print 'getter ', name
                # check for a setter
                setter = 'set_%s' % name
                if not hasattr(self, setter):
                    #print 'no setter for %s, skipping ' % name
                    continue
                # check for a get_name_widget
                getter_widget = 'get_%s_widget' % name
                if not hasattr(self, getter_widget):
                    #print 'no widget getter for %s, skipping ' % name
                    continue
                #print 'adding property: %s' % name
                self.property_names.append(name)
                self.access_functions[name] = (getattr(self, getter), getattr(self, setter))
                self.properties[name] = getattr(self, getter_widget)()
        

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        szr = wxBoxSizer(wxVERTICAL)
        for name in self.property_names:
            self.properties[name].display(panel)
            szr.Add(self.properties[name].panel, 0, wxEXPAND)
        panel.SetAutoLayout(1)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
        
    
def increment_label(label, number=[1]):
    _label = '%s_%d' % (label, number[0])
    while common.app_tree.has_name(label):
        number[0] += 1
        _label = '%s_%d' % (label, number[0])
    return _label
        
def add_widget_node(widget, sizer, pos):
    node = Tree.Node(widget)
    widget.node = node
    widget.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos-1)
    
def get_label_from_xml(attrs):
    from xml_parse import XmlParsingError
    try: label = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"

def initialize(edit_klass, builder, xml_builder, icon_path):
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets[edit_klass] = builder
    common.widgets_from_xml[edit_klass] = xml_builder

    return common.make_object_button(edit_klass, icon_path)
