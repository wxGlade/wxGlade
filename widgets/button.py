# button.py: wxButton objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
import common, misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *


class EditButton(ManagedBase):
    def __init__(self, name, parent, id, label, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxButton objects
        """
        #wxButton.__init__(self, parent, id, label)
        self.label = label
        ManagedBase.__init__(self, name, 'wxButton', parent, id, sizer, pos,
                             property_window, show=show)
        self.access_functions['label'] = (self.get_label, self.set_label)
        self.properties['label'] = TextProperty(self, 'label', None) 

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        self.properties['label'].display(panel)
        szr = misc.Sizer(wxVERTICAL) #wxBoxSizer(wxVERTICAL)
        szr.Add(self.properties['label'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(1)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def get_label(self):
        return self.label

    def set_label(self, value):
        if value != self.label:
            if self.widget:
                self.SetLabel(value)
                self.set_width(self.GetBestSize()[0])
            self.label = value
            self.old_label = value

    def create_widget(self):
        self.widget = wxButton(self, self.parent, self.id, self.label)


# end of class EditButton
        
def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditButton objects.
    """
    label = 'button_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'button_%d' % number[0]
    button = EditButton(label, parent, wxNewId(), misc._encode(label), sizer,
                        pos, common.property_panel)
    node = Tree.Node(button)
    button.node = node
    common.app_tree.insert(node, sizer.node, pos-1)

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditButton objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: label = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    button = EditButton(label, parent, wxNewId(), misc._encode(label), sizer,
                        pos, common.property_panel)
    sizer.set_item(button.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border, size=button.GetBestSize())
    node = Tree.Node(button)
    button.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return button

def code_generator(obj):
    """\
    fuction that generates python code for wxButton objects.
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    label = '"' + prop.get('label', '').replace('"', '\"') + '"'
    if obj.is_toplevel:
        l = ['self.%s = %s(self, %s, %s)\n' % (obj.name, obj.klass, id, label)]
        if id_name: l.append(id_name) # init lines are written in reverse order
        return l , [], []    
    size = pygen.generate_code_size(obj)
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    init = [ 'self.%s = wxButton(%s, %s, %s, size=%s)\n' % 
             (obj.name, parent, id, label, size) ]
    if id_name: init.append(id_name) # init lines are written in reverse order
    props_buf = []
    if prop.has_key('foreground'):
        props_buf.append(pygen.generate_code_foreground(obj))
    if prop.has_key('background'):
        props_buf.append(pygen.generate_code_background(obj))
    if prop.has_key('font'): props_buf.append(pygen.generate_code_font(obj))
    return init, props_buf, []

def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditButton'] = builder
    common.widgets_from_xml['EditButton'] = xml_builder
    common.class_names['EditButton'] = 'wxButton'

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxButton', code_generator)
    
    return common.make_object_button('EditButton', 'icons/button.xpm')
