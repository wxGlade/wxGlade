# toggle_button.py: wxToggleButton objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
import common, misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *


class EditToggleButton(wxToggleButton, ManagedBase):
    def __init__(self, name, parent, id, label, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxToggleButton objects
        """
        wxToggleButton.__init__(self, parent, id, label)
        self.old_label = label        
        ManagedBase.__init__(self, name, 'wxToggleButton', parent, id, sizer,
                             pos, property_window, show=show)

        self.access_functions['label'] = (self.GetLabel, self.set_label)
        self.access_functions['value'] = (self.GetValue,
                                          lambda v: self.SetValue(int(v)))
        self.properties['label'] = TextProperty(self, 'label', None)
        self.properties['value'] = CheckBoxProperty(self, 'value', None,
                                                    'Clicked')

        EVT_TOGGLEBUTTON(self, id, self.on_set_focus)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        szr = misc.Sizer(wxVERTICAL)
        self.properties['label'].display(panel)
        self.properties['value'].display(panel)
        szr.Add(self.properties['label'].panel, 0, wxEXPAND)
        szr.Add(self.properties['value'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
        
    def set_label(self, value):
        if value != self.old_label:
            self.SetLabel(value)
            self.set_width(self.GetBestSize()[0])
            self.old_label = value

# end of class EditToggleButton

        
def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditToggleButton objects.
    """
    label = 'button_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'button_%d' % number[0]
    button = EditToggleButton(label, parent, wxNewId(), misc._encode(label),
                              sizer, pos, common.property_panel)
    node = Tree.Node(button)
    button.node = node
    common.app_tree.insert(node, sizer.node, pos-1)

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditToggleButton objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: label = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    button = EditToggleButton(label, parent, wxNewId(), misc._encode(label),
                              sizer, pos, common.property_panel)
    sizer.set_item(button.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border, size=button.GetBestSize())
    node = Tree.Node(button)
    button.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return button

def code_generator(obj):
    """\
    fuction that generates python code for wxToggleButton objects.
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
    init = [ 'self.%s = wxToggleButton(%s, %s, %s, size=%s)\n' % 
             (obj.name, parent, id, label, size) ]
    if id_name: init.append(id_name) # init lines are written in reverse order
    props_buf = []
    if prop.has_key('foreground'):
        props_buf.append(pygen.generate_code_foreground(obj))
    if prop.has_key('background'):
        props_buf.append(pygen.generate_code_background(obj))
    if prop.has_key('font'): props_buf.append(pygen.generate_code_font(obj))
    value = prop.get('value')
    if value: props_buf.append('self.%s.SetValue(%s)\n' % (obj.name, value))
    return init, props_buf, []

def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditToggleButton'] = builder
    common.widgets_from_xml['EditToggleButton'] = xml_builder
    common.class_names['EditToggleButton'] = 'wxToggleButton'

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxToggleButton', code_generator)
    
    return common.make_object_button('EditToggleButton',
                                     'icons/toggle_button.xpm')
