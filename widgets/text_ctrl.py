# text_ctrl.py: wxTextCtrl objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
from edit_windows import ManagedBase
from tree import Tree
import common, misc
from widget_properties import *

class EditTextCtrl(ManagedBase):
    """\
    Class to handle wxTextCtrl objects
    """
    def __init__(self, name, parent, id, sizer, pos, property_window,
                 show=True):
        ManagedBase.__init__(self, name, 'wxTextCtrl', parent, id, sizer, pos,
                             property_window, show=show)
        self.value = ""
        self.style = 0
        self.access_functions['value'] = (self.get_value, self.set_value)
        self.access_functions['style'] = (self.get_style, self.set_style)
        prop = self.properties
        # value property
        prop['value'] = TextProperty(self, 'value', None)
        # style property
        self.style_pos  = (wxTE_PROCESS_ENTER, wxTE_PROCESS_TAB,
                           wxTE_MULTILINE,wxTE_PASSWORD, wxTE_READONLY,
                           wxHSCROLL, wxTE_RICH)
        style_labels = ('#section#Style', 'wxTE_PROCESS_ENTER',
                        'wxTE_PROCESS_TAB', 'wxTE_MULTILINE', 'wxTE_PASSWORD',
                        'wxTE_READONLY', 'wxHSCROLL', 'wxTE_RICH')
        prop['style'] = CheckListProperty(self, 'style', None, style_labels)

    def create_widget(self):
        self.widget = wxTextCtrl(self.parent.widget, self.id, value=self.value,
                                 style=self.style)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxScrolledWindow(self.notebook, -1) 
        prop = self.properties
        prop['value'].display(panel)
        prop['style'].display(panel)
        szr = misc.Sizer(wxVERTICAL)
        szr.Add(prop['value'].panel, 0, wxEXPAND)
        szr.Add(prop['style'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def get_value(self):
        return self.value

    def set_value(self, value):
        value = str(value)
        if value != self.value:
            self.value = value
            if self.widget: self.widget.SetValue(value)

    def get_style(self):
        retval = [0] * len(self.style_pos)
        try:
            for i in range(len(self.style_pos)):
                if self.style & self.style_pos[i]:
                    retval[i] = 1
        except AttributeError: pass
        return retval

    def set_style(self, value):
        value = self.properties['style'].prepare_value(value)
        self.style = 0
        for v in range(len(value)):
            if value[v]:
                self.style |= self.style_pos[v]
        if self.widget: self.widget.SetWindowStyleFlag(self.style)

# end of class EditTextCtrl


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditTextCtrl objects.
    """
    name = 'text_ctrl_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'text_ctrl_%d' % number[0]
    text = EditTextCtrl(name, parent, wxNewId(), sizer, pos,
                        common.property_panel)
    node = Tree.Node(text)
    text.node = node
    text.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos-1)

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory function to build EditTextCtrl objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    text = EditTextCtrl(name, parent, wxNewId(), sizer, pos,
                        common.property_panel)
    sizer.set_item(text.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border)
    node = Tree.Node(text)
    text.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return text

def code_generator(obj):
    """\
    function that generates python code for wxTextCtrl objects.
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    value = '"' + prop.get('value', '').replace('"', '\"') + '"'
    if obj.is_toplevel:
        l = ['self.%s = %s(self, %s, %s)\n' % (obj.name, obj.klass, id, value)]
        if id_name: l.append(id_name) # init lines are written in reverse order
        return l , [], []
    size = pygen.generate_code_size(obj)
    style = prop.get('style', '0')
    if not style: style = '0'
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    init = ['self.%s = wxTextCtrl(%s, %s, %s, size=%s,' \
            ' style=%s)\n' % (obj.name, parent, id, value, size, style)]
    if id_name: init.append(id_name)
    props_buf = []
    if prop.has_key('foreground'):
        props_buf.append(pygen.generate_code_foreground(obj))
    if prop.has_key('background'):
        props_buf.append(pygen.generate_code_background(obj))
    if prop.has_key('font'):
        props_buf.append(pygen.generate_code_font(obj))
    return init, props_buf, []


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditTextCtrl'] = builder
    common.widgets_from_xml['EditTextCtrl'] = xml_builder
    common.class_names['EditTextCtrl'] = 'wxTextCtrl'
    
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxTextCtrl', code_generator)
        
    return common.make_object_button('EditTextCtrl', 'icons/text_ctrl.xpm')
