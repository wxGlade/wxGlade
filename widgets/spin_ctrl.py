# spin_ctrl.py: wxSpinCtrl objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
from edit_windows import ManagedBase
from tree import Tree
import common, misc
from widget_properties import *

class EditSpinCtrl(ManagedBase):
    """\
    Class to handle wxSpinCtrl objects
    """
    def __init__(self, name, parent, id, sizer, pos, property_window,
                 show=True):
        ManagedBase.__init__(self, name, 'wxSpinCtrl', parent, id, sizer, pos,
                             property_window, show=show)

        prop = self.properties
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.access_functions['value'] = (self.get_value, self.set_value)
        self.access_functions['range'] = (self.get_range, self.set_range)
        style_labels = ('#section#Style', 'wxSP_ARROW_KEYS', 'wxSP_WRAP')
        self.style_pos = (wxSP_ARROW_KEYS, wxSP_WRAP)
        prop['style'] = CheckListProperty(self, 'style', None, style_labels)
        prop['range'] = TextProperty(self, 'range', None, can_disable=True)
        prop['value'] = SpinProperty(self, 'value', None, can_disable=True)

        self.style = style
        self.value = 0
        self.range = (0, 100) # Default values in wxSpinCtrl constructor.

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxScrolledWindow(self.notebook, -1)
        szr = misc.Sizer(wxVERTICAL)
        prop = self.properties
        prop['range'].display(panel)
        prop['value'].display(panel)
        prop['style'].display(panel)
        szr.Add(prop['range'].panel, 0, wxEXPAND)
        szr.Add(prop['value'].panel, 0, wxEXPAND)
        szr.Add(prop['style'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')        

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
        if self.widget:
            self.SetWindowStyleFlag(style)

    def get_range(self):
        return self.range

    def set_range(self, val):
        try: min_v, max_v = map(int, val.split(','))
        except: self.properties['range'].set_value('%s, %s', self.get_range())
        # !!!
        self.set_range(min_v, max_v)
        self.properties['value'].set_range(min_v, max_v)

    def get_value(self):
        return self.value

    def set_value(self, value):
        value = int(value)
        if self.value != value:
            self.value = value
            if self.widget:
                self.widget.SetValue(self.value)

    def create_widget(self):
        self.widget = wxSpinCtrl(self.parent, self.id, min=self.range[0],
                                 max=self.range[1], initial=self.value)

# end of class EditSpinCtrl


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditSpinCtrl objects.
    """
    name = 'spin_ctrl_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'spin_ctrl_%d' % number[0]
    text = EditSpinCtrl(name, parent, wxNewId(), sizer, pos,
                        common.property_panel)
    node = Tree.Node(text)
    text.node = node
    common.app_tree.insert(node, sizer.node, pos-1)

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory function to build EditSpinCtrl objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    text = EditSpinCtrl(name, parent, wxNewId(), sizer, pos,
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
    function that generates python code for wxSpinCtrl objects.
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    value = prop.get('value', '0')
    try: min_v, max_v = [ s.strip() for s in \
                          prop.get('range', '0, 100').split(',') ]
    except: min_v, max_v = '0', '100'
    
    if obj.is_toplevel:
        l = ['self.%s = %s(self, %s, min=%s, max=%s, initial=%s)\n' % \
             (obj.name, obj.klass, id, min_v, max_v, value)]
        if id_name: l.append(id_name) # init lines are written in reverse order
        return l , [], []
    size = pygen.generate_code_size(obj)
    style = prop.get('style', '0')
    if not style: style = '0'
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    init = ['self.%s = wxSpinCtrl(%s, %s, min=%s, max=%s, initial=%s, ' \
            'size=%s, style=%s)\n' % (obj.name, parent, id, min_v, max_v,
                                      value, size, style)]
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
    common.widgets['EditSpinCtrl'] = builder
    common.widgets_from_xml['EditSpinCtrl'] = xml_builder
    common.class_names['EditSpinCtrl'] = 'wxSpinCtrl'
    
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxSpinCtrl', code_generator)
        
    return common.make_object_button('EditSpinCtrl', 'icons/spin_ctrl.xpm')
