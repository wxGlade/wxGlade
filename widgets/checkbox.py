# checkbox.py: wxCheckBox objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
import common, misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *

class EditCheckBox(ManagedBase):
    def __init__(self, name, parent, id, label, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxCheckBox objects
        """
        ManagedBase.__init__(self, name, 'wxCheckBox', parent, id, sizer,
                             pos, property_window, show=show)
        self.label = label
        self.value = 0 # if nonzero, che checkbox is checked

        self.access_functions['label'] = (self.get_label, self.set_label)
        self.access_functions['checked'] = (self.get_value, self.set_value)
        self.properties['label'] = TextProperty(self, 'label', None)
        self.properties['checked'] = CheckBoxProperty(self, 'checked', None,
                                                      'Checked')

    def create_widget(self):
        self.widget = wxCheckBox(self.parent.widget, self.id, self.label)
        self.widget.SetValue(self.value)
        def on_checkbox(event):
            self.set_value(self.value)
        EVT_CHECKBOX(self.widget, self.id, on_checkbox)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        self.properties['label'].display(panel)
        self.properties['checked'].display(panel)
        szr = wxBoxSizer(wxVERTICAL)
        szr.Add(self.properties['label'].panel, 0, wxEXPAND)
        szr.Add(self.properties['checked'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def get_label(self):
        return self.label

    def set_label(self, value):
        value = str(value)
        if value != self.label:
            self.label = value
            if self.widget:
                self.widget.SetLabel(self.label)

    def get_value(self):
        return self.value

    def set_value(self, value):
        self.value = int(value)
        if self.widget:
            self.widget.SetValue(self.value)
            self.sizer.set_item(self.pos, size=self.widget.GetBestSize())

# end of class EditCheckBox
   
        
def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditCheckBox objects.
    """
    label = 'checkbox_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'checkbox_%d' % number[0]
    checkbox = EditCheckBox(label, parent, wxNewId(), label, sizer, pos,
                            common.property_panel)
    node = Tree.Node(checkbox)
    checkbox.node = node
    checkbox.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos-1)

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditCheckBox objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: label = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    checkbox = EditCheckBox(label, parent, wxNewId(),
                            misc._encode(label), sizer, pos,
                            common.property_panel, show=False) 
    sizer.set_item(checkbox.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border) #,
##                   size=checkbox.GetBestSize())
    node = Tree.Node(checkbox)
    checkbox.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return checkbox

def code_generator(obj):
    """\
    generates the python code for wxCheckBox objects
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    label = prop.get('label', '').replace('"', '\"')
    if obj.is_toplevel:
        l = ['self.%s = %s(self, %s, "%s")\n' %
             (obj.name, obj.klass, id, label)]
        if id_name: l.append(id_name)
        return l, [], []
    size = pygen.generate_code_size(obj)
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    style = prop.get("style", "0")
    if not style: style = "0"
    init = ['self.%s = wxCheckBox(%s, %s, "%s", size=%s, style=%s)\n' %
            (obj.name, parent, id, label, size, style) ]
    if id_name: init.append(id_name)
    props_buf = []
    checked = prop.get('checked')
    if checked: props_buf.append('self.%s.SetValue(1)\n' % obj.name)
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
    common.widgets['EditCheckBox'] = builder
    common.widgets_from_xml['EditCheckBox'] = xml_builder
    common.class_names['EditCheckBox'] = 'wxCheckBox'

    # python code generation functions
    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxCheckBox', code_generator)
    
    return common.make_object_button('EditCheckBox', 'icons/checkbox.xpm')
