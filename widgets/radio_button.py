# radio_button.py: wxRadioButton objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
import common, misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *
from misc import wxGladeRadioButton

class EditRadioButton(wxGladeRadioButton, ManagedBase):
    def __init__(self, name, parent, id, label, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxRadioButton objects
        """
        wxGladeRadioButton.__init__(self, parent, id, label)
        ManagedBase.__init__(self, name, 'wxRadioButton', parent, id, sizer,
                             pos, property_window, show=show)

        # label and checked properties
        self.access_functions['label'] = (self.GetLabel, self.set_label)
        self.access_functions['clicked'] = (lambda : self.GetValue(),
                                            self.set_value)
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['label'] = TextProperty(self, 'label', None)
        self.properties['clicked'] = CheckBoxProperty(self, 'clicked', None,
                                                      'Clicked')
        self.style_pos = [wxRB_GROUP]
        self.properties['style'] = CheckListProperty(self, 'style', None,
                                                     ['#section#Style',
                                                      'wxRB_GROUP'])
        self.old_label = label
        self.value = 0 # if nonzero, che radio button is selected
        EVT_CHECKBOX(self, id, lambda e: self.SetValue(self.value))

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        szr = misc.Sizer(wxVERTICAL)
        self.properties['label'].display(panel)
        self.properties['style'].display(panel)
        self.properties['clicked'].display(panel)
        szr.Add(self.properties['label'].panel, 0, wxEXPAND)
        szr.Add(self.properties['style'].panel, 0, wxEXPAND)
        szr.Add(self.properties['clicked'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def set_label(self, value):
        if value != self.old_label:
            self.SetLabel(str(value))
            self.old_label = value

    def set_value(self, value):
        self.value = value
        self.SetValue(self.value)
   
    def get_style(self):
        retval = [0] * len(self.style_pos)
        try:
            style = self.GetWindowStyleFlag()
            for i in range(len(self.style_pos)):
                if style & self.style_pos[i]:
                    retval[i] = 1
        except AttributeError:
            pass
        return retval

    def set_style(self, value):
        value = self.properties['style'].prepare_value(value)
        style = 0
        for v in range(len(value)):
            if value[v]:
                style |= self.style_pos[v]
        self.SetWindowStyleFlag(style)
        
def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditRadioButton objects.
    """
    label = 'radio_btn_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'radio_btn_%d' % number[0]
    radio = EditRadioButton(label, parent, wxNewId(), misc._encode(label),
                            sizer, pos, common.property_panel)
    node = Tree.Node(radio)
    radio.node = node
    common.app_tree.insert(node, sizer.node, pos-1)

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditRadioButton objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: label = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    radio = EditRadioButton(label, parent, wxNewId(), misc._encode(label),
                            sizer, pos, common.property_panel)
    sizer.set_item(radio.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border,
                   size=radio.GetBestSize())
    node = Tree.Node(radio)
    radio.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return radio

def code_generator(obj):
    """\
    generates the python code for wxRadioButton objects
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
    init = ['self.%s = wxRadioButton(%s, %s, "%s", size=%s, style=%s)\n' %
            (obj.name, parent, id, label, size, style) ]
    if id_name: init.append(id_name)
    props_buf = []
    clicked = prop.get('clicked')
    if clicked: props_buf.append('self.%s.SetValue(1)\n' % obj.name)
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
    common.widgets['EditRadioButton'] = builder
    common.widgets_from_xml['EditRadioButton'] = xml_builder
    common.class_names['EditRadioButton'] = 'wxRadioButton'

    # python code generation functions
    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxRadioButton', code_generator)

    return common.make_object_button('EditRadioButton',
                                     'icons/radio_button.xpm')
