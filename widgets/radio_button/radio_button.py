# radio_button.py: wxRadioButton objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
import common, misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *
from misc import wxGladeRadioButton

class EditRadioButton(ManagedBase):
    def __init__(self, name, parent, id, label, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxRadioButton objects
        """
        ManagedBase.__init__(self, name, 'wxRadioButton', parent, id, sizer,
                             pos, property_window, show=show)
        self.label = label
        self.value = 0 # if nonzero, che radio button is selected
        self.style = 0
        # label and checked properties
        self.access_functions['label'] = (self.get_label, self.set_label)
        self.access_functions['clicked'] = (self.get_value, self.set_value)
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['label'] = TextProperty(self, 'label', None)
        self.properties['clicked'] = CheckBoxProperty(self, 'clicked', None,
                                                      'Clicked')
        self.style_pos = [wxRB_GROUP]
        self.properties['style'] = CheckListProperty(self, 'style', None,
                                                     ['#section#Style',
                                                      'wxRB_GROUP'])

    def create_widget(self):
        self.widget = wxGladeRadioButton(self.parent.widget, self.id,
                                         self.label)
        EVT_CHECKBOX(self.widget, self.id,
                     lambda e: self.widget.SetValue(self.value))        

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        szr = wxBoxSizer(wxVERTICAL)
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

    def get_label(self): return self.label
    def get_value(self): return self.value

    def set_label(self, value):
        if value != self.label:
            self.label = value
            if self.widget:
                self.widget.SetLabel(str(value))
                if not self.properties['size'].is_active():
                    self.sizer.set_item(self.pos,
                                        size=self.widget.GetBestSize())

    def set_value(self, value):
        self.value = value
        if self.widget: self.widget.SetValue(self.value)
   
    def get_style(self):
        retval = [0] * len(self.style_pos)
        try:
            for i in range(len(self.style_pos)):
                if self.style & self.style_pos[i]:
                    retval[i] = 1
        except AttributeError:
            pass
        return retval

    def set_style(self, value):
        value = self.properties['style'].prepare_value(value)
        self.style = 0
        for v in range(len(value)):
            if value[v]:
                self.style |= self.style_pos[v]
        if self.widget: self.SetWindowStyleFlag(self.style)
        
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
    radio.show_widget(True)
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
    radio = EditRadioButton(label, parent, wxNewId(), "",
                            sizer, pos, common.property_panel)
    sizer.set_item(radio.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border)
##                    size=radio.GetBestSize())
    node = Tree.Node(radio)
    radio.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return radio
  

def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditRadioButton'] = builder
    common.widgets_from_xml['EditRadioButton'] = xml_builder

    return common.make_object_button('EditRadioButton',
                                     'icons/radio_button.xpm')
