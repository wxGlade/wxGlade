# static_text.py: wxStaticText objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: Python 2.2 license (see license.txt)

from wxPython.wx import *
import common, misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *

class EditStaticText(ManagedBase):
    def __init__(self, name, parent, id, label, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxStaticText objects
        """
        ManagedBase.__init__(self, name, 'wxStaticText', parent, id, sizer,
                             pos, property_window, show=show)
        self.label = label
        self.style = 0
        # label property
        self.access_functions['label'] = (self.get_label, self.set_label)
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.properties['label'] = TextProperty(self, 'label', None,
                                                multiline=True)
        self.style_pos  = (wxALIGN_LEFT, wxALIGN_RIGHT, wxALIGN_CENTER,
                           wxST_NO_AUTORESIZE)
        style_labels = ('#section#Style', 'wxALIGN_LEFT', 'wxALIGN_RIGHT',
                        'wxALIGN_CENTER', 'wxST_NO_AUTORESIZE')
        self.properties['style'] = CheckListProperty(self, 'style', None,
                                                     style_labels)  

    def create_widget(self):
        self.widget = wxStaticText(self.parent.widget, self.id,
                                   self.label.replace('\\n', '\n'))

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        szr = wxBoxSizer(wxVERTICAL)
        self.properties['label'].display(panel)
        self.properties['style'].display(panel)
        szr.Add(self.properties['label'].panel, 0, wxEXPAND)
        szr.Add(self.properties['style'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def get_label(self): return self.label

    def set_label(self, value):
        if value != self.label:
            self.label = value
            if self.widget:
                self.widget.SetLabel(value.replace('\\n', '\n'))
                if not self.properties['size'].is_active():
                    self.sizer.set_item(self.pos,
                                        size=self.widget.GetBestSize())

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
        if self.widget: self.widget.SetWindowStyleFlag(self.style)

# end of class EditStaticText


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditStaticText objects.
    """
    label = 'label_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'label_%d' % number[0]
    static_text = EditStaticText(label, parent, wxNewId(),
                                 misc._encode(label), sizer, pos,
                                 common.property_panel)
    node = Tree.Node(static_text)
    static_text.node = node
    static_text.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos-1)

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditStaticText objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: label = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    static_text = EditStaticText(label, parent, wxNewId(),
                                 misc._encode(label), sizer, pos,
                                 common.property_panel)
    sizer.set_item(static_text.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border)
##                    size=static_text.GetBestSize())
    node = Tree.Node(static_text)
    static_text.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return static_text
    

def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditStaticText'] = builder
    common.widgets_from_xml['EditStaticText'] = xml_builder
    
    return common.make_object_button('EditStaticText', 'icons/static_text.xpm')
