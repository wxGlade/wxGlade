# slider.py: wxSlider objects
# $Id: slider.py,v 1.10 2005/05/06 21:48:18 agriggio Exp $
#
# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
import common, misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *

class EditSlider(ManagedBase):

    events = [
        'EVT_COMMAND_SCROLL',
        'EVT_COMMAND_SCROLL_TOP',
        'EVT_COMMAND_SCROLL_BOTTOM',
        'EVT_COMMAND_SCROLL_LINEUP',
        'EVT_COMMAND_SCROLL_LINEDOWN',
        'EVT_COMMAND_SCROLL_PAGEUP',
        'EVT_COMMAND_SCROLL_PAGEDOWN',
        'EVT_COMMAND_SCROLL_THUMBTRACK',
        'EVT_COMMAND_SCROLL_THUMBRELEASE',
        'EVT_COMMAND_SCROLL_ENDSCROLL',
        ]
    
    def __init__(self, name, parent, id, style, sizer, pos,
                 property_window, show=True):
        """\
        Class to handle wxSlider objects
        """
        ManagedBase.__init__(self, name, 'wxSlider', parent, id, sizer,
                             pos, property_window, show=show)
        self.style = style
        self.value = 0
        self.range = (0, 10)

        prop = self.properties
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.access_functions['value'] = (self.get_value, self.set_value)
        self.access_functions['range'] = (self.get_range, self.set_range)
        style_labels = ('#section#Style', 'wxSL_HORIZONTAL', 'wxSL_VERTICAL',
                        'wxSL_AUTOTICKS', 'wxSL_LABELS', 'wxSL_LEFT',
                        'wxSL_RIGHT', 'wxSL_TOP')
        self.style_pos = (wxSL_HORIZONTAL, wxSL_VERTICAL,
                          wxSL_AUTOTICKS, wxSL_LABELS, wxSL_LEFT,
                          wxSL_RIGHT, wxSL_TOP)
        prop['style'] = CheckListProperty(self, 'style', None, style_labels)
        prop['range'] = TextProperty(self, 'range', None, can_disable=True)
        prop['value'] = SpinProperty(self, 'value', None, can_disable=True)

    def create_widget(self):
        self.widget = wxSlider(self.parent.widget, self.id, self.value,
                               self.range[0], self.range[1], style=self.style)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxScrolledWindow(self.notebook, -1, style=wxTAB_TRAVERSAL)
        prop = self.properties
        szr = wxBoxSizer(wxVERTICAL)
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
        if self.widget: self.widget.SetWindowStyleFlag(self.style)

    def get_range(self):
        return "%s, %s" % self.range

    def set_range(self, val):
        try: min_v, max_v = map(int, val.split(','))
        except: self.properties['range'].set_value(self.get_range())
        else: self.range = (min_v, max_v)
        self.properties['value'].set_range(min_v, max_v)
        if self.widget: self.widget.SetRange(min_v, max_v)

    def get_value(self):
        return self.value

    def set_value(self, value):
        value = int(value)
        if value != self.value:
            self.value = value
            if self.widget: self.widget.SetValue(value)

# end of class EditSlider

        
def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditStaticLine objects.
    """
    class Dialog(wxDialog):
        def __init__(self):
            wxDialog.__init__(self, None, -1, 'Select style')
            self.orientations = [ wxSL_HORIZONTAL, wxSL_VERTICAL ]
            self.orientation = wxSL_HORIZONTAL
            prop = RadioProperty(self, 'orientation', self,
                                 ['wxSL_HORIZONTAL', 'wxSL_VERTICAL'])
            szr = wxBoxSizer(wxVERTICAL)
            szr.Add(prop.panel, 0, wxALL|wxEXPAND, 10)
            style_labels = ('#section#', 'wxSL_AUTOTICKS', 'wxSL_LABELS',
                            'wxSL_LEFT', 'wxSL_RIGHT', 'wxSL_TOP')
            self.style_pos = (wxSL_AUTOTICKS, wxSL_LABELS, wxSL_LEFT,
                              wxSL_RIGHT, wxSL_TOP)
            self.style = 0
            self.style_prop = CheckListProperty(self, 'style', self,
                                                style_labels)
            szr.Add(self.style_prop.panel, 0, wxALL|wxEXPAND, 10)
            btn = wxButton(self, wxID_OK, 'OK')
            btn.SetDefault()
            szr.Add(btn, 0, wxBOTTOM|wxALIGN_CENTER, 10)
            self.SetAutoLayout(True)
            self.SetSizer(szr)
            szr.Fit(self)
            
        def __getitem__(self, value):
            if value == 'orientation':
                def set_orientation(o): self.orientation = self.orientations[o]
                return (lambda: self.orientation, set_orientation)
            else: return (self.get_style, self.set_style)
            
        def get_style(self):
            retval = [0] * len(self.style_pos)
            try:
                style = self.style
                for i in range(len(self.style_pos)):
                    if style & self.style_pos[i]:
                        retval[i] = 1
            except AttributeError: pass
            return retval

        def set_style(self, value):
            value = self.style_prop.prepare_value(value)
            style = 0
            for v in range(len(value)):
                if value[v]:
                    style |= self.style_pos[v]
            self.style = style

    # end of inner class

    dialog = Dialog()
    dialog.ShowModal()
    
    label = 'slider_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'slider_%d' % number[0]
    slider = EditSlider(label, parent, wxNewId(), dialog.orientation |
                        dialog.style, sizer, pos, common.property_panel)
    node = Tree.Node(slider)
    slider.node = node
    slider.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos-1) 


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditSlider objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    style = 0
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    slider = EditSlider(name, parent, wxNewId(), style, sizer,
                        pos, common.property_panel) 
    sizer.set_item(slider.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border)
    node = Tree.Node(slider)
    slider.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return slider
   

def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditSlider'] = builder
    common.widgets_from_xml['EditSlider'] = xml_builder
    
    return common.make_object_button('EditSlider', 'icons/slider.xpm')
