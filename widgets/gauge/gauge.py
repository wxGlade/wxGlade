# gauge.py: wxGauge objects
# $Id: gauge.py,v 1.7 2005/05/06 21:48:20 agriggio Exp $
#
# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
import common, misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *

class EditGauge(ManagedBase):
    def __init__(self, name, parent, id, style, sizer, pos,
                 property_window, show=True):
        """\
        Class to handle wxGauge objects
        """
        ManagedBase.__init__(self, name, 'wxGauge', parent, id, sizer,
                             pos, property_window, show=show)
        self.style = style
        self.range = 10

        prop = self.properties
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.access_functions['range'] = (self.get_range, self.set_range)
        style_labels = ('#section#Style', 'wxGA_HORIZONTAL', 'wxGA_VERTICAL',
                        'wxGA_PROGRESSBAR', 'wxGA_SMOOTH')
        self.style_pos = (wxGA_HORIZONTAL, wxGA_VERTICAL,
                          wxGA_PROGRESSBAR, wxGA_SMOOTH)
        prop['style'] = CheckListProperty(self, 'style', None, style_labels)
        prop['range'] = SpinProperty(self, 'range', None)

    def create_widget(self):
        self.widget = wxGauge(self.parent.widget, self.id, self.range,
                              style=self.style)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxScrolledWindow(self.notebook, -1, style=wxTAB_TRAVERSAL)
        prop = self.properties
        szr = wxBoxSizer(wxVERTICAL)
        prop['range'].display(panel)
        prop['style'].display(panel)
        szr.Add(prop['range'].panel, 0, wxEXPAND)
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
        return self.range

    def set_range(self, val):
        self.range = int(val)
        self.properties['range'].set_value(self.range)
        if self.widget: self.widget.SetRange(self.range)

# end of class EditGauge

        
def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditStaticLine objects.
    """
    class Dialog(wxDialog):
        def __init__(self):
            wxDialog.__init__(self, None, -1, 'Select style')
            self.orientations = [ wxGA_HORIZONTAL, wxGA_VERTICAL ]
            self.orientation = wxGA_HORIZONTAL
            prop = RadioProperty(self, 'orientation', self,
                                 ['wxGA_HORIZONTAL', 'wxGA_VERTICAL'])
            szr = wxBoxSizer(wxVERTICAL)
            szr.Add(prop.panel, 0, wxALL|wxEXPAND, 10)
            style_labels = ('#section#', 'wxGA_PROGRESSBAR', 'wxGA_SMOOTH')
            self.style_pos = (wxGA_PROGRESSBAR, wxGA_SMOOTH)
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
    
    label = 'gauge_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'gauge_%d' % number[0]
    gauge = EditGauge(label, parent, wxNewId(), dialog.orientation |
                      dialog.style, sizer, pos, common.property_panel)
    node = Tree.Node(gauge)
    gauge.node = node
    gauge.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos-1) 


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditGauge objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    style = 0
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    gauge = EditGauge(name, parent, wxNewId(), style, sizer,
                      pos, common.property_panel) 
    sizer.set_item(gauge.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border)
    node = Tree.Node(gauge)
    gauge.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return gauge
   

def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditGauge'] = builder
    common.widgets_from_xml['EditGauge'] = xml_builder
    
    return common.make_object_button('EditGauge', 'icons/gauge.xpm')
