# slider.py: wxSlider objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
import common, misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *

class EditSlider(wxSlider, ManagedBase):
    def __init__(self, name, parent, id, style, sizer, pos,
                 property_window, show=True):
        """\
        Class to handle wxSlider objects
        """
        wxSlider.__init__(self, parent, id, 0, 0, 10, style=style)
        ManagedBase.__init__(self, name, 'wxSlider', parent, id, sizer,
                             pos, property_window, show=show)

        prop = self.properties
        self.access_functions['style'] = (self.get_style, self.set_style)
        self.access_functions['value'] = (self.GetValue, lambda v:
                                          self.SetValue(int(v)))
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

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxScrolledWindow(self.notebook, -1)
        prop = self.properties
        szr = misc.Sizer(wxVERTICAL)
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
            style = self.GetWindowStyleFlag()
            for i in range(len(self.style_pos)):
                if style & self.style_pos[i]:
                    retval[i] = 1
        except AttributeError: pass
        return retval

    def set_style(self, value):
        value = self.properties['style'].prepare_value(value)
        style = 0
        for v in range(len(value)):
            if value[v]:
                style |= self.style_pos[v]
        self.SetWindowStyleFlag(style)

    def get_range(self): return (self.GetMin(), self.GetMax())

    def set_range(self, val):
        try: min_v, max_v = map(int, val.split(','))
        except: self.properties['range'].set_value('%s, %s', self.get_range())
        self.SetRange(min_v, max_v)
        self.properties['value'].set_range(min_v, max_v)
        
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
            szr.Add(prop.panel, 0, wxEXPAND)
            style_labels = ('#section#', 'wxSL_AUTOTICKS', 'wxSL_LABELS',
                            'wxSL_LEFT', 'wxSL_RIGHT', 'wxSL_TOP')
            self.style_pos = (wxSL_AUTOTICKS, wxSL_LABELS, wxSL_LEFT,
                              wxSL_RIGHT, wxSL_TOP)
            self.style = 0
            self.style_prop = CheckListProperty(self, 'style', self,
                                                style_labels)
            szr.Add(self.style_prop.panel, 0, wxEXPAND)
            szr.Add(wxButton(self, wxID_OK, 'OK'), 0, wxALL|wxALIGN_CENTER, 3)
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
    common.app_tree.insert(node, sizer.node, pos-1) 


def xml_builder(attrs, parent, sizer, sizeritem, pos=None, complete=False,
                tmp_slider=[None]):
    """\
    factory to build EditSlider objects from an xml file
    """
    class FakeSlider:
        styles = { 'wxSL_HORIZONTAL': wxSL_HORIZONTAL,
                   'wxSL_VERTICAL': wxSL_VERTICAL,
                   'wxSL_AUTOTICKS': wxSL_AUTOTICKS,
                   'wxSL_LABELS': wxSL_LABELS,
                   'wxSL_LEFT': wxSL_LEFT,
                   'wxSL_RIGHT': wxSL_RIGHT,
                   'wxSL_TOP': wxSL_TOP                   
                   }
        def __init__(self, attrs, parent, sizer, sizeritem, pos):
            self.attrs = attrs
            self.parent = parent
            self.sizer = sizer
            self.sizeritem = sizeritem
            self.pos = pos
        def __getitem__(self, value):
            if value != 'style': raise KeyError
            def set_style(val):
                style_list = [ FakeSlider.styles[v] for v in val.split('|') ]
                self.style = reduce(lambda a, b: a|b, style_list)
                return xml_builder(self.attrs, self.parent, self.sizer,
                                   self.sizeritem, self.pos, True)
            return (None, set_style)
    # end of class FakeSlider

    if not complete:
        tmp_slider[0] = FakeSlider(attrs, parent, sizer, sizeritem, pos)
        return tmp_slider[0]
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    style = tmp_slider[0].style    
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    slider = EditSlider(name, parent, wxNewId(), style, sizer,
                        pos, common.property_panel) 
    # see if the slider is an instance of a custom class, and set its klass
    # property 
    if hasattr(tmp_slider[0], 'klass'):
        slider.klass = tmp_slider[0].klass
        slider.klass_prop.set_value(slider.klass)
    sizer.set_item(slider.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border,
                   size=slider.GetBestSize())
    node = Tree.Node(slider)
    slider.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return slider

def code_generator(obj):
    """\
    generates the python code for wxSlider objects
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    value = prop.get('value', '0')
    try: min_v, max_v = [ s.strip() for s in prop['range'].split() ]
    except: min_v, max_v = '0', '10'
    if obj.is_toplevel:
        l = ['self.%s = %s(self, %s, %s, %s, %s)\n' % \
             (obj.name, obj.klass, id, value, min_v, max_v)]
        if id_name: l.append(id_name)
        return l, [], []
    size = pygen.generate_code_size(obj)
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    style = prop.get("style", "wxSL_HORIZONTAL")
    if not style: style = "wxSL_HORIZONTAL"
    init = ['self.%s = wxSlider(%s, %s, %s, %s, %s, size=%s, style=%s)\n' %
            (obj.name, parent, id, value, min_v, max_v, size, style) ]
    if id_name: init.append(id_name)
    props_buf = []
    if prop.has_key('foreground'):
        props_buf.append(pygen.generate_code_foreground(obj))
    if prop.has_key('background'):
        props_buf.append(pygen.generate_code_background(obj))
    return init, props_buf, []
    

def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditSlider'] = builder
    common.widgets_from_xml['EditSlider'] = xml_builder
    common.class_names['EditSlider'] = 'wxSlider'

    # python code generation functions
    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxSlider', code_generator)
    
    return common.make_object_button('EditSlider', 'icons/slider.xpm')
