# combo_box.py: wxComboBox objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
import common, misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *

from ChoicesProperty import *

class EditComboBox(wxComboBox, ManagedBase):
    def __init__(self, name, parent, id, choices, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxComboBox objects
        """
        wxComboBox.__init__(self, parent, id, choices=choices)
        ManagedBase.__init__(self, name, 'wxComboBox', parent, id, sizer,
                             pos, property_window, show=show)
        # properties
        self.access_functions['choices'] = (self.get_choices, self.set_choices)
        self.access_functions['style'] = (self.get_style, self.set_style)
        style_labels = ('#section#Style', 'wxCB_READONLY', 'wxCB_SORT')
        self.style = 0
        self.style_pos = [ eval(s) for s in style_labels[1:] ]
        self.properties['style'] = CheckListProperty(self, 'style', None,
                                                     style_labels)
        self.properties['choices'] = ChoicesProperty(self, 'choices', None,
                                                     [('Label',
                                                       GridProperty.STRING)],
                                                     len(choices))
        self.access_functions['selection'] = (self.GetSelection,
                                              self.set_selection)
        self.properties['selection'] = SpinProperty(self, 'selection', None,
                                                    r=(0, self.Number()-1))
        EVT_LEFT_DOWN(self, self.on_set_focus)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        szr = misc.Sizer(wxVERTICAL)
        self.properties['choices'].display(panel)
        self.properties['style'].display(panel)
        self.properties['selection'].display(panel)
        self.properties['choices'].set_col_sizes([-1])
        szr.Add(self.properties['style'].panel, 0, wxEXPAND)
        szr.Add(self.properties['selection'].panel, 0, wxEXPAND)
        szr.Add(self.properties['choices'].panel, 1, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def set_selection(self, val):
        self.SetSelection(int(val))

    def get_choices(self):
        return zip([ self.GetString(i) for i in range(self.Number()) ])

    def set_choices(self, values):
        self.Clear()
        for value in values:
            self.Append(value[0])
        self.sizer.set_item(self.pos, size=self.GetBestSize())
        self.properties['selection'].set_range(0, self.Number()-1)

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
        value = self.properties['style'].prepare_value(value)
        style = 0
        for v in range(len(value)):
            if value[v]:
                style |= self.style_pos[v]
        self.style = style

    def get_property_handler(self, prop_name):
        if prop_name == 'choices':
            return ChoicesHandler(self)

    if wxPlatform == '__WXMSW__':
        # on windows GetBestSize considers also the drop down menu, while we
        # don't want it to be included
        def GetBestSize(self):
            w, h = wxComboBox.GetBestSize(self)
            n = self.Number()
            return w, h/(n+1)

# end of class EditComboBox

        
def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditComboBox objects.
    """
    name = 'combo_box_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'combo_box_%d' % number[0]
    choice = EditComboBox(name, parent, wxNewId(), [misc._encode('choice 1')],
                          sizer, pos, common.property_panel)
    node = Tree.Node(choice)
    sizer.set_item(pos, size=choice.GetBestSize())
    choice.node = node
    common.app_tree.insert(node, sizer.node, pos-1)

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditComboBox objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    choice = EditComboBox(name, parent, wxNewId(), [], sizer, pos,
                        common.property_panel)
    sizer.set_item(choice.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border,
                   size=choice.GetBestSize())
    node = Tree.Node(choice)
    choice.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return choice

def code_generator(obj):
    """\
    generates the python code for wxComboBox objects
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    choices = prop.get('choices', [])
    if obj.is_toplevel:
        l = ['self.%s = %s(self, %s, choices=%s)\n' % (obj.name, obj.klass,
                                                       id, repr(choices))]
        if id_name: l.append(id_name)
        return l, [], []
    size = pygen.generate_code_size(obj)
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    style = prop.get("style", None)
    if not style: style = 'wxCB_DROPDOWN'
    else: style = 'wxCB_DROPDOWN|' + style
    init = ['self.%s = wxComboBox(%s, %s, choices=%s, size=%s, style=%s)\n' %
            (obj.name, parent, id, repr(choices), size, style) ]
    if id_name: init.append(id_name)
    props_buf = []
    selection = prop.get('selection')
    if selection is not None:
        props_buf.append('self.%s.SetSelection(%s)\n' % (obj.name, selection))
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
    common.widgets['EditComboBox'] = builder
    common.widgets_from_xml['EditComboBox'] = xml_builder
    common.class_names['EditComboBox'] = 'wxComboBox'

    # python code generation functions
    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxComboBox', code_generator)
        pygen.add_property_handler('choices', ChoicesCodeHandler)

    return common.make_object_button('EditComboBox', 'icons/combo_box.xpm')
