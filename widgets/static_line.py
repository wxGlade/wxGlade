# static_line.py: wxStaticLine objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
import common
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *

class EditStaticLine(wxStaticLine, ManagedBase):
    def __init__(self, name, parent, id, orientation, sizer, pos,
                 property_window, show=True):
        """\
        Class to handle wxStaticLine objects
        """
        wxStaticLine.__init__(self, parent, id, style=orientation)
        self.orientation = orientation
        ManagedBase.__init__(self, name, 'wxStaticLine', parent, id, sizer,
                             pos, property_window, show=show)
        self.sel_marker.Reparent(parent)
        od = { wxLI_HORIZONTAL: 'wxLI_HORIZONTAL',
               wxLI_VERTICAL: 'wxLI_VERTICAL' }
        self.properties['style'] = HiddenProperty(self, 'style',
                                                  od.get(orientation,
                                                         'wxLI_HORIZONTAL'))
        self.removed_p = self.properties['font']
        del self.properties['font']
        EVT_LEFT_DOWN(self, self.on_set_focus)

    def create_properties(self):
        ManagedBase.create_properties(self)
        if self.removed_p.panel: self.removed_p.panel.Hide()

    def __getitem__(self, key):
        if key != 'font': return ManagedBase.__getitem__(self, key)
        return (lambda : "", lambda v: None)

# end of class EditStaticLine
        
def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditStaticLine objects.
    """
    class Dialog(wxDialog):
        def __init__(self):
            wxDialog.__init__(self, None, -1, 'Select orientation')
            self.orientations = [ wxLI_HORIZONTAL, wxLI_VERTICAL ]
            self.orientation = wxLI_HORIZONTAL
            prop = RadioProperty(self, 'orientation', self,
                                 ['wxLI_HORIZONTAL', 'wxLI_VERTICAL'])
            szr = wxBoxSizer(wxVERTICAL)
            szr.Add(prop.panel, 0, wxEXPAND)
            szr.Add(wxButton(self, wxID_OK, 'OK'), 0, wxALL|wxALIGN_CENTER, 3)
            self.SetAutoLayout(True)
            self.SetSizer(szr)
            szr.Fit(self)
        def __getitem__(self, value):
            def set_orientation(o): self.orientation = self.orientations[o]
            return (lambda: self.orientation, set_orientation)
    # end of inner class

    dialog = Dialog()
    dialog.ShowModal()
    
    label = 'static_line_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'static_line_%d' % number[0]
    static_line = EditStaticLine(label, parent, wxNewId(), dialog.orientation,
                                 sizer, pos, common.property_panel)
    node = Tree.Node(static_line)
    static_line.node = node
    common.app_tree.insert(node, sizer.node, pos-1) 
    sizer.set_item(pos, flag=wxEXPAND)
    static_line.sizer_properties['flag'].set_value("wxEXPAND")


def xml_builder(attrs, parent, sizer, sizeritem, pos=None, complete=False,
                tmp_line=[None]):
    """\
    factory to build EditStaticLine objects from an xml file
    """
    class FakeLine:
        orientations = { 'wxLI_HORIZONTAL': wxLI_HORIZONTAL,
                         'wxLI_VERTICAL': wxLI_VERTICAL }
        def __init__(self, attrs, parent, sizer, sizeritem, pos):
            self.attrs = attrs
            self.parent = parent
            self.sizer = sizer
            self.sizeritem = sizeritem
            self.pos = pos
        def __getitem__(self, value):
            if value != 'style': raise KeyError
            def set_orient(val):
                self.orient = FakeLine.orientations[val]
                return xml_builder(self.attrs, self.parent, self.sizer,
                                   self.sizeritem, self.pos, True)
            return (None, set_orient)
    # end of class FakeLine

    if not complete:
        tmp_line[0] = FakeLine(attrs, parent, sizer, sizeritem, pos)
        return tmp_line[0]
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    orientation = tmp_line[0].orient    
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    static_line = EditStaticLine(name, parent, wxNewId(), orientation, sizer,
                                 pos, common.property_panel)
    # see if the static line is an instance of a custom class,
    # and set its klass property 
    if hasattr(tmp_line[0], 'klass'):
        static_line.klass = tmp_line[0].klass
        static_line.klass_prop.set_value(static_line.klass)
        
    sizer.set_item(static_line.pos, option=sizeritem.option,
                   flag=sizeritem.flag, border=sizeritem.border,
                   size=static_line.GetBestSize())
    node = Tree.Node(static_line)
    static_line.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return static_line

def code_generator(obj):
    """\
    generates the python code for wxStaticLine objects
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    if obj.is_toplevel:
        l = ['self.%s = %s(self, %s)\n' % (obj.name, obj.klass, id)]
        if id_name: l.append(id_name)
        return l, [], []
    size = pygen.generate_code_size(obj)
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    style = prop.get("style", "wxLI_HORIZONTAL")
    if not style: style = "wxLI_HORIZONTAL"
    init = ['self.%s = wxStaticLine(%s, %s, size=%s, style=%s)\n' %
            (obj.name, parent, id, size, style) ]
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
    common.widgets['EditStaticLine'] = builder
    common.widgets_from_xml['EditStaticLine'] = xml_builder
    common.class_names['EditStaticLine'] = 'wxStaticLine'

    # python code generation functions
    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxStaticLine', code_generator)
    
    return common.make_object_button('EditStaticLine', 'icons/static_line.xpm')
