# panel.py: wxPanel objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
import common, misc
from tree import Tree
from widget_properties import *
from edit_windows import ManagedBase

class EditPanel(wxPanel, ManagedBase):
    def __init__(self, name, parent, id, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxPanel objects
        """
        wxPanel.__init__(self, parent, id)
        ManagedBase.__init__(self, name, 'wxPanel', parent, id, sizer,
                             pos, property_window, show=show)
        self.has_sizer = False
        # event handlers
        EVT_LEFT_DOWN(self, self.drop_sizer)
        EVT_ENTER_WINDOW(self, self.on_enter)
        EVT_SIZE(parent, self.on_parent_size)
        if not self.Disconnect(-1, -1, wxEVT_LEFT_DOWN):
            print "EditPanel: Unable to disconnect the event hanlder"

    def on_enter(self, event):
        if not self.has_sizer and common.adding_sizer:
            self.SetCursor(wxCROSS_CURSOR)
        else:
            self.SetCursor(wxNullCursor)

    def drop_sizer(self, event):
        if self.has_sizer or not common.adding_sizer:
            self.on_set_focus(event) # default behaviour: call show_properties
            return
        self.SetCursor(wxNullCursor)
        common.adding_widget = common.adding_sizer = 0
        common.widgets[common.widget_to_add](self, None, None)
        common.widget_to_add = None
        self.has_sizer = True
        common.app_tree.app.saved = False

    def on_parent_size(self, event):
        print 'on_parent_size'
        if self.has_sizer:
            self.GetSizer().Layout()

    def GetBestSize(self):
        if self.has_sizer: return self.GetSizer().GetMinSize()
        return wxPanel.GetBestSize(self)

# end of class EditPanel
        

def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditPanel objects.
    """
    name = 'panel_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'panel_%d' % number[0]
    panel = EditPanel(name, parent, wxNewId(), sizer, pos,
                      common.property_panel)
    node = Tree.Node(panel)
    panel.node = node
    common.app_tree.insert(node, sizer.node, pos-1)
    sizer.set_item(panel.pos, 1, wxEXPAND)
    panel.sizer_properties['option'].set_value(1)
    panel.sizer_properties['flag'].set_value("wxEXPAND")

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditPanel objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if not sizer or not sizeritem:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    panel = EditPanel(name, parent, wxNewId(), sizer, pos,
                      common.property_panel, True)
    sizer.set_item(panel.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border)
    node = Tree.Node(panel)
    panel.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return panel

def code_generator(panel):
    """\
    generates the python code for wxPanel objects
    """
    pygen = common.code_writers['python']
    prop = panel.properties
    id_name, id = pygen.generate_code_id(panel)
    if panel.is_toplevel:
        l = ['self.%s = %s(self, %s)\n' % (panel.name, panel.klass, id)]
        if id_name: l.append(id_name)
        return l, [], []
    size = pygen.generate_code_size(panel)
    if not panel.parent.is_toplevel: parent = 'self.%s' % panel.parent.name
    else: parent = 'self'
    init = ['self.%s = wxPanel(%s, %s, size=%s)\n' % (panel.name, parent, id,
                                                      size) ]
    if id_name: init.append(id_name)
    props_buf = []
    if prop.has_key('foreground'):
        props_buf.append(pygen.generate_code_foreground(panel))
    if prop.has_key('background'):
        props_buf.append(pygen.generate_code_background(panel))
    if prop.has_key('font'): props_buf.append(pygen.generate_code_font(panel))
    return init, props_buf, []


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditPanel'] = builder
    common.widgets_from_xml['EditPanel'] = xml_builder
    common.class_names['EditPanel'] = 'wxPanel'

    # python code generation functions
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxPanel', code_generator)
        
    return common.make_object_button('EditPanel', 'icons/panel.xpm')
    
