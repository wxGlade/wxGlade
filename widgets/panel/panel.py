# panel.py: wxPanel objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

from wxPython.wx import *
import common, misc
from tree import Tree
from widget_properties import *
from edit_windows import ManagedBase


class EditPanel(ManagedBase):
    def __init__(self, name, parent, id, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxPanel objects
        """
        ManagedBase.__init__(self, name, 'wxPanel', parent, id, sizer,
                             pos, property_window, show=show)
        self.top_sizer = None # sizer to handle the layout of children

    def create_widget(self):
        self.widget = wxPanel(self.parent.widget, self.id)
        EVT_ENTER_WINDOW(self.widget, self.on_enter)
        self.widget.GetBestSize = self.get_widget_best_size

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self)
        # this must be done here since ManagedBase.finish_widget_creation
        # normally sets EVT_LEFT_DOWN to update_wiew
        if not self.widget.Disconnect(-1, -1, wxEVT_LEFT_DOWN):
            print "EditPanel: Unable to disconnect the event hanlder"
        EVT_LEFT_DOWN(self.widget, self.drop_sizer)

        
    def on_enter(self, event):
        if not self.top_sizer and common.adding_sizer:
            self.widget.SetCursor(wxCROSS_CURSOR)
        else:
            self.widget.SetCursor(wxNullCursor)

    def set_sizer(self, sizer):
        self.top_sizer = sizer
        if self.top_sizer and self.top_sizer.widget and self.widget:
            self.widget.SetAutoLayout(True)
            self.widget.SetSizer(self.top_sizer.widget)
            self.widget.Layout()
            self.sizer.set_item(self.pos, size=self.widget.GetBestSize())

    def drop_sizer(self, event):
        if self.top_sizer or not common.adding_sizer:
            self.on_set_focus(event) # default behaviour: call show_properties
            return
        self.widget.SetCursor(wxNullCursor)
        common.widgets[common.widget_to_add](self, None, None)
        common.adding_widget = common.adding_sizer = False
        common.widget_to_add = None
        common.app_tree.app.saved = False

    def get_widget_best_size(self):
        if self.top_sizer and self.widget.GetSizer():
            return self.widget.GetSizer().GetMinSize()
        return wxPanel.GetBestSize(self.widget)

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

    panel.set_option(1)
    panel.set_flag("wxEXPAND")
    panel.show_widget(True)

    common.app_tree.insert(node, sizer.node, pos-1)
    sizer.set_item(panel.pos, 1, wxEXPAND)
##     panel.sizer_properties['option'].set_value(1)
##     panel.sizer_properties['flag'].set_value("wxEXPAND")

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


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditPanel'] = builder
    common.widgets_from_xml['EditPanel'] = xml_builder
        
    return common.make_object_button('EditPanel', 'icons/panel.xpm')
    
