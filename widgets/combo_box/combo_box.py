# combo_box.py: wxComboBox objects
# $Id: combo_box.py,v 1.21 2005/05/06 21:48:22 agriggio Exp $
#
# Copyright (c) 2002-2005 Alberto Griggio <agriggio@users.sourceforge.net>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
import common, misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *

from ChoicesProperty import *

if wxPlatform == '__WXMSW__':
    # why on Windows combo boxes give segfaults? Need to investigate, but
    # for now replace them with choices
    # this seems to be because of the style of wxPanel: if there's a
    # wxTAB_TRAVERSAL, we have troubles -- now it should be fixed...
    class wxComboBox2(wxComboBox):
        # on windows GetBestSize considers also the drop down menu, while we
        # don't want it to be included
        def GetBestSize(self):
            w, h = wxComboBox.GetBestSize(self)
            n = self.GetCount()
            return w, h/(n+1)
else:
    wxComboBox2 = wxComboBox


class EditComboBox(ManagedBase):

    events = ['EVT_COMBOBOX', 'EVT_TEXT']
    
    def __init__(self, name, parent, id, choices, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxComboBox objects
        """
        import config
        ManagedBase.__init__(self, name, 'wxComboBox', parent, id, sizer,
                             pos, property_window, show=show)
        self.choices = choices
        if len(choices): self.selection = 0
        else: self.selection = -1
        self.style = 0
        # properties
        self.access_functions['choices'] = (self.get_choices, self.set_choices)
        self.access_functions['style'] = (self.get_style, self.set_style)
        style_labels = ('#section#Style', 'wxCB_SIMPLE',
                        'wxCB_READONLY', 'wxCB_SORT')
        self.style_pos = [ eval(s) for s in style_labels[1:] ]
        self.properties['style'] = CheckListProperty(self, 'style', None,
                                                     style_labels)
        self.properties['choices'] = ChoicesProperty(self, 'choices', None,
                                                     [('Label',
                                                       GridProperty.STRING)],
                                                     len(choices))
        self.access_functions['selection'] = (self.get_selection,
                                              self.set_selection)
        self.choices = list(choices)
        self.properties['selection'] = SpinProperty(self, 'selection', None,
                                                    r=(0, len(choices)-1))
        # 2003-09-04 added default_border
        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wxALL

    def create_widget(self):
        self.widget = wxComboBox2(self.parent.widget, self.id,
                                 choices=self.choices)
        self.set_selection(self.selection)
        EVT_LEFT_DOWN(self.widget, self.on_set_focus)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        szr = wxBoxSizer(wxVERTICAL)
        self.properties['choices'].display(panel)
        self.properties['style'].display(panel)
        self.properties['selection'].display(panel)
        szr.Add(self.properties['style'].panel, 0, wxEXPAND)
        szr.Add(self.properties['selection'].panel, 0, wxEXPAND)
        szr.Add(self.properties['choices'].panel, 1, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
        self.properties['choices'].set_col_sizes([-1])

    def get_selection(self):
        return self.selection

    def set_selection(self, value):
        value = int(value)
        if self.selection != value:
            self.selection = value
            if self.widget:
                self.widget.SetSelection(value)

    def get_choices(self):
        return zip(self.choices)

    def set_choices(self, values):
        self.choices = [ misc.wxstr(v[0]) for v in values ]
        self.properties['selection'].set_range(0, len(self.choices)-1)
        if self.widget:
            self.widget.Clear()
            for c in self.choices: self.widget.Append(c)
            if not self.properties['size'].is_active():
                self.sizer.set_item(self.pos, size=self.widget.GetBestSize())

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
##         if self.widget:
##             self.SetWindowStyleFlag(style)

    def create_widget(self):
        self.widget = wxComboBox2(self.parent.widget, self.id,
                                  choices=self.choices)
        EVT_LEFT_DOWN(self.widget, self.on_set_focus)

    def get_property_handler(self, prop_name):
        if prop_name == 'choices':
            return ChoicesHandler(self)
        return ManagedBase.get_property_handler(self, prop_name)

# end of class EditComboBox

        
def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditComboBox objects.
    """
    name = 'combo_box_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'combo_box_%d' % number[0]
    choice = EditComboBox(name, parent, wxNewId(), #[misc._encode('choice 1')],
                          [], sizer, pos, common.property_panel)
    node = Tree.Node(choice)
#    sizer.set_item(pos, size=choice.GetBestSize())
    choice.node = node
    choice.show_widget(True)
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
                   flag=sizeritem.flag, border=sizeritem.border)
##                    size=choice.GetBestSize())
    node = Tree.Node(choice)
    choice.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return choice

    

def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditComboBox'] = builder
    common.widgets_from_xml['EditComboBox'] = xml_builder

    return common.make_object_button('EditComboBox', 'icons/combo_box.xpm')
