# choice.py: wxChoice objects
# $Id: choice.py,v 1.15 2005/05/06 21:48:22 agriggio Exp $
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
    # On windows GetBestSize considers also the drop down menu, while we
    # don't want it to be included.
    class wxChoice2(wxChoice):
        def GetBestSize(self):
            w, h = wxChoice.GetBestSize(self)
            n = self.GetCount()
            return w, h/(n+1)
else:
    wxChoice2 = wxChoice


class EditChoice(ManagedBase):

    events = ['EVT_CHOICE']
    
    def __init__(self, name, parent, id, choices, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxChoice objects
        """
        import config
        ManagedBase.__init__(self, name, 'wxChoice', parent, id, sizer,
                             pos, property_window, show=show)
        self.choices = choices
        self.selection = 0
        
        self.access_functions['choices'] = (self.get_choices, self.set_choices)
        self.properties['choices'] = ChoicesProperty(self, 'choices', None,
                                                     [('Label',
                                                       GridProperty.STRING)],
                                                     len(choices))
        self.access_functions['selection'] = (self.get_selection,
                                              self.set_selection)
        self.properties['selection'] = SpinProperty(self, 'selection', None,
                                                    r=(0, len(choices)-1))
        # 2003-09-04 added default_border
        if config.preferences.default_border:
            self.border = config.preferences.default_border_size
            self.flag = wxALL

    def create_widget(self):
        self.widget = wxChoice2(self.parent.widget, self.id,
                               choices=self.choices)
        self.set_selection(self.selection)
        EVT_LEFT_DOWN(self.widget, self.on_set_focus)        

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        szr = wxBoxSizer(wxVERTICAL)
        self.properties['choices'].display(panel)
        self.properties['selection'].display(panel)
        szr.Add(self.properties['selection'].panel, 0, wxEXPAND)
        szr.Add(self.properties['choices'].panel, 1, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')
        self.properties['choices'].set_col_sizes([-1])

    def get_choices(self):
        return zip(self.choices)

    def set_choices(self, values):
        self.choices = [ misc.wxstr(v[0]) for v in values ]
        self.properties['selection'].set_range(0, len(self.choices)-1)
        if self.widget:
            self.widget.Clear()
            for c in self.choices:
                self.widget.Append(c)
            if not self.properties['size'].is_active():
                self.sizer.set_item(self.pos, size=self.widget.GetBestSize())
            self.widget.SetSelection(
                int(self.properties['selection'].get_value()))

    def get_property_handler(self, prop_name):
        if prop_name == 'choices':
            return ChoicesHandler(self)
        return ManagedBase.get_property_handler(self, prop_name)

    def get_selection(self):
        return self.selection

    def set_selection(self, value):
        value = int(value)
        if value != self.selection:
            self.selection = value
            if self.widget: self.widget.SetSelection(value)

# end of class EditChoice

        
def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditChoice objects.
    """
    name = 'choice_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'choice_%d' % number[0]
    choice = EditChoice(name, parent, wxNewId(), [],
                        #[misc._encode('choice 1')],
                        sizer, pos, common.property_panel)
    node = Tree.Node(choice)
    #sizer.set_item(pos, size=choice.GetBestSize())
    choice.node = node
    choice.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos-1)

def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditChoice objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: name = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    choice = EditChoice(name, parent, wxNewId(), [], sizer, pos,
                        common.property_panel) #, show=False)
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
    common.widgets['EditChoice'] = builder
    common.widgets_from_xml['EditChoice'] = xml_builder

    return common.make_object_button('EditChoice', 'icons/choice.xpm')
