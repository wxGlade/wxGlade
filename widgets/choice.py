# choice.py: wxChoice objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

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
            n = self.Number()
            return w, h/(n+1)
else:
    wxChoice2 = wxChoice


class EditChoice(ManagedBase):
    def __init__(self, name, parent, id, choices, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxChoice objects
        """
        ManagedBase.__init__(self, name, 'wxChoice', parent, id, sizer,
                             pos, property_window, show=show)
        self.access_functions['choices'] = (self.get_choices, self.set_choices)
        self.properties['choices'] = ChoicesProperty(self, 'choices', None,
                                                     [('Label',
                                                       GridProperty.STRING)],
                                                     len(choices))
        self.access_functions['selection'] = (self.get_selection,
                                              self.set_selection)
        self.properties['selection'] = SpinProperty(self, 'selection', None,
                                                    r=(0, len(self.choices-1)))
        # self.choices must use a copy of choices.
        self.choices = list(choices)
        if self.choices:
            self.selection = 0
        else:
            self.selection = -1

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        szr = misc.Sizer(wxVERTICAL)
        self.properties['choices'].display(panel)
        self.properties['selection'].display(panel)
        self.properties['choices'].set_col_sizes([-1])
        szr.Add(self.properties['selection'].panel, 0, wxEXPAND)
        szr.Add(self.properties['choices'].panel, 1, wxEXPAND)
        panel.SetAutoLayout(True)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def get_choices(self):
        # A copy of self.choice is returned, otherwise the caller
        # could be able to change self.choice but not what is shown
        # by self.widget.
        return list(self.choices)

    def set_choices(self, values):
        self.choices = list(values)
        self.properties['selection'].set_range(0, len(self.choices)-1)
        if self.widget:
            self.widget.Clear()
            for value in values:
                # !!! I can't understand what you are doing,
                # why value[0]?
                self.widget.Append(value[0])
            self.sizer.set_item(self.pos, size=self.widget.GetBestSize())
            self.widget.SetSelection(int(self.properties['selection'].get_value()))

    def get_selection(self):
        return self.selection

    def set_selection(self, value):
        value = int(value)
        if value != self.selection:
            self.selection = value
            if self.widget:
                self.widget.SetSelection(value)

    def get_property_handler(self, prop_name):
        if prop_name == 'choices':
            return ChoicesHandler(self)

    def create_widget(self):
        self.widget = wxChoice2(self.parent, self.id, choices=self.choices)
        if self.choices:
            self.widget.SetSelection(0)
        EVT_LEFT_DOWN(self.widget, self.on_set_focus)

# end of class EditChoice

        
def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditChoice objects.
    """
    name = 'choice_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'choice_%d' % number[0]
    choice = EditChoice(name, parent, wxNewId(), [misc._encode('choice 1')],
                        sizer, pos, common.property_panel)
    node = Tree.Node(choice)
    sizer.set_item(pos, size=choice.GetBestSize())
    choice.node = node
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
                   flag=sizeritem.flag, border=sizeritem.border,
                   size=choice.GetBestSize())
    node = Tree.Node(choice)
    choice.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return choice

def code_generator(obj):
    """\
    generates the python code for wxChoice objects
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
    style = prop.get("style", "0")
    init = ['self.%s = wxChoice(%s, %s, choices=%s, size=%s, style=%s)\n' %
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
    common.widgets['EditChoice'] = builder
    common.widgets_from_xml['EditChoice'] = xml_builder
    common.class_names['EditChoice'] = 'wxChoice'

    # python code generation functions
    pygen = common.code_writers.get("python")
    if pygen:
        pygen.add_widget_handler('wxChoice', code_generator)
        pygen.add_property_handler('choices', ChoicesCodeHandler)

    return common.make_object_button('EditChoice', 'icons/choice.xpm')
