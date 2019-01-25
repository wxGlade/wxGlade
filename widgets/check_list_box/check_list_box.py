"""\
wxCheckListBox objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@copyright: 2015 Franco Bugnano
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common
from edit_windows import ManagedBase, EditStylesMixin
from tree import Node
import new_properties as np
from ChoicesProperty import *


class EditCheckListBox(ManagedBase, EditStylesMixin):
    "Class to handle wxCheckListBox objects"
    # XXX maybe inherit from EditChoice
    _PROPERTIES = ["Widget", "style", "selection", "choices"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, id, choices, sizer, pos):
        ManagedBase.__init__(self, name, 'wxCheckListBox', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.selection = np.SpinProperty(-1, val_range=len(choices)-1, default_value=-1, immediate=True )
        self.choices   = ChoicesProperty( choices, [(_('Label'), np.GridProperty.STRING)] )

    def create_widget(self):
        choices = [c[0] for c in self.choices]
        self.widget = wx.CheckListBox(self.parent.widget, self.id, choices=choices)
        self.widget.SetSelection(self.selection)
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)

    def get_property_handler(self, prop_name):
        if prop_name == 'choices':
            return ChoicesHandler(self)
        return ManagedBase.get_property_handler(self, prop_name)

    def properties_changed(self, modified):
        # self.selection needs to be in range (-1,len(self.choices))
        choices = self.choices
        max_selection = len(choices)-1
        set_selection = False
        if not modified or "choices" in modified:
            # adjust range of selection
            self.properties['selection'].set_range(-1, max_selection)
            if self.selection>max_selection:
                set_selection = True
            if self.widget:
                # update widget
                self.widget.Clear()
                for c in choices: self.widget.Append(c[0])
                if not self.properties['size'].is_active():
                    self.sizer.set_item_best_size(self, size=self.widget.GetBestSize())

        if not modified or "selection" in modified or set_selection:
            if self.selection>max_selection:
                self.properties['selection'].set(max_selection)
            set_selection = True

        if self.widget and set_selection:
            self.widget.SetSelection(self.selection)  # -1 is identical to wx.NOT_FOUND

        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)



def builder(parent, sizer, pos, number=[1]):
    "factory function for EditCheckListBox objects"
    name = 'check_list_box_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'check_list_box_%d' % number[0]
    with parent.frozen():
        check_list_box = EditCheckListBox(name, parent, wx.NewId(), [[u'choice 1'],], sizer, pos)
        check_list_box.properties["style"].set_to_default()
        node = Node(check_list_box)
    ##     sizer.set_item(pos, size=check_list_box.GetBestSize())
        check_list_box.node = node
        if parent.widget: check_list_box.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditCheckListBox objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    check_list_box = EditCheckListBox(name, parent, wx.NewId(), [], sizer, pos)
    #sizer.set_item(check_list_box.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(check_list_box)
    check_list_box.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return check_list_box


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditCheckListBox'] = builder
    common.widgets_from_xml['EditCheckListBox'] = xml_builder

    return common.make_object_button('EditCheckListBox', 'list_box.xpm')
