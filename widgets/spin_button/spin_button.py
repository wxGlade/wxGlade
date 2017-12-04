"""\
wxSpinButton objects

based on wxGlade/widgets/spin_ctrl/

@copyright: 2004 D.H. aka crazyinsomniac at users.sourceforge.net
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
from tree import Node
import common, config
import new_properties as np


class EditSpinButton(ManagedBase, EditStylesMixin):
    "Class to handle wxSpinButton objects"
    # XXX unify with EditSpinCtrl?
    _PROPERTIES = ["Widget", "range", "value", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    recreate_on_style_change = True

    def __init__(self, name, parent, id, sizer, pos):
        ManagedBase.__init__(self, name, 'wxSpinButton', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.range = np.IntRangePropertyA( "0, 100" )
        self.value = np.SpinPropertyA(0, val_range=(0,100), immediate=True)

    def create_widget(self):
        self.widget = wx.SpinButton(self.parent.widget, self.id, style=self.style)
        self.widget.SetRange( *self.properties["range"].get_tuple() )
        self.widget.SetValue( self.value )

    def properties_changed(self, modified):  # from EditSlider
        if not modified or "range" in modified and self.widget:
            mi,ma = self.properties["range"].get_tuple()
            self.widget.SetRange(mi, ma)
            self.properties["value"].set_range(mi,ma)

        if not modified or "value" in modified or "range" in modified:
            # check that value is inside range
            value_p = self.properties["value"]
            if value_p.is_active():
                mi,ma = self.properties["range"].get_tuple()
                value = value_p.get()
                if value<mi:
                    value_p.set(mi)
                    value = mi
                elif value>ma:
                    value_p.set(ma)
                    value = ma
                if self.widget: self.widget.SetValue(value)

        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)



def builder(parent, sizer, pos, number=[1]):
    "factory function for EditSpinButton objects"
    name = 'spin_button_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'spin_button_%d' % number[0]
    with parent.frozen():
        text = EditSpinButton(name, parent, wx.NewId(), sizer, pos)
        text.properties["style"].set_to_default()
        text.check_defaults()
        node = Node(text)
        text.node = node
        if parent.widget: text.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory function to build EditSpinButton objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    text = EditSpinButton(name, parent, wx.NewId(), sizer, pos)
    #sizer.set_item(text.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(text)
    text.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return text


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditSpinButton'] = builder
    common.widgets_from_xml['EditSpinButton'] = xml_builder

    return common.make_object_button('EditSpinButton', 'spinbtn.xpm')
