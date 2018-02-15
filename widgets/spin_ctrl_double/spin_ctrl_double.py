"""\
wxSpinCtrlDouble objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
from tree import Node
import common, config
import new_properties as np


class EditSpinCtrlDouble(ManagedBase, EditStylesMixin):
    "Class to handle wxSpinCtrlDouble objects"
    # XXX unify with EditSpinButton?
    _PROPERTIES = ["Widget", "range", "value", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, id, sizer, pos):
        ManagedBase.__init__(self, name, 'wxSpinCtrlDouble', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.range = np.FloatRangePropertyA( "0.0, 100.0" )
        self.value = np.SpinDoublePropertyA(0, val_range=(0.0,100.0), immediate=True, default_value="")

    def create_widget(self):
        mi,ma = self.properties["range"].get_tuple()
        if self.properties["value"].is_active():
            self.widget = wx.SpinCtrlDouble(self.parent.widget, self.id, min=mi, max=ma, initial=self.value)
        else:
            self.widget = wx.SpinCtrlDouble(self.parent.widget, self.id, min=mi, max=ma)

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
                if self.widget:
                    self.widget.SetValue(value)

        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)


def builder(parent, sizer, pos, number=[1]):
    "factory function for EditSpinCtrl objects"
    name = 'spin_ctrl_double_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'spin_ctrl_double_%d' % number[0]
    with parent.frozen():
        spin = EditSpinCtrlDouble(name, parent, wx.NewId(), sizer, pos)
        spin.properties["style"].set_to_default()
        spin.check_defaults()
        node = Node(spin)
        spin.node = node
        if parent.widget: spin.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory function to build EditSpinCtrlDouble objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    spin = EditSpinCtrlDouble( name, parent, wx.NewId(), sizer, pos )
    spin.properties["value"].set_active(False)
    #sizer.set_item( spin.pos, proportion=sizeritem.proportion, flag=sizeritem.flag, border=sizeritem.border )
    node = Node(spin)
    spin.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return spin


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets['EditSpinCtrlDouble'] = builder
    common.widgets_from_xml['EditSpinCtrlDouble'] = xml_builder

    return common.make_object_button('EditSpinCtrlDouble', 'spin_ctrl_double.xpm')
