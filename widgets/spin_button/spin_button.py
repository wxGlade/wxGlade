"""\
wxSpinButton objects

based on wxGlade/widgets/spin_ctrl/

@copyright: 2004 D.H. aka crazyinsomniac at users.sourceforge.net
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
import common, config
import new_properties as np


class EditSpinButton(ManagedBase, EditStylesMixin):
    "Class to handle wxSpinButton objects"
    # XXX unify with EditSpinCtrl?
    WX_CLASS = 'wxSpinButton'
    _PROPERTIES = ["Widget", "range", "value", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    recreate_on_style_change = True

    def __init__(self, name, parent, index, style='wxSP_VERTICAL'):
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self, style)

        # initialise instance properties
        self.range = np.IntRangePropertyA( "0, 100" )
        self.value = np.SpinPropertyA(0, val_range=(0,100), immediate=True)

    def create_widget(self):
        self.widget = wx.SpinButton(self.parent_window.widget, wx.ID_ANY, style=self.style)
        self.widget.SetRange( *self.properties["range"].get_tuple() )
        self.widget.SetValue( self.value )

    def _properties_changed(self, modified, actions):  # from EditSlider
        if not modified or "range" in modified and self.widget:
            mi,ma = self.properties["range"].get_tuple()
            self.widget.SetRange(mi, ma)
            self.properties["value"].set_range(mi,ma)

        if not modified or "value" in modified or "range" in modified:
            # check that value is inside range
            value_p = self.properties["value"]
            if common.history: common.history.monitor_property( value_p )
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

        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)


def builder(parent, index):
    "factory function for EditSpinButton objects"
    name = parent.toplevel_parent.get_next_contained_name('spin_button_%d')
    with parent.frozen():
        editor = EditSpinButton(name, parent, index)
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory function to build EditSpinButton objects from a XML file"
    return EditSpinButton(name, parent, index, '')


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditSpinButton'] = EditSpinButton
    common.widgets['EditSpinButton'] = builder
    common.widgets_from_xml['EditSpinButton'] = xml_builder

    return common.make_object_button('EditSpinButton', 'spinbtn.png')
