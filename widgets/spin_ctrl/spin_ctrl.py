"""\
wxSpinCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
import time
import common, misc
import new_properties as np


class EditSpinCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wxSpinCtrl objects"
    # XXX unify with EditSpinButton?
    WX_CLASS = 'wxSpinCtrl'
    _PROPERTIES = ["Widget", "range", "value", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, index):
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.range = np.IntRangePropertyA( "0, 100" )
        self.value = np.SpinPropertyA(0, val_range=(0,100), immediate=True, default_value="")

    def create_widget(self):
        mi,ma = self.properties["range"].get_tuple()
        if self.properties["value"].is_active():
            self.widget = wx.SpinCtrl(self.parent_window.widget, wx.ID_ANY, min=mi, max=ma, initial=self.value)
        else:
            self.widget = wx.SpinCtrl(self.parent_window.widget, wx.ID_ANY, min=mi, max=ma)

    def finish_widget_creation(self, level, sel_marker_parent=None):
        ManagedBase.finish_widget_creation(self, level, sel_marker_parent)
        self.widget.Bind(wx.EVT_CHILD_FOCUS, self._on_set_focus)
        self.widget.Bind(wx.EVT_SET_FOCUS, self._on_set_focus)
        self.widget.Bind(wx.EVT_SPIN, self.on_set_focus)

    def _on_set_focus(self, event):
        # within a short time window, we ignore focus events as these seem due losing focus
        if not misc.focused_widget is self and time.time()-misc.focused_time > 0.05:
            # don't set focused_widget during event, as this may cause crashes; use delay instead
            misc.set_focused_widget(self, delayed=True)
        event.Skip()

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
                if self.widget:
                    self.widget.SetValue(value)

        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)


def builder(parent, index):
    "factory function for EditSpinCtrl objects"
    name = parent.toplevel_parent.get_next_contained_name('spin_ctrl_%d')
    with parent.frozen():
        editor = EditSpinCtrl(name, parent, index)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory function to build EditSpinCtrl objects from a XML file"
    editor = EditSpinCtrl( name, parent, index )
    editor.properties["value"].set_active(False)
    return editor


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditSpinCtrl'] = EditSpinCtrl
    common.widgets['EditSpinCtrl'] = builder
    common.widgets_from_xml['EditSpinCtrl'] = xml_builder

    return common.make_object_button('EditSpinCtrl', 'spin_ctrl.png')
