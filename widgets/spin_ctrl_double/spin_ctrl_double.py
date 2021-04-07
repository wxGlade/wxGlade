"""\
wxSpinCtrlDouble objects

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


class EditSpinCtrlDouble(ManagedBase, EditStylesMixin):
    "Class to handle wxSpinCtrlDouble objects"
    # XXX unify with EditSpinButton or SpinCtrl?
    WX_CLASS = 'wxSpinCtrlDouble'
    _PROPERTIES = ["Widget", "range", "value", "increment", "digits", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    _PROPERTY_HELP = { "digits": "Depending on your wx version you may need to set this explicitely.\n"
                                 "E.g. if it's undefined with wxPython >=4.1.1 you can only enter integers\n"
                                 "while on older versions you could enter any float number in this case.\n"
                                 "If you want to be on the safe side, leave it active." }

    def __init__(self, name, parent, index):
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.range = np.FloatRangePropertyA( "0.0, 100.0" )
        self.value = np.SpinDoublePropertyA(0, val_range=(0.0,100.0), immediate=True, default_value="")
        self.increment = np.SpinDoublePropertyD(1.0, val_range=(0.0,100.0), immediate=True, default_value=1.0)
        self.digits = np.SpinPropertyA(2, val_range=(0,20), immediate=True)

    def create_widget(self):
        mi,ma = self.properties["range"].get_tuple()
        kwargs = {}
        if self.properties["value"].is_active():
            kwargs["initial"] = self.value
        if self.properties["increment"].is_active():
            kwargs["inc"] = self.value
        self.widget = wx.SpinCtrlDouble(self.parent_window.widget, wx.ID_ANY, min=mi, max=ma, **kwargs)
        if self.properties["digits"].is_active():
            self.widget.SetDigits(self.digits)

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
            self.properties["increment"].set_range(mi,ma)

        if not modified or "increment" in modified and self.widget:
            self.widget.SetIncrement(self.increment)

        if not modified or "digits" in modified and self.widget:
            self.widget.SetDigits(self.digits)

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
    name = parent.toplevel_parent.get_next_contained_name('spin_ctrl_double_%d')
    with parent.frozen():
        editor = EditSpinCtrlDouble(name, parent, index)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory function to build EditSpinCtrlDouble objects from a XML file"
    editor = EditSpinCtrlDouble( name, parent, index )
    editor.properties["value"].set_active(False)
    editor.properties["digits"].set_active(False)
    return editor


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    if not hasattr(wx, "SpinCtrlDouble"): return None
    common.widget_classes['EditSpinCtrlDouble'] = EditSpinCtrlDouble
    common.widgets['EditSpinCtrlDouble'] = builder
    common.widgets_from_xml['EditSpinCtrlDouble'] = xml_builder

    return common.make_object_button('EditSpinCtrlDouble', 'spin_ctrl_double.png')
