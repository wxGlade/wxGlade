"""\
wxSpinCtrlDouble objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
import common, config, misc
import new_properties as np


class EditSpinCtrlDouble(ManagedBase, EditStylesMixin):
    "Class to handle wxSpinCtrlDouble objects"
    # XXX unify with EditSpinButton?
    WX_CLASS = 'wxSpinCtrlDouble'
    _PROPERTIES = ["Widget", "range", "value", "increment", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, pos):
        ManagedBase.__init__(self, name, 'wxSpinCtrlDouble', parent, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.range = np.FloatRangePropertyA( "0.0, 100.0" )
        self.value = np.SpinDoublePropertyA(0, val_range=(0.0,100.0), immediate=True, default_value="")
        self.increment = np.SpinDoublePropertyD(1.0, val_range=(0.0,100.0), immediate=True, default_value=1.0)

    def create_widget(self):
        mi,ma = self.properties["range"].get_tuple()
        kwargs = {}
        if self.properties["value"].is_active():
            kwargs["initial"] = self.value
        if self.properties["increment"].is_active():
            kwargs["inc"] = self.value
        self.widget = wx.SpinCtrlDouble(self.parent_window.widget, self.id, min=mi, max=ma, **kwargs)

    def finish_widget_creation(self, sel_marker_parent=None, re_add=True):
        ManagedBase.finish_widget_creation(self, sel_marker_parent, re_add)
        self.widget.Bind(wx.EVT_CHILD_FOCUS, self._on_set_focus)
        self.widget.Bind(wx.EVT_SET_FOCUS, self._on_set_focus)
        self.widget.Bind(wx.EVT_SPIN, self.on_set_focus)

    def _on_set_focus(self, event):
        # don't set focused_widget during event, as this may cause crashes
        if not misc.focused_widget is self:
            misc.set_focused_widget(self, delayed=True)
        event.Skip()

    def properties_changed(self, modified):  # from EditSlider
        if not modified or "range" in modified and self.widget:
            mi,ma = self.properties["range"].get_tuple()
            self.widget.SetRange(mi, ma)
            self.properties["value"].set_range(mi,ma)
            self.properties["increment"].set_range(mi,ma)

        if not modified or "increment" in modified and self.widget:
            self.widget.SetIncrement(self.increment)

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


def builder(parent, pos):
    "factory function for EditSpinCtrl objects"
    name = common.root.get_next_name('spin_ctrl_double_%d', parent)
    with parent.frozen():
        editor = EditSpinCtrlDouble(name, parent, pos)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(attrs, parent, pos=None):
    "factory function to build EditSpinCtrlDouble objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    editor = EditSpinCtrlDouble( name, parent, pos )
    editor.properties["value"].set_active(False)
    return editor


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    if not hasattr(wx, "SpinCtrlDouble"): return None
    common.widgets['EditSpinCtrlDouble'] = builder
    common.widgets_from_xml['EditSpinCtrlDouble'] = xml_builder

    return common.make_object_button('EditSpinCtrlDouble', 'spin_ctrl_double.xpm')
