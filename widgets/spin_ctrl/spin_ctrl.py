"""\
wxSpinCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2020 Dietmar Schwertberger
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

    def __init__(self, name, parent, pos):
        ManagedBase.__init__(self, name, 'wxSpinCtrl', parent, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.range = np.IntRangePropertyA( "0, 100" )
        self.value = np.SpinPropertyA(0, val_range=(0,100), immediate=True, default_value="")

    def create_widget(self):
        mi,ma = self.properties["range"].get_tuple()
        if self.properties["value"].is_active():
            self.widget = wx.SpinCtrl(self.parent_window.widget, self.id, min=mi, max=ma, initial=self.value)
        else:
            self.widget = wx.SpinCtrl(self.parent_window.widget, self.id, min=mi, max=ma)

    def finish_widget_creation(self, sel_marker_parent=None, re_add=True):
        ManagedBase.finish_widget_creation(self, sel_marker_parent, re_add)
        self.widget.Bind(wx.EVT_CHILD_FOCUS, self._on_set_focus)
        self.widget.Bind(wx.EVT_SET_FOCUS, self._on_set_focus)
        self.widget.Bind(wx.EVT_SPIN, self.on_set_focus)

    def _on_set_focus(self, event):
        # within a short time window, we ignore focus events as these seem due losing focus
        if not misc.focused_widget is self and time.time()-misc.focused_time > 0.05:
            # don't set focused_widget during event, as this may cause crashes; use delay instead
            misc.set_focused_widget(self, delayed=True)
        event.Skip()

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


def builder(parent, pos):
    "factory function for EditSpinCtrl objects"
    name = common.root.get_next_name('spin_ctrl_%d', parent)
    with parent.frozen():
        editor = EditSpinCtrl(name, parent, pos)
        editor.properties["style"].set_to_default()
        editor.check_defaults()
        if parent.widget: editor.create()
    return editor


def xml_builder(attrs, parent, pos=None):
    "factory function to build EditSpinCtrl objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    editor = EditSpinCtrl( name, parent, pos )
    editor.properties["value"].set_active(False)
    return editor


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditSpinCtrl'] = EditSpinCtrl
    common.widgets['EditSpinCtrl'] = builder
    common.widgets_from_xml['EditSpinCtrl'] = xml_builder

    return common.make_object_button('EditSpinCtrl', 'spin_ctrl.xpm')
