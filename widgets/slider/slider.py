"""\
wxSlider objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common
from edit_windows import ManagedBase, EditStylesMixin
import new_properties as np



class EditSlider(ManagedBase, EditStylesMixin):
    "Class to handle wxSlider objects"

    WX_CLASS = 'wxSlider'
    _PROPERTIES = ["Widget", "range", "value", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, index, style):
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self, style)

        # initialise instance properties
        self.range = np.IntRangePropertyA( "0, 10", notnull=True )
        self.value = np.SpinPropertyA(0, val_range=(0,10), immediate=True)

    def create_widget(self):
        mi,ma = self.properties["range"].get_tuple()
        value_p = self.properties["value"]
        value = value_p.get()  if value_p.is_active()  else  mi
        self.widget = wx.Slider(self.parent_window.widget, wx.ID_ANY, value, mi, ma, style=self.style)

    def _properties_changed(self, modified, actions):
        if not modified or "range" in modified:
            mi,ma = self.properties["range"].get_tuple()
            if self.widget:
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
    "factory function for editor objects from GUI"
    import dialogs, misc
    dialog = dialogs.WidgetStyleSelectionDialog( _('wxSlider'), _('Orientation'), 'wxSL_HORIZONTAL|wxSL_VERTICAL' )
    with misc.disable_stay_on_top(common.adding_window or parent):
        res = dialog.ShowModal()
    style = dialog.get_selection()
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    name = parent.toplevel_parent.get_next_contained_name('slider_%d')
    with parent.frozen():
        editor = EditSlider(name, parent, index, style)
        editor.properties["flag"].set("wxEXPAND")
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "Factory to build editor objects from a XML file"
    return EditSlider(name, parent, index, '')


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditSlider'] = EditSlider
    common.widgets['EditSlider'] = builder
    common.widgets_from_xml['EditSlider'] = xml_builder
    return common.make_object_button('EditSlider', 'slider.png')
