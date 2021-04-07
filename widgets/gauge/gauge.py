"""\
wxGauge objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common
from edit_windows import ManagedBase, EditStylesMixin
import new_properties as np


class EditGauge(ManagedBase, EditStylesMixin):
    "Class to handle wxGauge objects"

    WX_CLASS = "wxGauge"
    _PROPERTIES = ["Widget", "range", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    recreate_on_style_change = True

    def __init__(self, name, parent, index, style):
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self)
        if style: self.properties["style"].set(style)

        # initialise instance properties
        self.range = np.SpinProperty(10, val_range=(0,10000000), immediate=True)

    def create_widget(self):
        self.widget = wx.Gauge(self.parent_window.widget, wx.ID_ANY, self.range, style=self.style)
        if self.range>=3: self.widget.SetValue(self.range//3)

    def _properties_changed(self, modified, actions):
        if not modified or "range" in modified and self.widget:
            self.widget.SetRange(self.range)
        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)


def builder(parent, index):
    "Factory function for editor objects from GUI"
    import dialogs, misc
    dialog = dialogs.WidgetStyleSelectionDialog( _('wxGauge'), _('Orientation'), 'wxGA_HORIZONTAL|wxGA_VERTICAL')
    with misc.disable_stay_on_top(common.adding_window or parent):
        res = dialog.ShowModal()
    style = dialog.get_selection()
    dialog.Destroy()
    if res != wx.ID_OK: return

    name = parent.toplevel_parent.get_next_contained_name('gauge_%d')
    with parent.frozen():
        editor = EditGauge(name, parent, index, style)
        editor.properties["flag"].set("wxEXPAND")
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "Factory to build editor objects from a XML file"
    return EditGauge(name, parent, index, '')


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditGauge'] = EditGauge
    common.widgets['EditGauge'] = builder
    common.widgets_from_xml['EditGauge'] = xml_builder
    return common.make_object_button('EditGauge', 'gauge.png')
