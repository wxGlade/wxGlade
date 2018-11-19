"""\
wxGauge objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common
import wcodegen
from edit_windows import ManagedBase, EditStylesMixin
import new_properties as np


class EditGauge(ManagedBase, EditStylesMixin):
    "Class to handle wxGauge objects"

    _PROPERTIES = ["Widget", "range", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    recreate_on_style_change = True

    def __init__(self, name, parent, style, pos):
        ManagedBase.__init__(self, name, 'wxGauge', parent, pos)
        EditStylesMixin.__init__(self)
        if style: self.properties["style"].set(style)

        # initialise instance properties
        self.range = np.SpinProperty(10, val_range=(0,10000000), immediate=True)

    def create_widget(self):
        self.widget = wx.Gauge(self.parent_window.widget, self.id, self.range, style=self.style)
        if self.range>=3: self.widget.SetValue(self.range//3)

    def properties_changed(self, modified):
        if not modified or "range" in modified and self.widget:
            self.widget.SetRange(self.range)
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)

editor_class = EditGauge
editor_icon = 'gauge.xpm'
editor_name = 'EditGauge'
editor_style = ''

dlg_title = _('wxGauge')
box_title = _('Orientation')
choices = 'wxGA_HORIZONTAL|wxGA_VERTICAL'


def builder(parent, pos):
    "Factory function for editor objects from GUI"
    dialog = wcodegen.WidgetStyleSelectionDialog( dlg_title, box_title, choices)
    res = dialog.ShowModal()
    style = dialog.get_selection()
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    name = common.root.get_next_name('gauge_%d', parent)
    with parent.frozen():
        widget = editor_class(name, parent, style, pos)
        widget.properties["flag"].set("wxEXPAND")
        if parent.widget: widget.create()
    common.app_tree.insert(widget, parent, pos)


def xml_builder(attrs, parent, sizeritem, pos=None):
    "Factory to build editor objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    widget = editor_class(name, parent, editor_style, pos)
    #sizer.set_item(widget.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    common.app_tree.insert(widget, parent, pos)
    return widget


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets[editor_name] = builder
    common.widgets_from_xml[editor_name] = xml_builder
    return common.make_object_button(editor_name, editor_icon)
