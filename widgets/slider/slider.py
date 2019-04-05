"""\
wxSlider objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2019 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common, misc
import wcodegen
from edit_windows import ManagedBase, EditStylesMixin
from tree import Node
import new_properties as np



class EditSlider(ManagedBase, EditStylesMixin):
    "Class to handle wxSlider objects"

    _PROPERTIES = ["Widget", "range", "value", "style"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES

    def __init__(self, name, parent, id, style, sizer, pos):
        ManagedBase.__init__(self, name, 'wxSlider', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.range = np.IntRangePropertyA( "0, 10", notnull=True )
        self.value = np.SpinPropertyA(0, val_range=(0,10), immediate=True)
        if style: self.properties["style"].set(style)

    def create_widget(self):
        mi,ma = self.properties["range"].get_tuple()
        value_p = self.properties["value"]
        value = value_p.get()  if value_p.is_active()  else  mi
        self.widget = wx.Slider(self.parent.widget, self.id, value, mi, ma, style=self.style)

    def properties_changed(self, modified):
        if not modified or "range" in modified:
            mi,ma = self.properties["range"].get_tuple()
            if self.widget:
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




editor_class = EditSlider
editor_icon = 'slider.xpm'
editor_name = 'EditSlider'
editor_style = ''

dlg_title = _('wxSlider')
box_title = _('Orientation')
choices = 'wxSL_HORIZONTAL|wxSL_VERTICAL'
tmpl_label = 'slider'


def builder(parent, sizer, pos, number=[1]):
    "factory function for editor objects from GUI"
    dialog = wcodegen.WidgetStyleSelectionDialog( dlg_title, box_title, choices )
    with misc.disable_stay_on_top(common.adding_window or parent):
        res = dialog.ShowModal()
    style = dialog.get_selection()
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    label = '%s_%d' % (tmpl_label, number[0])
    while common.app_tree.has_name(label):
        number[0] += 1
        label = '%s_%d' % (tmpl_label, number[0])
    with parent.frozen():
        widget = editor_class(label, parent, wx.ID_ANY, style, sizer, pos)
        node = Node(widget)
        widget.node = node
        widget.properties["flag"].set("wxEXPAND")
        if parent.widget: widget.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "Factory to build editor objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    widget = editor_class(name, parent, wx.ID_ANY, editor_style, sizer, pos)
    #sizer.set_item(widget.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(widget)
    widget.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return widget


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets[editor_name] = builder
    common.widgets_from_xml[editor_name] = xml_builder
    return common.make_object_button(editor_name, editor_icon)
