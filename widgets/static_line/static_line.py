"""\
wxStaticLine objects

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


class EditStaticLine(ManagedBase, EditStylesMixin):
    "Class to handle wxStaticLine objects"
    _PROPERTIES = ["attribute"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    PROPERTIES.remove("font")
    ManagedBase.MOVE_PROPERTY(PROPERTIES, "attribute", "name")
    _PROPERTY_LABELS = {"attribute":'Store as attribute'}
    _PROPERTY_HELP={"attribute":'Store instance as attribute of window class; e.g. self.line_1 = wx.wxStaticLine(...)\n'
                                'Without this, you can not access the line from your program.'}
    def __init__(self, name, parent, id, style, sizer, pos):
        ManagedBase.__init__(self, name, 'wxStaticLine', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.attribute = np.CheckBoxProperty(False, default_value=False)
        if style: self.properties["style"].set(style)

    def create_widget(self):
        self.widget = wx.StaticLine(self.parent.widget, self.id, style=self.style)
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self)
        self.sel_marker.Reparent(self.parent.widget)
        #del self.properties['font']

    def __getitem__(self, key):
        if key != 'font':
            return ManagedBase.__getitem__(self, key)
        return lambda: "", lambda v: None

    def properties_changed(self, modified=None):
        EditStylesMixin.properties_changed(self, modified)
        ManagedBase.properties_changed(self, modified)


editor_class = EditStaticLine
editor_icon = 'static_line.xpm'
editor_name = 'EditStaticLine'
editor_style = ''

dlg_title = _('wxStaticLine')
box_title = _('Orientation')
choices = 'wxLI_HORIZONTAL|wxLI_VERTICAL'
tmpl_label = 'static_line'


def builder(parent, sizer, pos, number=[1]):
    "factory function for editor objects from GUI"
    dialog = wcodegen.WidgetStyleSelectionDialog(dlg_title, box_title, choices)
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
        import edit_sizers
        if isinstance(sizer, edit_sizers.edit_sizers.BoxSizerBase):
            if ( (sizer.orient & wx.VERTICAL   and style=="wxLI_HORIZONTAL") or 
                 (sizer.orient & wx.HORIZONTAL and style=="wxLI_VERTICAL") ):
                widget.properties["flag"].add("wxEXPAND")
        node = Node(widget)
        widget.node = node
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
