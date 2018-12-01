"""\
wxStaticLine objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common
import wcodegen
from edit_windows import ManagedBase, EditStylesMixin
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
    def __init__(self, name, parent, style, pos):
        ManagedBase.__init__(self, name, 'wxStaticLine', parent, pos)
        EditStylesMixin.__init__(self)

        # initialise instance properties
        self.attribute = np.CheckBoxProperty(False, default_value=False)
        if style: self.properties["style"].set(style)

    def create_widget(self):
        self.widget = wx.StaticLine(self.parent_window.widget, self.id, style=self.style)
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self)
        self.sel_marker.Reparent(self.parent_window.widget)
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


def builder(parent, pos):
    "factory function for editor objects from GUI"
    dialog = wcodegen.WidgetStyleSelectionDialog(dlg_title, box_title, choices)
    res = dialog.ShowModal()
    style = dialog.get_selection()
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    name = common.root.get_next_name('static_line_%d', parent)
    with parent.frozen():
        widget = editor_class(name, parent, style, pos)
        import edit_sizers
        if isinstance(sizer, edit_sizers.edit_sizers.BoxSizerBase):
            if ( (sizer.orient & wx.VERTICAL   and style=="wxLI_HORIZONTAL") or 
                 (sizer.orient & wx.HORIZONTAL and style=="wxLI_VERTICAL") ):
                widget.properties["flag"].add("wxEXPAND")
        if parent.widget: widget.create()
    common.app_tree.insert(widget, parent, pos)


def xml_builder(attrs, parent, pos=None):
    "Factory to build editor objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    widget = editor_class(name, parent, editor_style, pos)
    common.app_tree.insert(widget, parent, pos)
    return widget


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widgets[editor_name] = builder
    common.widgets_from_xml[editor_name] = xml_builder
    return common.make_object_button(editor_name, editor_icon)
