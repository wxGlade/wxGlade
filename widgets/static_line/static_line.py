"""\
wxStaticLine objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
import common
from edit_windows import ManagedBase, EditStylesMixin
import new_properties as np


class EditStaticLine(ManagedBase, EditStylesMixin):
    "Class to handle wxStaticLine objects"
    WX_CLASS = 'wxStaticLine'
    _PROPERTIES = ["attribute"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    PROPERTIES.remove("font")
    ManagedBase.MOVE_PROPERTY(PROPERTIES, "attribute", "name")
    _PROPERTY_LABELS = {"attribute":'Store as attribute'}
    _PROPERTY_HELP={"attribute":'Store instance as attribute of window class; e.g. self.line_1 = wx.wxStaticLine(...)\n'
                                'Without this, you can not access the line from your program.'}
    def __init__(self, name, parent, index, style):
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self, style)

        self.attribute = np.CheckBoxProperty(False, default_value=False)

    def create_widget(self):
        self.widget = wx.StaticLine(self.parent_window.widget, wx.ID_ANY, style=self.style)
        self.widget.Bind(wx.EVT_LEFT_DOWN, self.on_set_focus)

    def finish_widget_creation(self, level):
        ManagedBase.finish_widget_creation(self, level)
        self.sel_marker.Reparent(self.parent_window.widget)

    def __getitem__(self, key):
        if key != 'font':
            return ManagedBase.__getitem__(self, key)
        return lambda: "", lambda v: None

    def _properties_changed(self, modified, actions):
        EditStylesMixin._properties_changed(self, modified, actions)
        ManagedBase._properties_changed(self, modified, actions)


def builder(parent, index):
    "factory function for editor objects from GUI"
    import dialogs, misc
    dialog = dialogs.WidgetStyleSelectionDialog(_('wxStaticLine'), _('Orientation'), 'wxLI_HORIZONTAL|wxLI_VERTICAL')
    with misc.disable_stay_on_top(common.adding_window or parent):
        res = dialog.ShowModal()
    style = dialog.get_selection()
    dialog.Destroy()
    if res != wx.ID_OK:
        return

    name = parent.toplevel_parent.get_next_contained_name('static_line_%d')
    with parent.frozen():
        editor = EditStaticLine(name, parent, index, style)
        if parent.IS_SIZER and "orient" in parent.properties and parent.orient:
            if ( (parent.orient & wx.VERTICAL   and style=="wxLI_HORIZONTAL") or 
                 (parent.orient & wx.HORIZONTAL and style=="wxLI_VERTICAL") ):
                editor.properties["flag"].add("wxEXPAND")
        if parent.widget: editor.create()
    return editor


def xml_builder(parser, base, name, parent, index):
    "Factory to build editor objects from a XML file"
    return EditStaticLine(name, parent, index, '')


def initialize():
    "initialization function for the module: returns a wxBitmapButton to be added to the main palette"
    common.widget_classes['EditStaticLine'] = EditStaticLine
    common.widgets['EditStaticLine'] = builder
    common.widgets_from_xml['EditStaticLine'] = xml_builder
    return common.make_object_button('EditStaticLine', 'static_line.png')
