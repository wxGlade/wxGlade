"""\
wxListCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2021 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
from widgets.grid.grid import GridColsProperty, ColsHandler
import new_properties as np
import common, misc, compat


class EditListCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wxListCtrl objects"

    WX_CLASS = "wxListCtrl"
    _PROPERTIES = ["Widget", "style", "columns", "rows_number"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    _PROPERTY_HELP = {"rows_number":"This is just used for the design and preview windows.",
                      "columns":"Only for style LC_REPORT."}
    recreate_on_style_change = True

    def __init__(self, name, parent, index, style=wx.LC_REPORT | wx.BORDER_SUNKEN):
        ManagedBase.__init__(self, name, parent, index)
        EditStylesMixin.__init__(self, style)
        self.columns = GridColsProperty([])
        self.rows_number = np.SpinProperty(0, immediate=True, default_value=0)
        self.properties["style"]._ignore_names = {"wxLC_VIRTUAL"}
        self.properties["style"]._one_required = ["wxLC_ICON", "wxLC_SMALL_ICON", "wxLC_LIST", "wxLC_REPORT"]

    def create_widget(self):
        self.widget = wx.ListCtrl(self.parent_window.widget, wx.ID_ANY, style=self.style)
        self._update_widget_properties(modified=None)
        self.widget.Bind(wx.EVT_LIST_COL_CLICK, self.on_set_focus)
        self.widget.Bind(wx.EVT_LIST_COL_END_DRAG, self._on_grid_col_resize)

    def _on_grid_col_resize(self, event):
        "update width in columns property"
        col = event.GetColumn()
        new_value = self.widget.GetColumnWidth(col)
        prop = self.properties["columns"]
        value = prop.value[:]
        if new_value==value[col][1]: return
        value[col] = [value[col][0],new_value]
        prop._check_for_user_modification(value)
        prop.update_display()

    def finish_widget_creation(self, level):
        ManagedBase.finish_widget_creation(self, level, sel_marker_parent=self.widget)

    def _set_name(self):
        if not self.widget: return
        if self.rows_number>0:
            # add some info, one or two cells
            if len(self.columns)>=2:
                compat.ListCtrl_SetStringItem(self.widget, 0,0, "List Control:")
                compat.ListCtrl_SetStringItem(self.widget, 0,1, self.name)
            elif self.columns:
                compat.ListCtrl_SetStringItem(self.widget, 0, 0, "List Control: %s"%self.name)

    def _properties_changed(self, modified, actions):
        EditStylesMixin._properties_changed(self, modified, actions)
        if "recreate2" in actions: return
        self._update_widget_properties(modified)
        if modified: actions.add("refresh")
        ManagedBase._properties_changed(self, modified, actions)
        if not modified or "name" in modified:
            self._set_name()

    def _update_widget_properties(self, modified=None):
        # after initial creation, call with modified=None

        if not self.widget: return
        
        if self.style & wx.LC_REPORT:
            # columns and rows #############################################################################################
            if not modified or "columns" in modified or "style" in modified:
                columns = self.columns
                # adjust number of columns
                while self.widget.GetColumnCount()>len(columns):
                    self.widget.DeleteColumn(self.widget.GetColumnCount()-1)
                while self.widget.GetColumnCount()<len(columns):
                    i = self.widget.GetColumnCount()
                    self.widget.InsertColumn(i, columns[i][0])
                # set column widths and labels
                for i, (label,size) in enumerate(columns):
                    item = wx.ListItem()
                    item.SetText(label)
                    self.widget.SetColumn(i, item)
                    size = int(size or "0") 
                    if size>0:
                        # results with LIST_AUTOSIZE are not too good
                        self.widget.SetColumnWidth(i, size)
            if not modified or "rows_number" in modified or "style" in modified:
                self.widget.DeleteAllItems()
                if self.columns:
                    for i in range(self.rows_number):
                        compat.ListCtrl_InsertStringItem(self.widget, i, "")
    
            self._set_name()

    def get_property_handler(self, name):
        if name == 'columns': return ColsHandler(self)
        return ManagedBase.get_property_handler(self, name)


def builder(parent, index):
    "factory function for EditListCtrl objects"
    name = parent.toplevel_parent.get_next_contained_name('list_ctrl_%d')
    with parent.frozen():
        editor = EditListCtrl(name, parent, index, ["wxLC_REPORT", "wxLC_HRULES", "wxLC_VRULES"])
        #list_ctrl.properties["style"].set_to_default()  # default is wxLC_ICON
        editor.properties["columns"].set( [['A', -1], ['B', -1], ['C', -1]] )
        editor.properties["rows_number"].set(10)
        editor.properties["proportion"].set(1)
        editor.properties["flag"].set("wxEXPAND")
        if parent.widget: editor.create()
    #sizer.set_item(list_ctrl.index, 1, wx.EXPAND)
    return editor


def xml_builder(parser, base, name, parent, index):
    "factory function to build EditListCtrl objects from a XML file"
    return EditListCtrl(name, parent, index, 0)


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widget_classes['EditListCtrl'] = EditListCtrl
    common.widgets['EditListCtrl'] = builder
    common.widgets_from_xml['EditListCtrl'] = xml_builder

    return common.make_object_button('EditListCtrl', 'list_ctrl.png')
