"""\
wxListCtrl objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016-2018 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from edit_windows import ManagedBase, EditStylesMixin
from widgets.grid.grid import GridColsProperty, ColsHandler
import new_properties as np
from tree import Node
import common, misc, compat


class EditListCtrl(ManagedBase, EditStylesMixin):
    "Class to handle wxListCtrl objects"

    update_widget_style = True

    _PROPERTIES = ["Widget", "style", "columns", "rows_number"]
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    _PROPERTY_HELP = {"rows_number":"This is just used for the design and preview windows.",
                      "columns":"Only for style LC_REPORT."}

    def __init__(self, name, parent, id, sizer, pos, style=wx.LC_REPORT | wx.BORDER_SUNKEN):
        ManagedBase.__init__(self, name, 'wxListCtrl', parent, id, sizer, pos)
        EditStylesMixin.__init__(self)
        if style: self.properties["style"].set(style)
        self.columns = GridColsProperty([])
        self.rows_number = np.SpinProperty(0, immediate=True, default_value=0)

    def create_widget(self):
        self.widget = wx.ListCtrl(self.parent.widget, self.id, style=self.style)
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

    def finish_widget_creation(self):
        ManagedBase.finish_widget_creation(self, sel_marker_parent=self.widget)

    def _set_name(self):
        if not self.widget: return
        if self.rows_number>0:
            # add some info, one or two cells
            if len(self.columns)>=2:
                compat.ListCtrl_SetStringItem(self.widget, 0,0, "List Control:")
                compat.ListCtrl_SetStringItem(self.widget, 0,1, self.name)
            elif self.columns:
                compat.ListCtrl_SetStringItem(self.widget, 0, 0, "List Control: %s"%self.name)

    def properties_changed(self, modified):
        EditStylesMixin.properties_changed(self, modified)
        self._update_widget_properties(modified)
        ManagedBase.properties_changed(self, modified)
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
        self.widget.Refresh()

    def get_property_handler(self, name):
        if name == 'columns': return ColsHandler(self)
        return ManagedBase.get_property_handler(self, name)


def builder(parent, sizer, pos, number=[1]):
    "factory function for EditListCtrl objects"
    name = 'list_ctrl_%d' % number[0]
    while common.app_tree.has_name(name):
        number[0] += 1
        name = 'list_ctrl_%d' % number[0]
    with parent.frozen():
        list_ctrl = EditListCtrl(name, parent, wx.NewId(), sizer, pos)
        #list_ctrl.properties["style"].set_to_default()  # default is wxLC_ICON
        list_ctrl.properties["columns"].set( [['A', -1], ['B', -1], ['C', -1]] )
        list_ctrl.properties["rows_number"].set(10)
        list_ctrl.properties["style"].set( ["wxLC_REPORT", "wxLC_HRULES", "wxLC_VRULES"] )
        node = Node(list_ctrl)
        list_ctrl.node = node
        list_ctrl.properties["proportion"].set(1)
        list_ctrl.properties["flag"].set("wxEXPAND")
        if parent.widget: list_ctrl.create()
    common.app_tree.insert(node, sizer.node, pos-1)
    #sizer.set_item(list_ctrl.pos, 1, wx.EXPAND)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory function to build EditListCtrl objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        name = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    list_ctrl = EditListCtrl(name, parent, wx.NewId(), sizer, pos, style=0)
    #sizer.set_item(list_ctrl.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(list_ctrl)
    list_ctrl.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return list_ctrl


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette"
    common.widgets['EditListCtrl'] = builder
    common.widgets_from_xml['EditListCtrl'] = xml_builder

    return common.make_object_button('EditListCtrl', 'list_ctrl.xpm')
