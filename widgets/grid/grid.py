"""\
wxGrid objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2016 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from wx.grid import *
import common, misc, compat
from edit_windows import ManagedBase
from tree import Node
import new_properties as np
from wcodegen.taghandler import BaseXmlBuilderTagHandler


class GridColsProperty(np.GridProperty):
    def __init__(self, value):
        definition = [('Label', np.GridProperty.STRING), ('Size', np.GridProperty.INT)]
        default = ['', -1]
        np.GridProperty.__init__(self, value, definition, default)

    def write(self, output, tabs):
        value = self.get()
        if value:
            inner_xml = []
            for label, size in value:
                inner_xml += common.format_xml_tag(u'column', label, tabs+1, size=size)
            output.extend( common.format_xml_tag(u'columns', inner_xml, tabs, is_xml=True) )

    def _get_label(self, col):
        s = []
        while True:
            s.append(chr(ord('A') + col%26))
            col = col//26 - 1
            if col < 0: break
        s.reverse()
        return "".join(s)

    def _check_label(self, label, i):
        "return True if it's not the default value"
        return label != self._get_label(i)

    def add_row(self, event):
        np.GridProperty.add_row(self, event)
        rows = self.grid.NumberRows
        label = self._get_label(rows-1)
        self.grid.SetCellValue(rows-1, 0, label)
        # take the default column width from the previous row
        col_width = -1 if rows==1 else int(self.grid.GetCellValue(rows-2, 1))
        self.grid.SetCellValue(rows-1, 1, str(col_width))
        self.editing_values[-1] = [label, col_width]

    def insert_row(self, event):
        np.GridProperty.insert_row(self, event)
        label = self._get_label(self.cur_row)
        self.grid.SetCellValue(self.cur_row, 0, label)
        # take the default column width from the previous row
        col_width = -1 if self.cur_row<1 else int(self.grid.GetCellValue(self.cur_row-1, 1))
        self.grid.SetCellValue(self.cur_row, 1, str(col_width))
        self.editing_values[self.cur_row] = [label, col_width]



class ColsHandler(BaseXmlBuilderTagHandler):
    # for XML import
    def __init__(self, parent):
        super(ColsHandler, self).__init__()
        self.parent = parent
        self.columns = []
        self.curr_col = []
        self.curr_size = -1

    def start_elem(self, name, attrs):
        if name == 'column':
            self.curr_size = attrs.get('size', -1) or 0

    def end_elem(self, name):
        if name == 'columns':
            self.parent.properties['columns'].set(self.columns)
            self.parent.properties_changed(["columns"])
            return True
        elif name == 'column':
            char_data = self.get_char_data()
            self.columns.append([char_data, int(self.curr_size)])
        return False


class GridRowsProperty(GridColsProperty):
    def _get_label(self, row):
        return str(row)

    def _check_label(self, label, i):
        "return True if it's not the default value"
        return label != self._get_label(i)

    def load(self, value, activate=None, deactivate=None, notify=False):
        if isinstance(value, compat.unicode):
            value =  [[str(n),-1] for n in range(int(value))]
        np.GridProperty.load(self, value, activate, deactivate, notify)

    def write(self, output, tabs):
        is_default = True
        inner_xml = []
        rows = self.get()
        for i, (label, size) in enumerate(rows):
            if size!=-1 or label!=str(i):
                is_default=False
            inner_xml += common.format_xml_tag(u'row', label, tabs+1, size=size)
        if not is_default:
            # actually write labels and sizes
            output.extend( common.format_xml_tag(u'rows', inner_xml, tabs, is_xml=True) )
        else:
            # just write the number of rows
            output.extend( common.format_xml_tag(u'rows_number', str(len(rows)), tabs) )

    def add_row(self, event):
        np.GridProperty.add_row(self, event)
        rows = self.grid.NumberRows
        label = self._get_label(rows-1)
        self.grid.SetCellValue(rows-1, 0, label)
        # take the default row width from the previous row
        col_width = -1 if rows==1 else int(self.grid.GetCellValue(rows-2, 1))
        self.grid.SetCellValue(rows-1, 1, str(col_width))
        self.editing_values[-1] = [label, col_width]

    def insert_row(self, event):
        np.GridProperty.insert_row(self, event)
        label = self._get_label(self.cur_row)
        self.grid.SetCellValue(self.cur_row, 0, label)
        # take the default row width from the previous row
        col_width = -1 if self.cur_row<1 else int(self.grid.GetCellValue(self.cur_row-1, 1))
        self.grid.SetCellValue(self.cur_row, 1, str(col_width))
        self.editing_values[self.cur_row] = [label, col_width]



class RowsHandler(BaseXmlBuilderTagHandler):
    # for XML import
    def __init__(self, parent):
        super(RowsHandler, self).__init__()
        self.parent = parent
        self.rows = []
        self.curr_col = []
        self.curr_size = -1

    def start_elem(self, name, attrs):
        if name == 'row':
            self.curr_size = attrs.get('size', -1)

    def end_elem(self, name):
        if name == 'rows':
            self.parent.properties['rows'].set(self.rows)
            self.parent.properties_changed(["rows"])
            return True
        elif name == 'row':
            char_data = self.get_char_data()
            self.rows.append([char_data, int(self.curr_size)])
        return False


class EditGrid(ManagedBase):

    _PROPERTIES =["Widget", 'create_grid', 'row_label_size', 'col_label_size',
                  'enable_editing', 'enable_grid_lines', 'enable_col_resize', 'enable_row_resize', 'enable_grid_resize',
                  'lines_color', 'label_bg_color', 'selection_mode', 'columns', 'rows']
    PROPERTIES = ManagedBase.PROPERTIES + _PROPERTIES + ManagedBase.EXTRA_PROPERTIES
    _PROPERTY_HELP = {"create_grid":"The following properties are meaningful only if 'Create grid' is selected",
                      "columns":"Enter \\n for a line break in the label",
                      "rows":"Enter \\n for a line break in the label",
                      "selection_mode": "wxGrid.wxGridSelectCells, ...Rows or ...Columns"}
    _PROPERTY_LABELS = {'label_bg_color':"Label background colour",}

    #_SELECTION_MODES = { 'wxGrid.wxGridSelectCells':0, 'wxGrid.wxGridSelectRows':1, 'wxGrid.wxGridSelectColumns':2 }
    _SELECTION_MODES = ('wxGrid.wxGridSelectCells', 'wxGrid.wxGridSelectRows', 'wxGrid.wxGridSelectColumns')

    def __init__(self, name, parent, id, sizer, pos):
        "Class to handle wxGrid objects"
        ManagedBase.__init__(self, name, 'wxGrid', parent, id, sizer, pos)

        # instance properties
        self.create_grid = np.CheckBoxProperty(True)
        columns = [['A', -1], ['B', -1], ['C', -1]]
        self.columns = GridColsProperty([])
        rows =  [[str(n),-1] for n in range(10)]
        self.rows = GridRowsProperty( rows )
        self.properties["rows_number"] = self.properties["rows"]  # backward compatibility
        #self.rows_number        = np.SpinProperty(10, immediate=True)
        self.row_label_size     = np.SpinPropertyD(30, default_value=30, immediate=True)
        self.col_label_size     = np.SpinPropertyD(30, default_value=30, immediate=True)

        self.lines_color        = np.ColorPropertyD('#000000', default_value='#000000')
        self.label_bg_color     = np.ColorPropertyD('#C0C0C0', default_value='#C0C0C0')

        self.enable_editing     = np.CheckBoxProperty(True)
        self.enable_grid_lines  = np.CheckBoxProperty(True)
        self.enable_col_resize  = np.CheckBoxProperty(True)
        self.enable_row_resize  = np.CheckBoxProperty(True)
        self.enable_grid_resize = np.CheckBoxProperty(True)

        self.selection_mode = np.RadioProperty(0, [0,1,2], ["Cells","Rows","Columns"], aliases=self._SELECTION_MODES,
                                               columns=3)

    def create_widget(self):
        self.widget = Grid(self.parent.widget, self.id, (200, 200))
        #self.widget.CreateGrid(self.rows_number, len(self.columns))
        self.widget.CreateGrid(len(self.rows), len(self.columns))

        # read default colors from created widget
        default_background = misc.color_to_string( self.widget.GetLabelBackgroundColour() )
        default_lines      = misc.color_to_string( self.widget.GetGridLineColour() )
        self.properties['label_bg_color'].set_default(default_background)
        self.properties['lines_color'   ].set_default(default_lines)

        self._update_widget_properties(modified=None)

        # following two events are to permit select grid from designer frame
        self.widget.Bind(EVT_GRID_CELL_LEFT_CLICK, self.on_set_focus)
        self.widget.Bind(EVT_GRID_LABEL_LEFT_CLICK, self.on_set_focus)
        # these are to show the popup menu on right click
        self.widget.Bind(EVT_GRID_CELL_RIGHT_CLICK, self.popup_menu)
        self.widget.Bind(EVT_GRID_LABEL_RIGHT_CLICK, self.popup_menu)
        self.widget.Bind(EVT_GRID_COL_SIZE, self._on_grid_col_resize)
        self.widget.Bind(EVT_GRID_ROW_SIZE, self._on_grid_row_resize)

    def _on_grid_col_resize(self, event):
        col = event.GetRowOrCol()
        new_value = self.widget.GetColSize(col)
        prop = self.properties["columns"]
        value = prop.value[:]
        if new_value==value[col][1]: return
        value[col] = [value[col][0],new_value]
        prop._check_for_user_modification(value)
        prop.update_display()

    def _on_grid_row_resize(self, event):
        row = event.GetRowOrCol()
        new_value = self.widget.GetRowSize(row)
        prop = self.properties["rows"]
        value = prop.value[:]
        if new_value==value[row][1]: return
        value[row] = [value[row][0],new_value]
        prop._check_for_user_modification(value)
        prop.update_display()

    def _update_widget_properties(self, modified=None):
        # after initial creation, call with modified=None
        if not modified or "create_grid" in modified:
            # block/unblock other properties
            blocked = not self.create_grid
            for name in self._PROPERTIES[2:]:
                self.properties[name].set_blocked(blocked)

        if not self.widget: return

        if not modified or "enable_editing" in modified:
            if modified is None: self.widget.EnableEditing(self.enable_editing)  # set only initially

        def m(name):
            # returns True if an argument is modified and either active or has a default_value
            if modified and not name in modified:
                return False
            if not self.properties[name].is_active() and self.properties[name].default_value is np._DefaultArgument:
                return False
            return True

        # 'simple' properties ##########################################################################################
        if m("row_label_size"):     self.widget.SetRowLabelSize(self.row_label_size)
        if m("col_label_size"):     self.widget.SetColLabelSize(self.col_label_size)
        if m("enable_grid_lines"):  self.widget.EnableGridLines(self.enable_grid_lines)
        if m("enable_col_resize"):  self.widget.EnableDragColSize(self.enable_col_resize)
        if m("enable_row_resize"):  self.widget.EnableDragRowSize(self.enable_row_resize)
        if m("enable_grid_resize"): self.widget.EnableDragGridSize(self.enable_grid_resize)
        if m("lines_color"):        self.widget.SetGridLineColour( self.properties["lines_color"].get_color() )
        if m("label_bg_color"):     self.widget.SetLabelBackgroundColour( self.properties["label_bg_color"].get_color() )
        if m("selection_mode"):     self.widget.SetSelectionMode(self.selection_mode)

        # columns and rows #############################################################################################
        if m("columns"):
            columns = self.columns
            # adjust number of columns
            delta = len(columns) - self.widget.GetNumberCols()
            if   delta>0: self.widget.AppendCols(delta)
            elif delta<0: self.widget.DeleteCols(0, -delta)
            # set column widths and labels
            for i, (label,size) in enumerate(columns):
                size = int(size or "0") 
                if size>0:
                    self.widget.SetColSize(i, size)
                self.widget.SetColLabelValue(i, label.replace('\\n', '\n'))
        if m("rows"):
            rows = self.rows
            # adjust number of rows
            delta = len(rows) - self.widget.GetNumberRows()
            if   delta>0: self.widget.AppendRows(delta)
            elif delta<0: self.widget.DeleteRows(0, -delta)
            # set row heights and labels
            for i, (label,size) in enumerate(rows):
                size = int(size or "0") 
                if size>0:
                    self.widget.SetRowSize(i, size)
                self.widget.SetRowLabelValue(i, label.replace('\\n', '\n'))

        self.widget.ForceRefresh()

    def properties_changed(self, modified):
        self._update_widget_properties(modified)
        ManagedBase.properties_changed(self, modified)

    def get_property_handler(self, name):
        if name == 'columns': return ColsHandler(self)
        if name == 'rows': return RowsHandler(self)
        return ManagedBase.get_property_handler(self, name)



def builder(parent, sizer, pos, number=[1]):
    "factory function for EditGrid objects"
    label = 'grid_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'grid_%d' % number[0]
    with parent.frozen():
        grid = EditGrid(label, parent, wx.NewId(), sizer, pos)
        # A grid should be wx.EXPANDed and 'option' should be 1, or you can't see it.
        grid.properties["proportion"].set(1)
        grid.properties["flag"].set("wxEXPAND")
        node = Node(grid)
        grid.node = node
        if parent.widget: grid.create()
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    "factory to build EditGrid objects from a XML file"
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    grid = EditGrid(label, parent, wx.NewId(), sizer, pos)
    #sizer.set_item(grid.pos, proportion=sizeritem.proportion, span=sizeritem.span, flag=sizeritem.flag, border=sizeritem.border)
    node = Node(grid)
    grid.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos-1)
    return grid


def initialize():
    "initialization function for the module: returns a wx.BitmapButton to be added to the main palette."
    common.widgets['EditGrid'] = builder
    common.widgets_from_xml['EditGrid'] = xml_builder

    return common.make_object_button('EditGrid', 'grid.xpm')

