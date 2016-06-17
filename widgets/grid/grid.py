"""\
wxGrid objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import wx
from wx.grid import *
import common
import compat
import misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *
from wcodegen.taghandler import BaseXmlBuilderTagHandler


class GridColsProperty(GridProperty):
    def write(self, outfile, tabs):
        value = self.get_value()
        inner_xml = u''
        for label, size in value:
            inner_xml += common.format_xml_tag(
                u'column', label, tabs + 1, size=size)
        stmt = common.format_xml_tag(u'columns', inner_xml, tabs, is_xml=True)
        outfile.write(stmt)

    def _get_label(self, col):
        s = []
        while True:
            s.append(chr(ord('A') + col%26))
            col = col/26 - 1
            if col < 0: break
        s.reverse()
        return "".join(s)

    def add_row(self, event):
        GridProperty.add_row(self, event)
        label = self._get_label(self.rows-1)
        self.grid.SetCellValue(self.rows-1, 0, label)
        self.grid.SetCellValue(self.rows-1, 1, '-1')

    def insert_row(self, event):
        GridProperty.insert_row(self, event)
        label = self._get_label(self.cur_row)
        self.grid.SetCellValue(self.cur_row, 0, label)
        self.grid.SetCellValue(self.cur_row, 1, '-1')

# end of class GridColumnsProperty


class ColsHandler(BaseXmlBuilderTagHandler):

    def __init__(self, parent):
        super(ColsHandler, self).__init__()
        self.parent = parent
        self.columns = []
        self.curr_col = []
        self.curr_size = '-1'

    def start_elem(self, name, attrs):
        if name == 'column':
            self.curr_size = attrs.get('size', '-1')

    def end_elem(self, name):
        if name == 'columns':
            self.parent.set_columns(self.columns)
            self.parent.properties['columns'].set_value(self.columns)
            return True
        elif name == 'column':
            char_data = self.get_char_data()
            self.columns.append([char_data, self.curr_size])
        return False

# end of class ColsHandler


class EditGrid(ManagedBase):

    def __init__(self, name, parent, id, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxGrid objects
        """
        # values of properties for the grid:
        self.row_label_size = 30
        self.col_label_size = 30
        self.enable_editing = True
        self.enable_grid_lines = True
        self.rows_number = 10
        self.enable_col_resize = True
        self.enable_row_resize = True
        self.enable_grid_resize = True
        self.lines_color = '#000000'
        self.label_bg_color = '#C0C0C0'
        self.selection_mode = 0  # == wxGrid.wxGridSelectCells
        self.create_grid = True
        self.columns = [['A', '-1'], ['B', '-1'], ['C', '-1']]

        ManagedBase.__init__(self, name, 'wxGrid', parent, id, sizer, pos,
                             property_window, show=show)
        props = self.properties
        af = self.access_functions
        af['create_grid'] = (self.get_create_grid, self.set_create_grid)
        props['create_grid'] = CheckBoxProperty(
            self, 'create_grid', write_always=True, label=_("Create grid"))
        af['row_label_size'] = (self.get_row_label_size,
                                self.set_row_label_size)
        props['row_label_size'] = SpinProperty(
            self, 'row_label_size', None, can_disable=True,
            label=_("Row label size"), omitter='create_grid')
        af['col_label_size'] = (self.get_col_label_size,
                                self.set_col_label_size)
        props['col_label_size'] = SpinProperty(
            self, 'col_label_size', None, can_disable=True,
            label=_("Column label size"), omitter='create_grid')
        af['enable_editing'] = (self.get_enable_editing,
                                self.set_enable_editing)
        props['enable_editing'] = CheckBoxProperty(
            self, 'enable_editing', None, write_always=True,
            label=_("Enable editing"), omitter='create_grid')
        af['enable_grid_lines'] = (self.get_enable_grid_lines,
                                   self.set_enable_grid_lines)
        props['enable_grid_lines']= CheckBoxProperty(
            self, 'enable_grid_lines', None, write_always=True,
            label=_("Enable grid lines"), omitter='create_grid')
        af['rows_number'] = (self.get_rows_number, self.set_rows_number)
        props['rows_number'] = SpinProperty(
            self, 'rows_number', None,
            label=_("Number of rows"), omitter='create_grid')
        af['enable_col_resize'] = (self.get_enable_col_resize,
                                   self.set_enable_col_resize)
        props['enable_col_resize']= CheckBoxProperty(
            self, 'enable_col_resize', None, write_always=True,
            label=_("Enable resizing columns"), omitter='create_grid')
        af['enable_row_resize'] = (self.get_enable_row_resize,
                                   self.set_enable_row_resize)
        props['enable_row_resize'] = CheckBoxProperty(
            self, 'enable_row_resize', None, write_always=True,
            label=_("Enable resizing rows"), omitter='create_grid')
        af['enable_grid_resize'] = (self.get_enable_grid_resize,
                                    self.set_enable_grid_resize)
        props['enable_grid_resize'] = CheckBoxProperty(
            self, 'enable_grid_resize', None, write_always=True,
            label=_("Enable resizing grid"), omitter='create_grid')
        af['lines_color'] = (self.get_lines_color, self.set_lines_color)
        props['lines_color']= ColorDialogProperty(
            self, 'lines_color', None,
            label=_("Lines colour"), omitter='create_grid')
        af['label_bg_color'] = (self.get_label_bg_color,
                                self.set_label_bg_color)
        props['label_bg_color']= ColorDialogProperty(
            self, 'label_bg_color', None,
            label=_("Label background colour"), omitter='create_grid')
        af['selection_mode'] = (self.get_selection_mode,
                                self.set_selection_mode)
        props['selection_mode'] = RadioProperty(
            self, 'selection_mode', None,
            ['wxGrid.wxGridSelectCells',
             'wxGrid.wxGridSelectRows',
             'wxGrid.wxGridSelectColumns'],
            label=_("Selection mode"), omitter='create_grid')
        af['columns'] = (self.get_columns, self.set_columns)
        props['columns'] = GridColsProperty(
            self, 'columns', None,
            [('Label', GridProperty.STRING), ('Size', GridProperty.INT)],
            label=_("Columns"), omitter='create_grid')

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wx.ScrolledWindow(self.notebook, -1, style=wx.TAB_TRAVERSAL)
        self.properties['create_grid'].display(panel)
        self.properties['columns'].display(panel)
        self.properties['rows_number'].display(panel)
        self.properties['row_label_size'].display(panel)
        self.properties['col_label_size'].display(panel)
        self.properties['enable_editing'].display(panel)
        self.properties['enable_grid_lines'].display(panel)
        self.properties['enable_col_resize'].display(panel)
        self.properties['enable_row_resize'].display(panel)
        self.properties['enable_grid_resize'].display(panel)
        self.properties['lines_color'].display(panel)
        self.properties['label_bg_color'].display(panel)
        self.properties['selection_mode'].display(panel)
        self.toggle_blocked_properties()
        szr = wx.BoxSizer(wx.VERTICAL)
        szr.Add(self.properties['create_grid'].panel, 0, wx.EXPAND)
        szr.Add(wx.StaticLine(panel, -1), 0, wx.ALL|wx.EXPAND, 5)
        szr.Add(wx.StaticText(panel, -1, _("The following properties are "
                             "meaningful\nonly if 'Create grid' is selected")),
                0, wx.LEFT|wx.RIGHT|wx.EXPAND, 10)
        szr.Add(wx.StaticLine(panel, -1), 0, wx.ALL|wx.EXPAND, 5)
        szr.Add(self.properties['columns'].panel, 0, wx.ALL|wx.EXPAND, 2)
        szr.SetItemMinSize(self.properties['columns'].panel, 1, 150)
        szr.Add(self.properties['rows_number'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['row_label_size'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['col_label_size'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['lines_color'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['label_bg_color'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['enable_editing'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['enable_grid_lines'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['enable_col_resize'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['enable_row_resize'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['enable_grid_resize'].panel, 0, wx.EXPAND)
        szr.Add(self.properties['selection_mode'].panel, 0, wx.ALL|wx.EXPAND, 5)
        panel.SetAutoLayout(1)
        compat.SizerItem_SetSizer(panel, szr)
        szr.Fit(panel)
        w, h = panel.GetClientSize()
        self.notebook.AddPage(panel, _('Widget'))
        import math
        panel.SetScrollbars(1, 5, 1, int(math.ceil(h/5.0)))
        self.properties['columns'].set_col_sizes([-1, 0])

    def create_widget(self):
        self.widget = Grid(self.parent.widget, self.id, (200, 200))
        self.widget.CreateGrid(self.rows_number, len(self.columns))
        if not self.properties['label_bg_color'].is_active():
            self.label_bg_color = misc.color_to_string(
                self.widget.GetLabelBackgroundColour())
            self.properties['label_bg_color'].set_value(self.label_bg_color)
        if not self.properties['lines_color'].is_active():
            self.lines_color = misc.color_to_string(
                self.widget.GetGridLineColour())
            self.properties['lines_color'].set_value(self.lines_color)
        self.widget.SetRowLabelSize(self.row_label_size)
        self.widget.SetColLabelSize(self.col_label_size)
        self.widget.EnableEditing(self.enable_editing)
        self.widget.EnableGridLines(self.enable_grid_lines)
        self.widget.EnableDragColSize(self.enable_col_resize)
        self.widget.EnableDragRowSize(self.enable_row_resize)
        self.widget.EnableDragGridSize(self.enable_grid_resize)
        self.widget.SetGridLineColour(misc.string_to_color(self.lines_color))
        self.widget.SetLabelBackgroundColour(misc.string_to_color(
            self.label_bg_color))
        i = 0
        for label, size_str in self.columns:
            try:
                size_int = int(size_str)
            except:
                size_int = 0
            label = label.replace('\\n', '\n')
            self.widget.SetColLabelValue(i, label)
            if size_int > 0:
                self.widget.SetColSize(i, size_int)
            i += 1

        self.set_selection_mode(self.selection_mode)
        # following two events are to permit select grid from designer frame
        EVT_GRID_CELL_LEFT_CLICK(self.widget, self.on_set_focus)
        EVT_GRID_LABEL_LEFT_CLICK(self.widget, self.on_set_focus)
        # these are to show the popup menu on right click
        EVT_GRID_CELL_RIGHT_CLICK(self.widget, self.popup_menu)
        EVT_GRID_LABEL_RIGHT_CLICK(self.widget, self.popup_menu)

    def toggle_blocked_properties(self):
        to_block = not self.get_create_grid()
        for name in self.get_property_blocking('create_grid'):
            widget = self.properties[name]
            if getattr(widget, 'toggle_blocked', None):
                widget.toggle_blocked(to_block)

    def get_create_grid(self):
        return self.create_grid

    def set_create_grid(self, value):
        self.create_grid = bool(int(value))
        self.toggle_blocked_properties()

    def get_row_label_size(self):
        return self.row_label_size

    def set_row_label_size(self, value):
        self.row_label_size = int(value)
        if value and self.widget:
            self.widget.SetRowLabelSize(self.row_label_size)

    def get_col_label_size(self):
        return self.col_label_size

    def set_col_label_size(self, value):
        self.col_label_size = int(value)
        if value and self.widget:
            self.widget.SetColLabelSize(self.col_label_size)

    def get_enable_editing(self):
        return self.enable_editing

    def set_enable_editing(self, value):
        self.enable_editing = bool(int(value))
        # Do nothing.
##        if value and self.widget:
##            self.widget.EnableEditing(self.enable_editing) # NO!

    def get_enable_grid_lines(self):
        return self.enable_grid_lines

    def set_enable_grid_lines(self, value):
        self.enable_grid_lines = bool(int(value))
        if self.widget:
            self.widget.EnableGridLines(self.enable_grid_lines)
            #self.widget.Update()

    def get_rows_number(self):
        return self.rows_number

    def set_rows_number(self, value):
        self.rows_number = int(value)     # the value the user entered
        if value > 0 and self.widget:
            # the value that the grid has
            actual_rows_number = self.widget.GetNumberRows()
            if self.rows_number > actual_rows_number:
                # we have to add rows
                self.widget.AppendRows(self.rows_number - actual_rows_number)
            if actual_rows_number > self.rows_number:
                # we have to delete rows
                self.widget.DeleteRows(self.rows_number,
                                       actual_rows_number - self.rows_number)
            #self.widget.Update()

    def get_enable_col_resize(self):
        return self.enable_col_resize

    def set_enable_col_resize(self, value):
        self.enable_col_resize = bool(int(value))
        if self.widget:
            self.widget.EnableDragColSize(self.enable_col_resize)

    def get_enable_row_resize(self):
        return self.enable_row_resize

    def set_enable_row_resize(self, value):
        self.enable_row_resize = bool(int(value))
        if self.widget:
            self.widget.EnableDragRowSize(self.enable_row_resize)

    def get_enable_grid_resize(self):
        return self.enable_grid_resize

    def set_enable_grid_resize(self, value):
        self.enable_grid_resize = bool(int(value))
        if self.widget:
            self.widget.EnableDragGridSize(self.enable_grid_resize)

    def get_lines_color(self):
        return self.lines_color

    def set_lines_color(self, value):
        self.lines_color = str(value)
        if self.widget:
            self.widget.SetGridLineColour(misc.string_to_color(
                self.lines_color))

    def get_label_bg_color(self):
        return self.label_bg_color

    def set_label_bg_color(self, value):
        self.label_bg_color = str(value)
        if self.widget:
            self.widget.SetLabelBackgroundColour(misc.string_to_color(
                self.label_bg_color))

    def get_selection_mode(self):
        return self.selection_mode

    def set_selection_mode(self, value):
        _sel_modes = {
            'wxGrid.wxGridSelectCells': 0,
            'wxGrid.wxGridSelectRows': 1,
            'wxGrid.wxGridSelectColumns': 2,
            }
        if value in _sel_modes:
            self.selection_mode = _sel_modes[value]
        else:
            try:
                value = int(value)
            except:
                pass
            else:
                self.selection_mode = value

    def get_columns(self):
        return self.columns

    def set_columns(self, cols):
        # first of all, adjust col number
        _oldcolnum = len(self.columns)
        _colnum = len(cols)
        self.columns = cols
        if not self.widget: return
        if _colnum > _oldcolnum:
            self.widget.AppendCols(_colnum - _oldcolnum)
        if _colnum < _oldcolnum:
            self.widget.DeleteCols(0, _oldcolnum - _colnum)
        i = 0
        for label, size_str in cols:
            try:
                size_int = int(size_str)
            except:
                size_int = 0
            label = label.replace('\\n', '\n')
            self.widget.SetColLabelValue(i, misc.wxstr(label))
            if size_int > 0:
                self.widget.SetColSize(i, size_int)
            i += 1
        self.widget.ForceRefresh()

    def get_property_handler(self, name):
        if name == 'columns': return ColsHandler(self)
        return ManagedBase.get_property_handler(self, name)

# end of class EditGrid


def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditGrid objects.
    """
    label = 'grid_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'grid_%d' % number[0]
    grid = EditGrid(label, parent, wx.NewId(), sizer, pos,
                    common.property_panel)
    # A grid should be wx.EXPANDed and 'option' should be 1,
    # or you can't see it.
    grid.set_option(1)
    grid.esm_border.set_style("wxEXPAND")
    node = Tree.Node(grid)
    grid.node = node
    grid.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos - 1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditGrid objects from a XML file
    """
    from xml_parse import XmlParsingError
    try:
        label = attrs['name']
    except KeyError:
        raise XmlParsingError(_("'name' attribute missing"))
    if sizer is None or sizeritem is None:
        raise XmlParsingError(_("sizer or sizeritem object cannot be None"))
    grid = EditGrid(label, parent, wx.NewId(), sizer,
                    pos, common.property_panel, show=False)
    sizer.set_item(grid.pos, option=sizeritem.option, flag=sizeritem.flag,
                   border=sizeritem.border)
    node = Tree.Node(grid)
    grid.node = node
    if pos is None:
        common.app_tree.add(node, sizer.node)
    else:
        common.app_tree.insert(node, sizer.node, pos - 1)
    return grid


def initialize():
    """\
    initialization function for the module: returns a wx.BitmapButton to be
    added to the main palette.
    """
    common.widgets['EditGrid'] = builder
    common.widgets_from_xml['EditGrid'] = xml_builder

    return common.make_object_button('EditGrid', 'grid.xpm')

