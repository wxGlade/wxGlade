# Grid.py: wxGrid objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

from wxPython.wx import *
from wxPython.grid import *
import common, misc
from edit_windows import ManagedBase
from tree import Tree
from widget_properties import *


class EditGrid(ManagedBase):
    def __init__(self, name, parent, id, sizer, pos, property_window,
                 show=True):
        """\
        Class to handle wxGrid objects
        """
        # values of properties for the grid:
        self.row_label_size = 30
        self.col_label_size = 20
        self.enable_editing = False
        self.enable_grid_lines = True
        self.rows_number = 10
        self.columns_number = 3
        self.enable_col_resize = True
        self.enable_row_resize = True
        self.enable_grid_resize = False
        self.lines_color = '#000000'
        
        ManagedBase.__init__(self, name, 'wxGrid', parent, id, sizer, pos,
                             property_window, show=show)
        self.access_functions['row_label_size'] = (self.get_row_label_size,
                                                 self.set_row_label_size)
        self.properties['row_label_size'] = SpinProperty(self, 'row_label_size',
                                                       None, can_disable=False)
        self.access_functions['col_label_size'] = (self.get_col_label_size,
                                                 self.set_col_label_size)
        self.properties['col_label_size'] = SpinProperty(self, 'col_label_size',
                                                       None,can_disable=False)
        self.access_functions['enable_editing'] = (self.get_enable_editing,
                                                  self.set_enable_editing)
        self.properties['enable_editing'] = CheckBoxProperty(self,
                                                            'enable_editing',
                                                            None)
        self.access_functions['enable_grid_lines'] = (self.get_enable_grid_lines,
                                                    self.set_enable_grid_lines)
        self.properties['enable_grid_lines']= CheckBoxProperty(self,
                                                             'enable_grid_lines',
                                                             None)
        self.access_functions['columns_number'] = (self.get_columns_number,
                                              self.set_columns_number)
        self.properties['columns_number'] = SpinProperty(self, 'columns_number', None)
        self.access_functions['rows_number'] = (self.get_rows_number,
                                              self.set_rows_number)
        self.properties['rows_number'] = SpinProperty(self, 'rows_number', None)
        self.access_functions['enable_col_resize'] = (self.get_enable_col_resize,
                                                    self.set_enable_col_resize)
        self.properties['enable_col_resize']= CheckBoxProperty(self,
                                                             'enable_col_resize',
                                                             None)
        self.access_functions['enable_row_resize'] = (self.get_enable_row_resize,
                                                    self.set_enable_row_resize)
        self.properties['enable_row_resize']= CheckBoxProperty(self,
                                                             'enable_row_resize',
                                                             None)
        self.access_functions['enable_grid_resize'] = (self.get_enable_grid_resize,
                                                    self.set_enable_grid_resize)
        self.properties['enable_grid_resize']= CheckBoxProperty(self,
                                                             'enable_grid_resize',
                                                             None)
        self.access_functions['lines_color'] = (self.get_lines_color,
                                                    self.set_lines_color)
        self.properties['lines_color']= ColorDialogProperty(self,
                                                             'lines_color',
                                                             None)

    def create_properties(self):
        ManagedBase.create_properties(self)
        panel = wxPanel(self.notebook, -1)
        self.properties['row_label_size'].display(panel)
        self.properties['col_label_size'].display(panel)
        self.properties['enable_editing'].display(panel)
        self.properties['enable_grid_lines'].display(panel)
        self.properties['columns_number'].display(panel)
        self.properties['rows_number'].display(panel)
        self.properties['enable_col_resize'].display(panel)
        self.properties['enable_row_resize'].display(panel)
        self.properties['enable_grid_resize'].display(panel)
        self.properties['lines_color'].display(panel)
        szr = wxBoxSizer(wxVERTICAL)
        szr.Add(self.properties['columns_number'].panel, 0, wxEXPAND)
        szr.Add(self.properties['rows_number'].panel, 0, wxEXPAND)
        szr.Add(self.properties['row_label_size'].panel, 0, wxEXPAND)
        szr.Add(self.properties['col_label_size'].panel, 0, wxEXPAND)
        szr.Add(self.properties['lines_color'].panel, 0, wxEXPAND)
        szr.Add(self.properties['enable_editing'].panel, 0, wxEXPAND)
        szr.Add(self.properties['enable_grid_lines'].panel, 0, wxEXPAND)
        szr.Add(self.properties['enable_col_resize'].panel, 0, wxEXPAND)
        szr.Add(self.properties['enable_row_resize'].panel, 0, wxEXPAND)
        szr.Add(self.properties['enable_grid_resize'].panel, 0, wxEXPAND)
        panel.SetAutoLayout(1)
        panel.SetSizer(szr)
        szr.Fit(panel)
        self.notebook.AddPage(panel, 'Widget')

    def create_widget(self):
        self.widget = wxGrid(self.parent.widget, self.id,(200,200))
        self.widget.CreateGrid(self.rows_number, self.columns_number)
        self.widget.SetRowLabelSize(self.row_label_size)
        self.widget.SetColLabelSize(self.col_label_size)
        self.widget.EnableEditing(self.enable_editing)
        self.widget.EnableGridLines(self.enable_grid_lines)
        self.widget.EnableDragColSize(self.enable_col_resize)
        self.widget.EnableDragRowSize(self.enable_row_resize)
        self.widget.EnableDragGridSize(self.enable_grid_resize)
        self.widget.SetGridLineColour(misc.string_to_color(self.lines_color))
        # A grid should be wxEXPANDed and 'option' should be 1,
        # or you can't see it.
        self.set_option(1)  
        self.set_flag("wxEXPAND")
        # following two events are to permit select grid from designer frame
        EVT_GRID_CELL_LEFT_CLICK(self.widget, self.on_set_focus)  
        EVT_GRID_LABEL_LEFT_CLICK(self.widget, self.on_set_focus)
    
    def click_on_grid(self, e):
        print "click\n"
        self.update_view(True)


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
        self.enable_editing = bool(value)
        # Do nothing.
##        if value and self.widget:
##            self.widget.EnableEditing(self.enable_editing) # NO!

    def get_enable_grid_lines(self):
        return self.enable_grid_lines

    def set_enable_grid_lines(self, value):
        self.enable_grid_lines = bool(value)
        if self.widget:
            self.widget.EnableGridLines(self.enable_grid_lines)
            #self.widget.Update()

    def get_columns_number(self):
        return self.columns_number

    def set_columns_number(self, value):
        self.columns_number = int(value)     # the value the user entered
        if value > 0 and self.widget:
            # the value that the grid has
            actual_columns_number = self.widget.GetNumberCols()
            if self.columns_number > actual_columns_number:
                # we have to add column
                self.widget.AppendCols(self.columns_number - actual_columns_number)
            if actual_columns_number > self.columns_number:
                # we have to delete column
                self.widget.DeleteCols(self.columns_number,
                                       actual_columns_number - self.columns_number)
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
        self.enable_col_resize = bool(value)
        if self.widget:
            self.widget.EnableDragColSize(self.enable_col_resize)

    def get_enable_row_resize(self):
        return self.enable_row_resize

    def set_enable_row_resize(self, value):
        self.enable_row_resize = bool(value)
        if self.widget:
            self.widget.EnableDragRowSize(self.enable_row_resize)

    def get_enable_grid_resize(self):
        return self.enable_grid_resize

    def set_enable_grid_resize(self, value):
        self.enable_grid_resize = bool(value)
        if self.widget:
            self.widget.EnableDragGridSize(self.enable_grid_resize)

    def get_lines_color(self):
        return self.lines_color

    def set_lines_color(self, value):
        self.lines_color = str(value)
        if self.widget:
            self.widget.SetGridLineColour(misc.string_to_color(self.lines_color))

# end of class EditGrid
        

def builder(parent, sizer, pos, number=[1]):
    """\
    factory function for EditGrid objects.
    """
    label = 'grid_%d' % number[0]
    while common.app_tree.has_name(label):
        number[0] += 1
        label = 'grid_%d' % number[0]
    grid = EditGrid(label, parent, wxNewId(), sizer,
                        pos, common.property_panel)
    node = Tree.Node(grid)
    grid.node = node
    grid.show_widget(True)
    common.app_tree.insert(node, sizer.node, pos-1)


def xml_builder(attrs, parent, sizer, sizeritem, pos=None):
    """\
    factory to build EditGrid objects from an xml file
    """
    from xml_parse import XmlParsingError
    try: label = attrs['name']
    except KeyError: raise XmlParsingError, "'name' attribute missing"
    if sizer is None or sizeritem is None:
        raise XmlParsingError, "sizer or sizeritem object cannot be None"
    grid = EditGrid(label, parent, wxNewId(), sizer,
                        pos, common.property_panel, show=False)
    sizer.set_item(grid.pos, option=sizeritem.option, flag=sizeritem.flag,
                    border=sizeritem.border, size=(100,100))  #HELP#
                    #border=sizeritem.border, size=grid.GetBestSize())
    node = Tree.Node(grid)
    grid.node = node
    if pos is None: common.app_tree.add(node, sizer.node)
    else: common.app_tree.insert(node, sizer.node, pos-1)
    return grid


def initialize():
    """\
    initialization function for the module: returns a wxBitmapButton to be
    added to the main palette.
    """
    common.widgets['EditGrid'] = builder
    common.widgets_from_xml['EditGrid'] = xml_builder

    return common.make_object_button('EditGrid', 'icons/grid.xpm')
