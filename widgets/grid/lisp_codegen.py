"""\
Lisp generator functions for wxGrid objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@copyright: 2017 Dietmar Schwertberger
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class LispCodeGenerator(wcodegen.LispWidgetCodeWriter):

    def get_code(self, obj):
        id_name, id = self.codegen.generate_code_id(obj)
        parent = self.format_widget_access(obj.parent_window)
        init = []
        if id_name:
            init.append(id_name)

        obj_name = self.codegen._format_name(obj.name)
        init.append('(setf (slot-%s obj) (wxGrid_Create %s %s -1 -1 -1 -1 wxWANTS_CHARS))\n' % (obj_name, parent, id))
        init += self.get_properties_code(obj)
        return init, []

    def get_properties_code(self, obj):
        if not obj.create_grid: return []

        codegen = self.codegen
        out = []
        name = codegen._format_name(obj.name)

        rows_p = obj.properties["rows"]
        cols_p = obj.properties["columns"]
        rows    = rows_p.value
        columns = cols_p.value
        out.append('(wxGrid_CreateGrid (slot-%s obj) %s %s 0)\n' % (name, len(rows), len(columns)))

        if obj.check_prop('row_label_size'):  out.append('(wxGrid_SetRowLabelSize (slot-%s obj) %s)\n' % (name, obj.row_label_size))
        if obj.check_prop('col_label_size'):  out.append('(wxGrid_SetColLabelSize (slot-%s obj) %s)\n' % (name, obj.col_label_size))

        if obj.check_prop('label_font'): out.append(codegen.generate_code_font(obj, 'label_font', 'SetLabelFont'))
        if obj.check_prop('cell_font'):  out.append(codegen.generate_code_font(obj, 'cell_font',  'SetDefaultCellFont'))

        if not obj.enable_editing:      out.append('(wxGrid_EnableEditing (slot-%s obj) 0)\n' % name)
        if not obj.enable_grid_lines:   out.append('(wxGrid_EnableGridLines (slot-%s obj) 0)\n' % name)
        if not obj.enable_col_resize:   out.append('(wxGrid_EnableDragColSize (slot-%s obj) 0)\n' % name)
        if not obj.enable_row_resize:   out.append('(wxGrid_EnableDragRowSize (slot-%s obj) 0)\n' % name)
        if not obj.enable_grid_resize : out.append('(wxGrid_EnableDragGridSize (slot-%s obj) 0)\n' % name)
        if obj.check_prop('lines_color'):
            fmt = '(wxGrid_SetGridLineColour (slot-%s obj) (wxColour:wxColour_CreateFromStock %s))\n'
            out.append( fmt % (name, codegen._string_to_colour(obj.lines_color)) )
        if obj.check_prop('label_bg_color'):
            fmt = '(wxGrid_SetLabelBackgroundColour (slot-%s obj) (wxColour:wxColour_CreateFromStock %s))\n'
            out.append( fmt % (name, codegen._string_to_colour(obj.label_bg_color)) )

        sel_mode = obj.properties['selection_mode'].get_string_value()
        if sel_mode and sel_mode != 'wxGrid.wxGridSelectCells':
            out.append('(wxGrid_SetSelectionMode (slot-%s obj) %s)\n' % (name, sel_mode.replace('wxGrid.','')))

        # set columns
        for i, (label, size) in enumerate(columns):
            if cols_p._check_label(label, i):
                label = label.replace('\\n', '\n')
                out.append('(wxGrid_SetColLabelValue (slot-%s obj) %s %s)\n' % (name, i, codegen.quote_str(label)))
            if size>0:
                out.append('(wxGrid_SetColSize (slot-%s obj) %s %s)\n' % (name, i, size))
        # set rows
        for i, (label, size) in enumerate(rows):
            if rows_p._check_label(label, i):
                label = label.replace('\\n', '\n')
                out.append('(wxGrid_SetRowLabelValue (slot-%s obj) %s %s)\n' % (name, i, codegen.quote_str(label)))
            if size>0:
                out.append('(wxGrid_SetRowSize (slot-%s obj) %s %s)\n' % (name, i, size))

        out.extend(codegen.generate_code_common_properties(obj))
        return out


def initialize():
    klass = 'wxGrid'
    common.class_names['EditGrid'] = klass
    common.register('lisp', klass, LispCodeGenerator(klass) )
