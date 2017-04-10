"""\
Lisp generator functions for wxGrid objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from .codegen import ColsCodeHandler, _check_label


class LispCodeGenerator(wcodegen.LispWidgetCodeWriter):

    def get_code(self, obj):
        id_name, id = self.codegen.generate_code_id(obj)
        parent = self.format_widget_access(obj.parent)
        init = []
        if id_name:
            init.append(id_name)

        init.append('(setf (slot-%s obj) (wxGrid_Create %s %s -1 -1 -1 -1 wxWANTS_CHARS))\n' % (obj.name, parent, id))
        props_buf = self.get_properties_code(obj)
        return init, props_buf, []

    def get_properties_code(self, obj):
        out = []
        name = obj.name

        prop = obj.properties

        try: create_grid = int(prop['create_grid'])
        except (KeyError, ValueError): create_grid = False
        if not create_grid: return []

        columns = prop.get('columns', [['A', '-1']])
        out.append('(wxGrid_CreateGrid (slot-%s obj) %s %s 0)\n' % (name, prop.get('rows_number', '1'), len(columns)))
        if prop.get('row_label_size'):
            out.append('(wxGrid_SetRowLabelSize (slot-%s obj) %s)\n' % (name, prop['row_label_size']))
        if prop.get('col_label_size'):
            out.append('(wxGrid_SetColLabelSize (slot-%s obj) %s)\n' % (name, prop['col_label_size']))
        enable_editing = prop.get('enable_editing', '1')
        if enable_editing != '1':
            out.append('(wxGrid_EnableEditing (slot-%s obj) 0)\n' % name)
        enable_grid_lines = prop.get('enable_grid_lines', '1')
        if enable_grid_lines != '1':
            out.append('(wxGrid_EnableGridLines (slot-%s obj) 0)\n' % name)
        enable_col_resize = prop.get('enable_col_resize', '1')
        if enable_col_resize != '1':
            out.append('(wxGrid_EnableDragColSize (slot-%s obj) 0)\n' % name)
        enable_row_resize = prop.get('enable_row_resize', '1')
        if enable_row_resize != '1':
            out.append('(wxGrid_EnableDragRowSize (slot-%s obj) 0)\n' % name)
        enable_grid_resize = prop.get('enable_grid_resize', '1')
        if enable_grid_resize != '1':
            out.append('(wxGrid_EnableDragGridSize (slot-%s obj) 0)\n' % name)
        if prop.get('lines_color', False):
            out.append('(wxGrid_SetGridLineColour (slot-%s obj) (wxColour:wxColour_CreateFromStock %s))\n' %
                       (name, self.codegen._string_to_colour(prop['lines_color'])))
        if prop.get('label_bg_color', False):
            out.append('(wxGrid_SetLabelBackgroundColour (slot-%s obj) (wxColour:wxColour_CreateFromStock %s))\n'
                       %(name, self.codegen._string_to_colour(prop['label_bg_color'])))

        sel_mode = prop.get('selection_mode')

        if sel_mode and sel_mode != 'wxGridSelectCells':
            out.append('(wxGrid_SetSelectionMode (slot-%s obj) %s)\n' % (name, sel_mode.replace('wxGrid.','')))

        i = 0
        for label, size in columns:
            if _check_label(label, i):
                out.append('(wxGrid_SetColLabelValue (slot-%s obj) %s %s)\n' % (name, i, self.codegen.quote_str(label)))
            try:
                if int(size) > 0:
                    out.append('(wxGrid_SetColSize (slot-%s obj) %s %s)\n' % (name, i, size))
            except ValueError: pass
            i += 1

        out.extend(self.codegen.generate_common_properties(obj))
        return out

# end of class LispCodeGenerator


def initialize():
    klass = 'wxGrid'
    common.class_names['EditGrid'] = klass
    common.register('lisp', klass, LispCodeGenerator(klass),
                    'columns', ColsCodeHandler)
