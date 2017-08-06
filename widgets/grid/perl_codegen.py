"""\
Perl generator functions for wxGrid objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from .codegen import _check_col_label, _check_row_label


class PerlCodeGenerator(wcodegen.PerlWidgetCodeWriter):

    def get_code(self, obj):
        id_name, id = self.codegen.generate_code_id(obj)
        parent = self.format_widget_access(obj.parent)
        init = []
        if id_name:
            init.append(id_name)

        klass = obj.base
        if klass != obj.klass:
            klass = obj.klass
        else:
            klass = klass.replace('wx', 'Wx::', 1)

        init.append('$self->{%s} = %s->new(%s, %s);\n' % (obj.name, klass, parent, id))
        props_buf = self.get_properties_code(obj)
        return init, props_buf, []

    def get_properties_code(self, obj):
        out = []
        name = self.format_widget_access(obj)
        prop = obj.properties

        if not obj.create_grid: return []

        rows = obj.rows
        columns = obj.columns
        out.append( '%s->CreateGrid(%s, %s);\n' % (name, len(rows), len(columns)) )

        if obj.check_prop('row_label_size'): out.append( '%s->SetRowLabelSize(%s);\n' % (name, obj.row_label_size) )
        if obj.check_prop('col_label_size'): out.append( '%s->SetColLabelSize(%s);\n' % (name, obj.col_label_size) )
        
        if not obj.enable_editing: out.append( '%s->EnableEditing(0);\n' % name )
        if not obj.enable_grid_lines: out.append( '%s->EnableGridLines(0);\n' % name )
        if not obj.enable_col_resize: out.append( '%s->EnableDragColSize(0);\n' % name )
        if not obj.enable_row_resize: out.append( '%s->EnableDragRowSize(0);\n' % name )
        if not obj.enable_grid_resize: out.append( '%s->EnableDragGridSize(0);\n' % name )
        
        if obj.check_prop('lines_color'):
            fmt = '%s->SetGridLineColour(Wx::Colour->new(%s));\n'
            out.append( fmt % (name, self.codegen._string_to_colour(obj.lines_color)) )
        if obj.check_prop('label_bg_color'):
            fmt = '%s->SetLabelBackgroundColour(Wx::Colour->new(%s));\n'
            out.append( fmt % (name, self.codegen._string_to_colour(obj.label_bg_color)) )
        sel_mode = obj.properties["selection_mode"].get_string_value()
        if sel_mode and sel_mode != 'wxGrid.wxGridSelectCells':
            out.append( '%s->SetSelectionMode(%s);\n' % (name, sel_mode.replace('wxGrid.','')) )

        # set columns
        for i, (label, size) in enumerate(columns):
            if _check_col_label(label, i):
                out.append( '%s->SetColLabelValue(%s, %s);\n' % (name, i, self.codegen.quote_str(label)) )
            try:
                if int(size) > 0:
                    out.append( '%s->SetColSize(%s, %s);\n' % (name, i, size) )
            except ValueError: pass
        # set rows
        for i, (label, size) in enumerate(rows):
            if _check_row_label(label, i):
                out.append( '%s->SetRowLabelValue(%s, %s);\n' % (name, i, self.codegen.quote_str(label)) )
            try:
                if int(size) > 0:
                    out.append( '%s->SetRowSize(%s, %s);\n' % (name, i, size) )
            except ValueError: pass

        out.extend(self.codegen.generate_common_properties(obj))
        return out


def initialize():
    klass = 'wxGrid'
    common.class_names['EditGrid'] = klass
    common.register('perl', klass, PerlCodeGenerator(klass) )
