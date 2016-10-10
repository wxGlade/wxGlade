"""
Code generator functions for wxGrid objects

@copyright: 2002-2007 Alberto Griggio
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from wcodegen.taghandler import BaseCodeWriterTagHandler


class ColsCodeHandler(BaseCodeWriterTagHandler):

    def __init__(self):
        super(ColsCodeHandler, self).__init__()
        self.columns = []
        self.col_name = ''
        self.col_size = ''

    def start_elem(self, name, attrs):
        if name == 'column':
            s = attrs.get('size', '')
            self.col_size = s
            self._content = []

    def end_elem(self, name, code_obj):
        if name == 'columns':
            code_obj.properties['columns'] = self.columns
            return True
        elif name == 'column':
            char_data = self.get_char_data()
            self.columns.append([char_data, self.col_size])
        return False



def _check_label(label, col):
    """Checks if 'label' is not the default one for the columns 'col':
    returns True if the label is a custom one, False otherwise"""
    # build the default value
    s = []
    while True:
        s.append(chr(ord('A') + col % 26))
        col = col/26 - 1
        if col < 0: break
    s.reverse()
    # then compare it with label
    return label != "".join(s)


class PythonCodeGenerator(wcodegen.PythonWidgetCodeWriter):

    import_modules = ['import wx.grid\n']

    def cn(self, c):
        #self._logger.debug('PythonStaticTextGenerator.cn with arg:', c)
        # TODO remove ugly hack for wxColour
        if c == 'wxColour':
            return wcodegen.PythonWidgetCodeWriter.cn(self, c)
        if c[:2] == 'wx':
            c = c[2:]
        return 'wx.grid.' + c

    def get_code(self, obj):
        id_name, id = self.codegen.generate_code_id(obj)
        parent = self.format_widget_access(obj.parent)
        init = []
        if id_name:
            init.append(id_name)
        klass = obj.klass
        if klass == obj.base:
            klass = self.cn(klass)
        init.append('self.%s = %s(%s, %s, size=(1, 1))\n' % (obj.name, klass, parent, id))
        props_buf = self.get_properties_code(obj)
        return init, props_buf, []

    def get_properties_code(self, obj):
        out = []
        name = self.format_widget_access(obj)
        prop = obj.properties

        try:
            create_grid = int(prop['create_grid'])
        except (KeyError, ValueError):
            create_grid = False
        if not create_grid: return []

        columns = prop.get('columns', [['A', '-1']])
        out.append('%s.CreateGrid(%s, %s)\n' % (name, prop.get('rows_number', '1'), len(columns)))
        if prop.get('row_label_size'):
            out.append('%s.SetRowLabelSize(%s)\n' % (name, prop['row_label_size']))
        if prop.get('col_label_size'):
            out.append('%s.SetColLabelSize(%s)\n' % (name, prop['col_label_size']))
        enable_editing = prop.get('enable_editing', '1')
        if enable_editing != '1':
            out.append('%s.EnableEditing(0)\n' % name)
        enable_grid_lines = prop.get('enable_grid_lines', '1')
        if enable_grid_lines != '1':
            out.append('%s.EnableGridLines(0)\n' % name)
        enable_col_resize = prop.get('enable_col_resize', '1')
        if enable_col_resize != '1':
            out.append('%s.EnableDragColSize(0)\n' % name)
        enable_row_resize = prop.get('enable_row_resize', '1')
        if enable_row_resize != '1':
            out.append('%s.EnableDragRowSize(0)\n' % name)
        enable_grid_resize = prop.get('enable_grid_resize', '1')
        if enable_grid_resize != '1':
            out.append('%s.EnableDragGridSize(0)\n' % name)
        if prop.get('lines_color', False):
            out.append(('%s.SetGridLineColour(' + self.cn('wxColour') + '(%s))\n') %
                       (name, self.codegen._string_to_colour(prop['lines_color'])))
        if prop.get('label_bg_color', False):
            out.append(('%s.SetLabelBackgroundColour(' + self.cn('wxColour') + '(%s))\n') %
                       (name, self.codegen._string_to_colour(prop['label_bg_color'])))
        sel_mode = prop.get('selection_mode')
        if sel_mode and sel_mode != 'wxGrid.wxGridSelectCells':
            out.append('%s.SetSelectionMode(%s)\n' % (name, self.cn('wxGrid') + sel_mode[6:]))

        i = 0
        for label, size in columns:
            if _check_label(label, i):
                out.append('%s.SetColLabelValue(%s, %s)\n' % (name, i, self.codegen.quote_str(label)))
            try:
                if int(size) > 0:
                    out.append('%s.SetColSize(%s, %s)\n' % (name, i, size))
            except ValueError: pass
            i += 1

        out.extend(self.codegen.generate_common_properties(obj))
        return out



class CppCodeGenerator(wcodegen.CppWidgetCodeWriter):
    import_modules = ['<wx/grid.h>']

    def get_code(self, obj):
        "generates C++ code for wxGrid objects."
        id_name, id = self.codegen.generate_code_id(obj)
        ids = [id_name]  if id_name else  []
        parent = self.format_widget_access(obj.parent)
        init = ['%s = new %s(%s, %s);\n' % (obj.name, obj.klass, parent, id)]
        props_buf = self.get_properties_code(obj)
        return init, ids, props_buf, []

    def get_properties_code(self, obj):
        out = []
        name = 'this'
        if not obj.is_toplevel: name = obj.name
        prop = obj.properties

        try:
            create_grid = int(prop['create_grid'])
        except (KeyError, ValueError):
            create_grid = False
        if not create_grid:
            return []

        columns = prop.get('columns', [['A', '-1']])
        out.append('%s->CreateGrid(%s, %s);\n' % (name, prop.get('rows_number', '1'), len(columns)))
        if prop.get('row_label_size'):
            out.append('%s->SetRowLabelSize(%s);\n' % (name, prop['row_label_size']))
        if prop.get('col_label_size'):
            out.append('%s->SetColLabelSize(%s);\n' % (name, prop['col_label_size']))
        enable_editing = prop.get('enable_editing', '1')
        if enable_editing != '1':
            out.append('%s->EnableEditing(false);\n' % name)
        enable_grid_lines = prop.get('enable_grid_lines', '1')
        if enable_grid_lines != '1':
            out.append('%s->EnableGridLines(false);\n' % name)
        enable_col_resize = prop.get('enable_col_resize', '1')
        if enable_col_resize != '1':
            out.append('%s->EnableDragColSize(false);\n' % name)
        enable_row_resize = prop.get('enable_row_resize', '1')
        if enable_row_resize != '1':
            out.append('%s->EnableDragRowSize(false);\n' % name)
        enable_grid_resize = prop.get('enable_grid_resize', '1')
        if enable_grid_resize != '1':
            out.append('%s->EnableDragGridSize(false);\n' % name)
        if prop.get('lines_color', False):
            out.append('%s->SetGridLineColour(wxColour(%s));\n' %
                       (name, self.codegen._string_to_colour(prop['lines_color'])))
        if prop.get('label_bg_color', False):
            out.append('%s->SetLabelBackgroundColour(wxColour(%s));\n' %
                       (name, self.codegen._string_to_colour(prop['label_bg_color'])))
        sel_mode = prop.get('selection_mode', '').replace('.', '::')
        if sel_mode and sel_mode != 'wxGrid::wxGridSelectCells':
            out.append('%s->SetSelectionMode(%s);\n' % (name, sel_mode))

        i = 0
        for label, size in columns:
            if _check_label(label, i):
                out.append('%s->SetColLabelValue(%s, %s);\n' % (name, i, self.codegen.quote_str(label)))
            try:
                if int(size) > 0:
                    out.append('%s->SetColSize(%s, %s);\n' % (name, i, size))
            except ValueError:
                pass
            i += 1

        out.extend(self.codegen.generate_common_properties(obj))
        return out



def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class GridXrcObject(xrcgen.DefaultXrcObject):
        unsupported = set(['column', 'create_grid', 'rows_number', 'row_label_size', 'col_label_size',
                           'enable_editing', 'enable_grid_lines', 'enable_col_resize', 'enable_row_resize',
                           'enable_grid_resize', 'lines_color', 'label_bg_color', 'selection_mode'])

        def write_property(self, name, val, outfile, tabs):
            if name not in self.unsupported:
                xrcgen.DefaultXrcObject.write_property(self, name, val, outfile, tabs)
    return GridXrcObject(obj)


def initialize():
    klass = 'wxGrid'
    common.class_names['EditGrid'] = klass
    common.register('python', klass, PythonCodeGenerator(klass), 'columns', ColsCodeHandler, klass)
    common.register('C++',    klass, CppCodeGenerator(klass),    'columns', ColsCodeHandler, klass)
    common.register('XRC',    klass, xrc_code_generator)
