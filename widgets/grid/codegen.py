# codegen.py: code generator functions for wxGrid objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY


import common

def python_code_generator(obj):
    """\
    fuction that generates python code for wxGrid objects.
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
##     if obj.is_toplevel:
##         l = []
##         if id_name: l.append(id_name)
##         l.append('self.%s = %s(%s, %s)\n' % (obj.name, obj.klass, parent,
##                                                  id))
##         return l, [], []    
    init = []
    if id_name: init.append(id_name)
    init.append('self.%s = %s(%s, %s)\n' % (obj.name, obj.klass, parent, id))
    props_buf = python_generate_properties(obj)
    return init, props_buf, []


def python_generate_properties(obj):
    pygen = common.code_writers['python']
    out = []
    name = 'self'
    if not obj.is_toplevel: name += '.%s' % obj.name
    prop = obj.properties
    
    try: create_grid = int(prop['create_grid'])
    except (KeyError, ValueError): create_grid = False
    if not create_grid: return []
    
    out.append('%s.CreateGrid(%s, %s)\n' % (name, prop.get('rows_number'),
                                            prop.get('columns_number')))
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
        out.append('%s.SetGridLineColour(wxColour(%s))\n' %
                   (name, pygen._string_to_colour(prop['lines_color'])))
    if prop.get('label_bg_color', False):
        out.append('%s.SetLabelBackgroundColour(wxColour(%s))\n' %
                   (name, pygen._string_to_colour(prop['label_bg_color'])))
    sel_mode = prop.get('selection_mode')
    if sel_mode and sel_mode != 'wxGrid.wxGridSelectCells':
        out.append('%s.SetSelectionMode(%s)\n' % (name, sel_mode))
    out.extend(pygen.generate_common_properties(obj))
    if prop.get('column_labels', False):
        v = common.smart_split(prop.get('column_labels'), 1)
        i = 0
        for s in v:
            if s != '' and i < prop.get('columns_number'):
                out.append('%s.SetColLabelValue(%d,\'%s\')\n' %
                           (name, i, s))
            i = i + 1

    return out


def cpp_code_generator(obj):
    """\
    fuction that generates C++ code for wxGrid objects.
    """
    cppgen = common.code_writers['C++']
    prop = obj.properties
    id_name, id = cppgen.generate_code_id(obj)
    if id_name: ids = [ id_name ]
    else: ids = []
    if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
    else: parent = 'this'
    #label = '"' + prop.get('label', '').replace('"', r'\"') + '"'
    if obj.is_toplevel:
        l = ['%s = new %s(%s, %s);\n' % (obj.name, obj.klass, parent,
                                             id)]
        return l , ids, [], []    
    init = [ '%s = new wxGrid(%s, %s);\n' % 
             (obj.name, parent, id) ]
    props_buf = cppgen.generate_common_properties(obj)
    if prop.get('default', False):
        props_buf.append('%s->SetDefault();\n' % obj.name)
    return init, ids, props_buf, []

def cpp_generate_properties(obj):
    cppgen = common.code_writers['C++']
    out = []
    name = 'this'
    if not obj.is_toplevel: name = obj.name
    prop = obj.properties
    
    try: create_grid = int(prop['create_grid'])
    except (KeyError, ValueError): create_grid = False
    if not create_grid: return []
    
    out.append('%s.CreateGrid(%s, %s)\n' % (name, prop.get('rows_number'),
                                            prop.get('columns_number')))
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
        out.append('%s.SetGridLineColour(wxColour(%s))\n' %
                   (name, pygen._string_to_colour(prop['lines_color'])))
    if prop.get('label_bg_color', False):
        out.append('%s.SetLabelBackgroundColour(wxColour(%s))\n' %
                   (name, pygen._string_to_colour(prop['label_bg_color'])))
    sel_mode = prop.get('selection_mode')
    if sel_mode and sel_mode != 'wxGrid.wxGridSelectCells':
        out.append('%s.SetSelectionMode(%s)\n' % (name, sel_mode))
    out.extend(pygen.generate_common_properties(obj))
    if prop.get('column_labels', False):
        v = common.smart_split(prop.get('column_labels'), 1)
        i = 0
        for s in v:
            if s != '' and i < prop.get('columns_number'):
                out.append('%s.SetColLabelValue(%d,\'%s\')\n' %
                           (name, i, s))
            i = i + 1

    return out



def initialize():
    common.class_names['EditGrid'] = 'wxGrid'
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxGrid', python_code_generator,
                                 python_generate_properties,
                                 ['from wxPython.grid import *\n'])
    xrcgen = common.code_writers.get("XRC")
    if xrcgen:
        xrcgen.add_widget_handler('wxGrid', xrcgen.NotImplementedXrcObject)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        constructor = [('wxWindow*', 'parent'), ('int', 'id'),
                       ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                       ('const wxSize&', 'size', 'wxDefaultSize'),
                       ('long', 'style', '0')]
        cppgen.add_widget_handler('wxGrid', cpp_code_generator, constructor,
                                  cpp_generate_properties)
