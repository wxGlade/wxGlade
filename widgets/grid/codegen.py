# codegen.py: code generator functions for wxGrid objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common,misc

def python_code_generator(obj):
    """\
    fuction that generates python code for wxGrid objects.
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj)
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    if obj.is_toplevel:
        l = []
        if id_name: l.append(id_name)
        l.append('self.%s = %s(%s, %s)\n' % (obj.name, obj.klass, parent,
                                                 id))
        return l, [], []    
    init = []
    if id_name: init.append(id_name)
    init.append('self.%s = wxGrid(%s, %s)\n' %
                (obj.name, parent, id))
    props_buf = python_generate_properties(obj)
    return init, props_buf, []


def python_generate_properties(obj):
    pygen = common.code_writers['python']
    out = []
    name = 'self'
    if not obj.is_toplevel: name += '.%s' % obj.name
    prop = obj.properties
    out.append('%s.CreateGrid(%s, %s)\n' % (name, prop.get('rows_number'), prop.get('columns_number')))
    if prop.get('row_label_size', False):
        out.append('%s.SetRowLabelSize(%s)\n' % (name, prop['row_label_size']))
    if prop.get('col_label_size', False):
        out.append('%s.SetColLabelSize(%s)\n' % (name, prop['col_label_size']))
    if prop.get('enable_editing', False):
        out.append('%s.EnableEditing(%s)\n' % (name, prop['enable_editing']))
    if prop.get('enable_grid_lines', False):
        out.append('%s.EnableGridLines(%s)\n' %
                   (name, prop['enable_grid_lines']))
    if prop.get('enable_col_resize', False):
        out.append('%s.EnableDragColSize(%s)\n' %
                   (name, prop['enable_col_resize']))
    if prop.get('enable_row_resize', False):
        out.append('%s.EnableDragRowSize(%s)\n' %
                   (name, prop['enable_row_resize']))
    if prop.get('enable_grid_resize', False):
        out.append('%s.EnableDragGridSize(%s)\n' %
                   (name, prop['enable_grid_resize']))
    if prop.get('lines_color', False):
        c = misc.string_to_color(prop['lines_color'])
        out.append('%s.SetGridLineColour(wxColour(%d,%d,%d))\n' %
                   (name, int(c.Red()), int(c.Green()), int(c.Blue()) ) )
    out.extend(pygen.generate_common_properties(obj))
    return out


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']
    class GridXrcObject(xrcgen.DefaultXrcObject):
        def write_property(self, name, val, outfile, tabs):
            if name == 'label':
                # translate & into _ as accelerator marker
                val2 = val.replace('&', '_')
                if val.count('&&') > 0:
                    while True:
                        index = val.find('&&')
                        if index < 0: break
                        val = val2[:index] + '&&' + val2[index+2:]
                else: val = val2
            xrcgen.DefaultXrcObject.write_property(self, name, val,
                                                   outfile, tabs)
    # end of class GridXrcObject

    return GridXrcObject(obj)


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
    if obj.properties.get('default', False):
        out.append('SetDefault();\n')
    out.extend(cppgen.generate_common_properties(obj))
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
        xrcgen.add_widget_handler('wxGrid', xrc_code_generator)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        constructor = [('wxWindow*', 'parent'), ('int', 'id'),
                       ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                       ('const wxSize&', 'size', 'wxDefaultSize'),
                       ('long', 'style', '0')]
        cppgen.add_widget_handler('wxGrid', cpp_code_generator, constructor,
                                  cpp_generate_properties)
