# codegen.py: code generator functions for wxStaticBitmap objects
#
# Copyright (c) 2002-2003 Alberto Griggio <albgrig@tiscalinet.it>
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common, os

_bmp_str_types = {
    '.bmp' : 'wxBITMAP_TYPE_BMP',
    '.gif' : 'wxBITMAP_TYPE_GIF',
    '.xpm' : 'wxBITMAP_TYPE_XPM',
    '.jpg' : 'wxBITMAP_TYPE_JPEG',
    '.jpeg': 'wxBITMAP_TYPE_JPEG',
    '.png' : 'wxBITMAP_TYPE_PNG',
    '.pcx' : 'wxBITMAP_TYPE_PCX'
    }

def python_code_generator(obj):
    """\
    fuction that generates python code for wxStaticBitmap objects.
    """
    pygen = common.code_writers['python']
    prop = obj.properties

    attribute = pygen.test_attribute(obj)

    id_name, id = pygen.generate_code_id(obj) 
    bmp_file = prop.get('bitmap', '')
    if not bmp_file: bmp = 'wxNullBitmap'
    else:
        type = _bmp_str_types.get(os.path.splitext(bmp_file)[1].lower())
        if not type: bmp = 'wxNullBitmap'
        else:
            if os.sep == '\\': bmp_file = bmp_file.replace(os.sep, '/')
            bmp = 'wxBitmap("%s", %s)' % (bmp_file.replace('"', r'\"'), type)
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    init = []
    if id_name: init.append(id_name)
    if attribute: prefix = 'self.'
    else: prefix = ''
    init.append('%s%s = %s(%s, %s, %s)\n' % 
                (prefix, obj.name, obj.klass, parent, id, bmp))
    props_buf = pygen.generate_common_properties(obj)
    if not attribute:
        # the object doesn't have to be stored as an attribute of the custom
        # class, but it is just considered part of the layout
        return [], [], init + props_buf
    return init, props_buf, []


def cpp_code_generator(obj):
    """\
    fuction that generates C++ code for wxStaticBitmap objects.
    """
    cppgen = common.code_writers['C++']
    prop = obj.properties

    attribute = cppgen.test_attribute(obj)

    id_name, id = cppgen.generate_code_id(obj) 
    if id_name: ids = [ id_name ]
    else: ids = []
    bmp_file = prop.get('bitmap', '')
    if not bmp_file: bmp = 'wxNullBitmap'
    else:
        type = _bmp_str_types.get(os.path.splitext(bmp_file)[1].lower())
        if not type: bmp = 'wxNullBitmap'
        else:
            if os.sep == '\\': bmp_file = bmp_file.replace(os.sep, '/')
            bmp = 'wxBitmap("%s", %s)' % (bmp_file.replace('"', r'\"'), type)
    if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
    else: parent = 'this'
    if attribute: prefix = ''
    else: prefix = '%s* ' % obj.klass
    init = [ '%s%s = new %s(%s, %s, %s);\n' %
             (prefix, obj.name, obj.klass, parent, id, bmp) ]
    props_buf = cppgen.generate_common_properties(obj)
    if not attribute:
        return [], ids, [], init + props_buf
    return init, ids, props_buf, []


def initialize():
    common.class_names['EditStaticBitmap'] = 'wxStaticBitmap'

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxStaticBitmap', python_code_generator)
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxStaticBitmap', cpp_code_generator)

    
