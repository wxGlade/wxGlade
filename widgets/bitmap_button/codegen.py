# codegen.py: code generator functions for wxBitmapButton objects
#
# Copyright (c) 2002 Alberto Griggio <albgrig@tiscalinet.it>
# License: GPL (see license.txt)

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
    fuction that generates python code for wxBitmapButton objects.
    """
    pygen = common.code_writers['python']
    prop = obj.properties
    id_name, id = pygen.generate_code_id(obj) 
    bmp_file = prop.get('bitmap', '')
    if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
    else: parent = 'self'
    if not bmp_file: bmp = 'wxNullBitmap'
    else:
        type = _bmp_str_types.get(os.path.splitext(bmp_file)[1].lower())
        if not type: bmp = 'wxNullBitmap'
        else:
            if os.sep == '\\': bmp_file = bmp_file.replace(os.sep, '/')
            bmp = 'wxBitmap("%s", %s)' % (bmp_file.replace('"', r'\"'), type)
    if obj.is_toplevel:
        l = ['self.%s = %s(%s, %s, %s)\n' % (obj.name, obj.klass, parent,
                                             id, bmp)]
        if id_name: l.append(id_name) # init lines are written in reverse order
        return l , [], []    
    init = [ 'self.%s = wxBitmapButton(%s, %s, %s)\n' % 
             (obj.name, parent, id, bmp) ]
    if id_name: init.append(id_name) # init lines are written in reverse order
    props_buf = pygen.generate_common_properties(obj)
    if not prop.has_key(size):
        props_buf.append('self.%s.SetSize(self.%s.GetBestSize())\n' % \
                         (obj.name, obj.name))
    return init, props_buf, []

def python_generate_properties(obj):
    pygen = common.code_writers['python']
    out = []
    if not obj.properties.has_key('size'):
        out.append('self.SetSize(self.GetBestSize())\n')
    out.extend(pygen.generate_common_properties(obj))
    return out


def cpp_code_generator(obj):
    """\
    fuction that generates C++ code for wxBitmapButton objects.
    """
    cppgen = common.code_writers['C++']
    prop = obj.properties
    id_name, id = cppgen.generate_code_id(obj) 
    if id_name: ids = [ '%s = %s' % (id_name, id) ]
    else: ids = []
    bmp_file = prop.get('bitmap', '')
    if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
    else: parent = 'this'
    if not bmp_file: bmp = 'wxNullBitmap'
    else:
        type = _bmp_str_types.get(os.path.splitext(bmp_file)[1].lower())
        if not type: bmp = 'wxNullBitmap'
        else:
            if os.sep == '\\': bmp_file = bmp_file.replace(os.sep, '/')
            bmp = 'wxBitmap("%s", %s)' % (bmp_file.replace('"', r'\"'), type)
    if obj.is_toplevel:
        l = ['%s = new %s(%s, %s, %s);\n' % (obj.name, obj.klass, parent,
                                            id, bmp)]
        return l, ids, [], []    
    init = [ '%s = new wxBitmapButton(%s, %s, %s);\n' % 
             (obj.name, parent, id, bmp) ]
    props_buf = cppgen.generate_common_properties(obj)
    if not prop.has_key(size):
        props_buf.append('%s->SetSize(%s->GetBestSize());\n' % \
                         (obj.name, obj.name))
    return init, ids, props_buf, []

def cpp_generate_properties(obj):
    cppgen = common.code_writers['C++']
    out = []
    if not obj.properties.has_key('size'):
        out.append('SetSize(GetBestSize());\n')
    out.extend(cppgen.generate_common_properties(obj))
    return out


def initialize():
    common.class_names['EditBitmapButton'] = 'wxBitmapButton'
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxBitmapButton', python_code_generator,
                                 python_generate_properties)        
    cppgen = common.code_writers.get('C++')
    if cppgen:
        constructor = [('wxWindow*', 'parent'), ('int', 'id'),
                       ('const wxBitmap&', 'bitmap'),
                       ('const wxPoint&', 'pos', 'wxDefaultPosition'),
                       ('const wxSize&', 'size', 'wxDefaultSize'),
                       ('long', 'style', '0')]
        cppgen.add_widget_handler('wxBitmapButton', cpp_code_generator,
                                  constructor, cpp_generate_properties)
