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
    size = pygen.generate_code_size(obj)
    if size != '(-1, -1)': size = ', size=%s' % size
    else: size = ''
    init = [ 'self.%s = wxBitmapButton(%s, %s, %s%s)\n' % 
             (obj.name, parent, id, bmp, size) ]
    if id_name: init.append(id_name) # init lines are written in reverse order
    props_buf = []
    if prop.has_key('foreground'):
        props_buf.append(pygen.generate_code_foreground(obj))
    if prop.has_key('background'):
        props_buf.append(pygen.generate_code_background(obj))
    if prop.has_key('font'): props_buf.append(pygen.generate_code_font(obj))
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


def initialize():
    common.class_names['EditBitmapButton'] = 'wxBitmapButton'
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxBitmapButton', python_code_generator,
                                 python_generate_properties)        
