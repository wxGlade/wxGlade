# codegen.py: code generator functions for wxBitmapButton objects
# $Id: codegen.py,v 1.12 2003/07/26 09:15:57 agriggio Exp $
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

class PythonCodeGenerator:
    def get_code(self, obj):
        pygen = common.code_writers['python']
        prop = obj.properties
        id_name, id = pygen.generate_code_id(obj) 
        bmp_file = prop.get('bitmap', '')
        if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
        else: parent = 'self'
        if not bmp_file:
            bmp = 'wxNullBitmap'
        elif bmp_file.startswith('var:'):
            if obj.preview:
                bmp = 'wxEmptyBitmap(1, 1)'
            else:
                bmp = 'wxBitmapFromXPMData(%s)' % bmp_file[4:].strip()
        else:
            bmp = 'wxBitmap(%s, wxBITMAP_TYPE_ANY)' % \
                  pygen.quote_str(bmp_file, False, False)
        init = []
        if id_name: init.append(id_name)
        init.append('self.%s = %s(%s, %s, %s)\n' % 
                    (obj.name, obj.klass, parent, id, bmp))
        props_buf = pygen.generate_common_properties(obj)
        if not prop.has_key('size'):
            props_buf.append('self.%s.SetSize(self.%s.GetBestSize())\n' % \
                             (obj.name, obj.name))
        if prop.get('default', False):
            props_buf.append('self.%s.SetDefault()\n' % obj.name)
        return init, props_buf, []

# end of class PythonCodeGenerator


class CppCodeGenerator:
    def get_code(self, obj):
        """\
        fuction that generates C++ code for wxBitmapButton objects.
        """
        cppgen = common.code_writers['C++']
        prop = obj.properties
        id_name, id = cppgen.generate_code_id(obj) 
        if id_name: ids = [ id_name ]
        else: ids = []
        bmp_file = prop.get('bitmap', '')
        if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
        else: parent = 'this'
        if not bmp_file:
            bmp = 'wxNullBitmap'
        elif bmp_file.startswith('var:'):
            bmp = 'wxBitmap(%s)' % bmp_file[4:].strip()
        else:
            bmp = 'wxBitmap(%s, wxBITMAP_TYPE_ANY)' % \
                  cppgen.quote_str(bmp_file, False, False)
        init = [ '%s = new %s(%s, %s, %s);\n' % 
                 (obj.name, obj.klass, parent, id, bmp) ]
        props_buf = cppgen.generate_common_properties(obj)
        if not prop.has_key('size'):
            props_buf.append('%s->SetSize(%s->GetBestSize());\n' % \
                             (obj.name, obj.name))
        if prop.get('default', False):
            props_buf.append('%s->SetDefault();\n' % obj.name)
        return init, ids, props_buf, []

# end of class CppCodeGenerator


def initialize():
    common.class_names['EditBitmapButton'] = 'wxBitmapButton'
    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxBitmapButton', PythonCodeGenerator())
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxBitmapButton', CppCodeGenerator())
