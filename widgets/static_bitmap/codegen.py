# codegen.py: code generator functions for wxStaticBitmap objects
# $Id: codegen.py,v 1.17 2003/12/20 00:26:58 agriggio Exp $
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
        cn = pygen.cn
        prop = obj.properties

        attribute = pygen.test_attribute(obj)

        id_name, id = pygen.generate_code_id(obj) 
        bmp_file = prop.get('bitmap', '')
        if not bmp_file:
            bmp = cn('wxNullBitmap')
        elif bmp_file.startswith('var:'):
            if obj.preview:
                bmp = cn('wxEmptyBitmap(1, 1)')
            else:
                bmp = cn('wxBitmapFromXPMData') + '(%s)' % bmp_file[4:].strip()
        else:
            bmp = (cn('wxBitmap') + '(%s, ' + cn('wxBITMAP_TYPE_ANY') +
                   ')') % pygen.quote_str(bmp_file, False, False)
        if not obj.parent.is_toplevel: parent = 'self.%s' % obj.parent.name
        else: parent = 'self'
        init = []
        if id_name: init.append(id_name)
        if attribute: prefix = 'self.'
        else: prefix = ''
        style = prop.get('style')
        if style:
            style = ', style=' + style
        else:
            style = ''
        klass = obj.klass
        if klass == obj.base: klass = pygen.cn(klass)
        init.append('%s%s = %s(%s, %s, %s%s)\n' % 
                    (prefix, obj.name, klass, parent, id, bmp, style))
        props_buf = pygen.generate_common_properties(obj)
        if not attribute:
            # the object doesn't have to be stored as an attribute of the
            # custom class, but it is just considered part of the layout
            return [], [], init + props_buf
        return init, props_buf, []

# end of class PythonCodeGenerator


class CppCodeGenerator:
    def get_code(self, obj):
        cppgen = common.code_writers['C++']
        prop = obj.properties

        attribute = cppgen.test_attribute(obj)

        id_name, id = cppgen.generate_code_id(obj) 
        if id_name: ids = [ id_name ]
        else: ids = []
        bmp_file = prop.get('bitmap', '')
        if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
        else: parent = 'this'
        if not bmp_file:
            bmp = 'wxNullBitmap'
        elif bmp_file.startswith('var:'):
            bmp = 'wx.Bitmap(%s)' % bmp_file[4:].strip()
        else:
            bmp = 'wx.Bitmap(%s, wxBITMAP_TYPE_ANY)' % \
                  cppgen.quote_str(bmp_file, False, False)
        if not obj.parent.is_toplevel: parent = '%s' % obj.parent.name
        else: parent = 'this'
        if attribute: prefix = ''
        else: prefix = '%s* ' % obj.klass
        style = prop.get('style')
        if style:
            style = ', wxDefaultPosition, wxDefaultSize, ' + style
        else:
            style = ''
        init = [ '%s%s = new %s(%s, %s, %s%s);\n' %
                 (prefix, obj.name, obj.klass, parent, id, bmp, style) ]
        props_buf = cppgen.generate_common_properties(obj)
        if not attribute:
            return [], ids, [], init + props_buf
        return init, ids, props_buf, []

# end of class CppCodeGenerator


def xrc_code_generator(obj):
    xrcgen = common.code_writers['XRC']

    class XrcCodeGenerator(xrcgen.DefaultXrcObject):
        def write(self, *args, **kwds):
            try: del self.properties['attribute']
            except KeyError: pass
            xrcgen.DefaultXrcObject.write(self, *args, **kwds)

    return XrcCodeGenerator(obj)


def initialize():
    common.class_names['EditStaticBitmap'] = 'wxStaticBitmap'

    pygen = common.code_writers.get('python')
    if pygen:
        pygen.add_widget_handler('wxStaticBitmap', PythonCodeGenerator())
    cppgen = common.code_writers.get('C++')
    if cppgen:
        cppgen.add_widget_handler('wxStaticBitmap', CppCodeGenerator())
    xrcgen = common.code_writers.get('XRC')
    if xrcgen:
        xrcgen.add_widget_handler('wxStaticBitmap', xrc_code_generator)
