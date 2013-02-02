# codegen.py: code generator functions for wxStaticBitmap objects
#
# Copyright (c) 2002-2007 Alberto Griggio <agriggio@users.sourceforge.net>
#
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common

#this should be in common 
_bmp_str_types = {
    '.bmp' : 'wxBITMAP_TYPE_BMP',
    '.gif' : 'wxBITMAP_TYPE_GIF',
    '.xpm' : 'wxBITMAP_TYPE_XPM',
    '.jpg' : 'wxBITMAP_TYPE_JPEG',
    '.jpeg': 'wxBITMAP_TYPE_JPEG',
    '.png' : 'wxBITMAP_TYPE_PNG',
    '.pcx' : 'wxBITMAP_TYPE_PCX'
    }


class LispCodeGenerator:
    def get_code(self, obj):
        init = []
        codegen = common.code_writers['lisp']
        prop = obj.properties

        attribute = codegen.test_attribute(obj)

        id_name, id = codegen.generate_code_id(obj) 

        if not obj.parent.is_toplevel:
            parent = '(slot-%s obj)' % obj.parent.name
        else:
            parent = '(slot-top-window obj)'

        bmp_file = prop.get('bitmap', '')
        if not bmp_file:
            bmp = 'wxNullBitmap'
        elif bmp_file.startswith('var:'):
            # this is a variable holding bitmap path
            var = bmp_file[4:].strip()
            if var[0] != "$":
                var = "$" + var
            bmp = '(wxBitmap_CreateLoad %s wxBITMAP_TYPE_ANY)' % var
        elif bmp_file.startswith('code:'):
            bmp = '(%s)' % bmp_file[5:].strip()
        else:
            bmp = '(wxBitmap_CreateLoad %s wxBITMAP_TYPE_ANY)' % \
                  codegen.quote_path(bmp_file)

        if id_name: init.append(id_name)
        if attribute:
            prefix = '(slot-%s obj)' % obj.name
        else:
            prefix = '$self'

        style = prop.get('style')
        if not style:
            style = '0'
        else:
            style = codegen.cn_f(style)

        init.append('(setf %s (wxStaticBitmap_Create %s %s  %s -1 -1 -1 -1 %s))\n' % 
                    (prefix, parent, id, bmp, style))
        props_buf = codegen.generate_common_properties(obj)

        if not attribute:
            # the object doesn't have to be stored as an attribute of the
            # custom class, but it is just considered part of the layout
            return [], [], init + props_buf
        return init, props_buf, []

# end of class LispCodeGenerator


def initialize():
    common.class_names['EditStaticBitmap'] = 'wxStaticBitmap'

    codegen = common.code_writers.get('lisp')
    if codegen:
        codegen.add_widget_handler('wxStaticBitmap', LispCodeGenerator())
