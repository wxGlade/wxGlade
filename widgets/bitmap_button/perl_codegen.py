# perl_codegen.py : perl generator functions for wxBitmapButton objects
# $Id: perl_codegen.py,v 1.1 2003/06/23 21:21:11 crazyinsomniac Exp $
#
# Copyright (c) 2002-2003 D.H. aka crazyinsomniac on sourceforge
# License: MIT (see license.txt)
# THIS PROGRAM COMES WITH NO WARRANTY

import common, os

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

class PerlCodeGenerator:
    def get_code(self, obj):
        plgen = common.code_writers['perl']
        prop = obj.properties
        id_name, id = plgen.generate_code_id(obj) 
        bmp_file = prop.get('bitmap', '')

        if not obj.parent.is_toplevel:
            parent = '$self->{%s}' % obj.parent.name
        else:
            parent = '$self'

        if not bmp_file: bmp = 'wxNullBitmap'
        else:
            type = _bmp_str_types.get(os.path.splitext(bmp_file)[1].lower())
            if not type:
                bmp = 'wxNullBitmap'
            else:
                bmp = 'Wx::Bitmap->new(%s, %s)' % \
                      (plgen.quote_str(bmp_file), type)
        init = []
        if id_name: init.append(id_name)
        init.append('\t$self->{%s} = %s->new(%s, %s, %s);\n' % 
                    (obj.name, obj.klass.replace('wx','Wx::',1), parent, id, bmp))

        props_buf = plgen.generate_common_properties(obj)
        if not prop.has_key('size'):
            props_buf.append('\t$self->{%s}->SetSize($self->{%s}->GetBestSize());\n' % \
                             (obj.name, obj.name))
        if prop.get('default', False):
            props_buf.append('\t$self->{%s}->SetDefault();\n' % obj.name)
        return init, props_buf, []

# end of class PerlCodeGenerator



def initialize():
    common.class_names['EditBitmapButton'] = 'wxBitmapButton'
    plgen = common.code_writers.get('perl')

    if plgen:
        plgen.add_widget_handler('wxBitmapButton', PerlCodeGenerator())
