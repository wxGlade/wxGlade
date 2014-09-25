"""\
Perl generator functions for wxBitmapButton objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlBitmapButtonGenerator(wcodegen.PerlWidgetCodeWriter):
    def get_code(self, obj):
        plgen = common.code_writers['perl']
        prop = obj.properties
        id_name, id = plgen.generate_code_id(obj) 
        bmp_file = prop.get('bitmap', '')

        if not obj.parent.is_toplevel:
            parent = '$self->{%s}' % obj.parent.name
        else:
            parent = '$self'

        if not bmp_file:
            bmp = 'wxNullBitmap'
        elif bmp_file.startswith('var:'):
            # this is a variable holding pathname of bitmap
            var = bmp_file[4:].strip()
            if var[0] != "$":
                var = "$" + var
            bmp = 'Wx::Bitmap->new(%s, wxBITMAP_TYPE_ANY)' % var
        elif bmp_file.startswith('code:'): # this is a code chunk
            bmp = '(%s)' % bmp_file[5:].strip()
        else:
            bmp = 'Wx::Bitmap->new(%s, wxBITMAP_TYPE_ANY)' % \
                  plgen.quote_path(bmp_file)
        init = []
        if id_name:
            init.append(id_name)

        klass = obj.base
        if klass != obj.klass:
            klass = obj.klass
        else:
            klass = klass.replace('wx', 'Wx::', 1)

        init.append('$self->{%s} = %s->new(%s, %s, %s);\n' % 
                    ( obj.name, klass, parent, id, bmp) )

        props_buf = plgen.generate_common_properties(obj)

        disabled_bmp = prop.get('disabled_bitmap')
        if disabled_bmp:
            if disabled_bmp.startswith('var:'):
                var = disabled_bmp[4:].strip()
                if var[0] != "$":
                    var = "$" + var
                props_buf.append(
                    '$self->{%s}->SetBitmapDisabled('
                    'Wx::Bitmap->new(%s, wxBITMAP_TYPE_ANY));\n' %
                    (obj.name, var))
            elif disabled_bmp.startswith('code:'):
                var = disabled_bmp[5:].strip()
                props_buf.append(
                    '$self->{%s}->SetBitmapDisabled('
                    '%s);\n' % (obj.name, var))
            else:
                props_buf.append(
                    '$self->{%s}->SetBitmapDisabled('
                    'Wx::Bitmap->new(%s, wxBITMAP_TYPE_ANY));\n' % \
                    (obj.name, plgen.quote_path(disabled_bmp)))
                
        if not prop.has_key('size'):
            props_buf.append(
                '$self->{%s}->SetSize($self->{%s}->GetBestSize());\n' %
                (obj.name, obj.name)
                )

        if prop.get('default', False):
            props_buf.append('$self->{%s}->SetDefault();\n' % obj.name)
        return init, props_buf, []

# end of class PerlBitmapButtonGenerator


def initialize():
    klass = 'wxBitmapButton'
    common.class_names['EditBitmapButton'] = klass
    common.register('perl', klass, PerlBitmapButtonGenerator(klass))
