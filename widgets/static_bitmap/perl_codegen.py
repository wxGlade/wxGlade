"""\
Perl code generator functions for wxStaticBitmap objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlStaticBitmapGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, ' \
           '%(bitmap)s%(style)s);\n'
    prefix_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)

        bmp_file = obj.properties.get('bitmap', '')
        if not bmp_file:
            stmt = self.codegen.cn('wxNullBitmap')
        elif bmp_file.startswith('var:'):
            # this is a variable holding bitmap path
            var = bmp_file[4:].strip()
            if var[0] != "$":
                var = "$" + var
            stmt = 'Wx::Bitmap->new(%s, wxBITMAP_TYPE_ANY)' % var
        elif bmp_file.startswith('code:'):
            stmt = '(%s)' % bmp_file[5:].strip()
        else:
            stmt = 'Wx::Bitmap->new(%s, wxBITMAP_TYPE_ANY)' % \
                   self.codegen.quote_path(bmp_file)

        self.tmpl_dict['bitmap'] = stmt

        return

# end of class PerlStaticBitmapGenerator


def initialize():
    klass = 'wxStaticBitmap'
    common.class_names['EditStaticBitmap'] = klass
    common.register('perl', klass, PerlStaticBitmapGenerator(klass))
