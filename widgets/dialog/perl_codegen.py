"""\
Perl generator functions for wxDialog objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class PerlDialogGenerator(wcodegen.PerlWidgetCodeWriter):
    new_signature = ['$parent', '$id', '$title', '$pos', '$size', '$style', '$name']

    def get_code(self, obj):
        return [], [], []

    def get_properties_code(self, obj):
        out = []
        if obj.title:
            out.append( '$self->SetTitle(%s);\n' % self.codegen.quote_str(obj.title) )
        if obj.icon:
            stmt_icon = self.generate_code_bitmap(obj.icon, self.codegen.preview)
            out.append('my $icon = &Wx::wxNullIcon;\n')
            out.append('$icon->CopyFromBitmap(%s);\n' % stmt_icon)
            out.append('$self->SetIcon($icon);\n')
        out.extend(self.codegen.generate_common_properties(obj))
        return out

    def get_layout_code(self, obj):
        ret = ['$self->Layout();\n']
        if "centered" in obj.properties and obj.centered:
            ret.append('$self->Centre();\n')
        return ret


def initialize():
    klass = 'wxDialog'
    common.class_names['EditDialog'] = klass
    common.toplevels['EditDialog'] = 1
    common.register('perl', klass, PerlDialogGenerator(klass))
