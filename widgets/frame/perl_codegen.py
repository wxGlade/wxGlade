"""\
Perl code generator functions for wxFrame objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2013-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlFrameCodeGenerator(wcodegen.PerlWidgetCodeWriter):
    new_signature = [
        '$parent', '$id', '$title', '$pos', '$size', '$style', '$name'
    ]

    def get_code(self, obj):
        return [], [], [], []  # the frame can't be a children

    def get_properties_code(self, obj):
        out = []
        append = out.append
        title = obj.properties.get('title')
        if title:
            append('$self->SetTitle(%s);\n' % self.codegen.quote_str(title))
        icon = obj.properties.get('icon')
        if icon:
            stmt_icon = self.generate_code_bitmap(icon, obj.preview)
            out.append('my $icon = &Wx::wxNullIcon;\n')
            out.append('$icon->CopyFromBitmap(%s);\n' % stmt_icon)
            out.append('$self->SetIcon($icon);\n')
        out.extend(self.codegen.generate_common_properties(obj))
        return out

    def get_layout_code(self, obj):
        ret = ['$self->Layout();\n']
        try:
            if int(obj.properties['centered']):
                ret.append('$self->Centre();\n')
        except (KeyError, ValueError):
            pass
        return ret

# end of class PerlFrameCodeGenerator


class PerlMDIChildFrameCodeGenerator(PerlFrameCodeGenerator):
    import_modules = ['Wx::MDI']

# end of class PerlMDIChildFrameCodeGenerator


def initialize():
    klass = 'wxFrame'
    cn = common.class_names
    cn['EditFrame'] = klass
    cn['EditMDIChildFrame'] = 'wxMDIChildFrame'
    common.toplevels['EditFrame'] = 1
    common.toplevels['EditMDIChildFrame'] = 1

    plgen = common.code_writers.get('perl')
    if plgen:
        awh = plgen.add_widget_handler
        awh('wxFrame', PerlFrameCodeGenerator(klass))
        awh('wxMDIChildFrame', PerlMDIChildFrameCodeGenerator(klass))

        aph = plgen.add_property_handler
        aph('menubar', plgen.DummyPropertyHandler)
