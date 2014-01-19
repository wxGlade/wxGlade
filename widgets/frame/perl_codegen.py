"""\
Perl code generator functions for wxFrame objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2013-2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
from MenuTree import *
from codegen import StatusFieldsHandler


class PerlStatusBarCodeGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = $self->CreateStatusBar(%(labels_len)s%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)

        labels, widths = obj.properties['statusbar']
        self.tmpl_dict['labels_len'] = len(labels)
        self.tmpl_dict['widths'] = ', '.join(map(str, widths))
        self.tmpl_dict['widths_len'] = len(widths)
        append = self.tmpl_props.append

        append('%(name)s->SetStatusWidths(%(widths)s);\n')
        append('\n')

        append('%(comment)s statusbar fields\n')
        append('my( @%(obj_name)s_fields ) = (\n')
        for lb in labels:
            append('%%(tab)s%s,\n' % self.codegen.quote_str(lb))
        append(');\n')
        append('\n')

        stmt = """\
if( @%(obj_name)s_fields ) {
%(tab)s%(name)s->SetStatusText($%(obj_name)s_fields[$_], $_)
%(tab)sfor 0 .. $#%(obj_name)s_fields ;
}"""
        self.tmpl_props.extend(self.stmt2list(stmt))

# end of class PerlStatusBarCodeGenerator


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
            append('my $icon = Wx::Icon->new();\n')
            append('$icon->CopyFromBitmap(Wx::Bitmap->new(%s, '
                   'wxBITMAP_TYPE_ANY));\n' % self.codegen.quote_str(icon))
            append('$self->SetIcon($icon);\n')

        out.extend(self.codegen.generate_common_properties(obj))
        return out

    def get_layout_code(self, obj):
        ret = ['$self->Layout();\n']
        try:
            if int(obj.properties['centered']):
                ret.append('$self->Centre();\n')
        except (KeyError, ValueError):
            pass
        if obj.properties.get('size', '').strip() and \
           self.codegen.for_version < (2, 8):
            ret.append(self.codegen.generate_code_size(obj))
        return ret

# end of class PerlFrameCodeGenerator


class PerlMDIChildFrameCodeGenerator(PerlFrameCodeGenerator):
    extra_headers = ['Wx::MDI']

# end of class PerlMDIChildFrameCodeGenerator


def initialize():
    cn = common.class_names
    cn['EditFrame'] = 'wxFrame'
    cn['EditMDIChildFrame'] = 'wxMDIChildFrame'
    cn['EditStatusBar'] = 'wxStatusBar'
    common.toplevels['EditFrame'] = 1
    common.toplevels['EditMDIChildFrame'] = 1

    plgen = common.code_writers.get('perl')
    if plgen:
        awh = plgen.add_widget_handler
        awh('wxFrame', PerlFrameCodeGenerator())
        awh('wxMDIChildFrame', PerlMDIChildFrameCodeGenerator())
        awh('wxStatusBar', PerlStatusBarCodeGenerator())

        aph = plgen.add_property_handler
        aph('fields', StatusFieldsHandler)
        aph('menubar', plgen.DummyPropertyHandler)
        aph('statusbar', plgen.DummyPropertyHandler)
