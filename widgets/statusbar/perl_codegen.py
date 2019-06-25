"""\
Perl code generator functions for wxStatusBar objects

@copyright: 2002-2007 Alberto Griggio
@copyright: 2013-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen
#from .codegen import StatusFieldsHandler


class PerlStatusBarCodeGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = $self->CreateStatusBar(%(labels_len)s%(style)s);\n'
    prefix_style = False
    tmpl_flags = ', %s'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)

        fields = obj.fields # properties['statusbar']
        labels = [f[0] for f in fields]
        widths = [int(f[1]) for f in fields]
        self.tmpl_dict['labels_len'] = len(labels)
        self.tmpl_dict['widths'] = ', '.join([str(w) for w in widths])
        self.tmpl_dict['widths_len'] = len(widths)

        append = self.tmpl_props.append
        append( '%(name)s->SetStatusWidths(%(widths)s);\n' )
        append( '\n' )

        # don't add statusbar fields without labels
        if [lb for lb in labels if lb]:
            append( '%(comment)s statusbar fields\n' )
            append( 'my( @%(obj_name)s_fields ) = (\n' )
            for lb in labels:
                append( '%%(tab)s%s,\n' % self.codegen.quote_str(lb) )
            append( ');\n' )
            append( '\n' )

            stmt = """\
if( @%(obj_name)s_fields ) {
%(tab)s%(name)s->SetStatusText($%(obj_name)s_fields[$_], $_)
%(tab)sfor 0 .. $#%(obj_name)s_fields ;
}"""
            self.tmpl_props.extend( self.stmt2list(stmt) )


def initialize():
    klass = 'wxStatusBar'
    common.class_names['EditStatusBar'] = klass

    plgen = common.code_writers.get('perl')
    if plgen:
        plgen.register_widget_code_generator('wxStatusBar', PerlStatusBarCodeGenerator(klass))
