"""
Perl code generator functions for wxHyperlinkCtrl objects

@copyright: 2012-2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlHyperlinkCtrlGenerator(wcodegen.PerlWidgetCodeWriter):

    supported_by = ((2, 8), (3, 0),)

    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, %(label)s, ' \
           '%(url)s%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['url'] = self.codegen.quote_str(obj.properties.get('url', ''))
        self.has_setvalue1 = obj.properties.get('checked', False)
        return

# end of class PerlHyperlinkCtrlGenerator


def initialize():

    common.class_names['EditHyperlinkCtrl'] = 'wxHyperlinkCtrl'
    plgen = common.code_writers.get('perl')

    if plgen:
        plgen.add_widget_handler(
            'wxHyperlinkCtrl',
            PerlHyperlinkCtrlGenerator()
        )
