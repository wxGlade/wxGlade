"""\
Perl generator functions for wxCheckBox objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class PerlCodeGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, ' \
           '%(label)s%(style)s);\n'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.has_setvalue1 = obj.properties.get('checked', False)
        return

# end of class PerlCodeGenerator


def initialize():
    common.class_names['EditCheckBox'] = 'wxCheckBox'

    plgen = common.code_writers.get('perl')
    if plgen:
        plgen.add_widget_handler('wxCheckBox', PerlCodeGenerator())
