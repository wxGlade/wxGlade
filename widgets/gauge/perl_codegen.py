"""\
Perl generator functions for wxGauge objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen


class PerlGaugeGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, ' \
           '%(range)s%(style)s);\n'
    default_style = 'wxGA_HORIZONTAL'

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)
        self.tmpl_dict['range'] = obj.properties.get('range', '10')
        return

# end of class PerlGaugeGenerator


def initialize():
    klass = 'wxGauge'
    common.class_names['EditGauge'] = klass
    common.register('perl', klass, PerlGaugeGenerator(klass))
