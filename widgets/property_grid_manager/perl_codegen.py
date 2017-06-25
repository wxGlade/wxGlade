"""\
Perl generator functions for wxPropertyGridManager objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@copyright: 2015 Franco Bugnano
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlCodeGenerator(wcodegen.PerlWidgetCodeWriter):
    import_modules = ['use Wx::PropertyGridManager;\n']
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s%(style)s);\n'
    prefix_style = True

    def _prepare_tmpl_content(self, obj):
        wcodegen.PerlWidgetCodeWriter._prepare_tmpl_content(self, obj)

        return


def initialize():
    klass = 'wxPropertyGridManager'
    common.class_names['EditPropertyGridManager'] = klass
    common.register('perl', klass, PerlCodeGenerator(klass))
