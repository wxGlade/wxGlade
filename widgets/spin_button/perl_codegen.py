"""\
Perl generator functions for wxSpinButton objects

@copyright: 2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlSpinButtonGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s,%(style)s);\n'


def initialize():
    klass = 'wxSpinButton'
    common.class_names['EditSpinButton'] = klass
    common.register('perl', klass, PerlSpinButtonGenerator(klass))
