"""\
Perl generator functions for wxTreeCtrl objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlTreeCtrlGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s%(style)s);\n'


def initialize():
    klass = 'wxTreeCtrl'
    common.class_names['EditTreeCtrl'] = klass
    common.register('perl', klass, PerlTreeCtrlGenerator(klass))
