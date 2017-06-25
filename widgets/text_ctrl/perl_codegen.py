"""\
Perl generator functions for wxTextCtrl objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlTextCtrlGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, %(value)s%(style)s);\n'


def initialize():
    klass = 'wxTextCtrl'
    common.class_names['EditTextCtrl'] = klass
    common.register('perl', klass, PerlTextCtrlGenerator(klass))
