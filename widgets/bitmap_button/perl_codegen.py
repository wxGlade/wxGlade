"""\
Perl generator functions for wxBitmapButton objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2015 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""

import common
import wcodegen


class PerlBitmapButtonGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, ' \
           '%(bitmap)s%(style)s);\n'

# end of class PerlBitmapButtonGenerator


def initialize():
    klass = 'wxBitmapButton'
    common.class_names['EditBitmapButton'] = klass
    common.register('perl', klass, PerlBitmapButtonGenerator(klass))
