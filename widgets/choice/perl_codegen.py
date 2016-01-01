"""\
Perl generator functions for wxChoice objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from ChoicesCodeHandler import *


class PerlChoiceGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, ' \
           'wxDefaultPosition, wxDefaultSize, [%(choices)s], %(style)s);\n'

# end of class PerlChoiceGenerator


def initialize():
    klass = 'wxChoice'
    common.class_names['EditChoice'] = klass
    common.register('perl', klass, PerlChoiceGenerator(klass),
                    'choices', ChoicesCodeHandler)
