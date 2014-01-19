"""\
Perl generator functions for wxChoice objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from ChoicesCodeHandler import *


class PerlChoiceGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, ' \
           'wxDefaultPosition, wxDefaultSize, [%(choices)s], %(style)s);\n'
    has_choice = True

# end of class PerlChoiceGenerator


def initialize():
    common.class_names['EditChoice'] = 'wxChoice'

    plgen = common.code_writers.get('perl')
    if plgen:
        plgen.add_widget_handler('wxChoice', PerlChoiceGenerator())
        plgen.add_property_handler('choices', ChoicesCodeHandler)
