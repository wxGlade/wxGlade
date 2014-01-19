"""\
Perl generator functions for wxListBox objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from ChoicesCodeHandler import *


class PerlListBoxGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, ' \
           'wxDefaultPosition, wxDefaultSize, [%(choices)s], %(style)s);\n'
    has_choice = True

# end of class PerlListBoxGenerator


def initialize():
    common.class_names['EditListBox'] = 'wxListBox'

    plgen = common.code_writers.get('perl')
    if plgen:
        plgen.add_widget_handler('wxListBox', PerlListBoxGenerator())
        plgen.add_property_handler('choices', ChoicesCodeHandler)
