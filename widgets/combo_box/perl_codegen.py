"""\
Perl generator functions for wxComboBox objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from ChoicesCodeHandler import *


class PerlComboBoxGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, "", ' \
           'wxDefaultPosition, wxDefaultSize, [%(choices)s], %(style)s);\n'

    default_style = 'wxCB_DROPDOWN'
    has_choice = True
    prefix_style = False
    set_default_style = True

# end of class PerlComboBoxGenerator


def initialize():
    common.class_names['EditComboBox'] = 'wxComboBox'

    plgen = common.code_writers.get('perl')
    if plgen:
        plgen.add_widget_handler('wxComboBox', PerlComboBoxGenerator())
        plgen.add_property_handler('choices', ChoicesCodeHandler)
