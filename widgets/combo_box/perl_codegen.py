"""\
Perl generator functions for wxComboBox objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014-2016 Carsten Grohmann
@license: MIT (see LICENSE.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from ChoicesCodeHandler import *


class PerlComboBoxGenerator(wcodegen.PerlWidgetCodeWriter):
    tmpl = '%(name)s = %(klass)s->new(%(parent)s, %(id)s, "", ' \
           'wxDefaultPosition, wxDefaultSize, [%(choices)s], %(style)s);\n'

    prefix_style = False
    set_default_style = True

# end of class PerlComboBoxGenerator


def initialize():
    klass = 'wxComboBox'
    common.class_names['EditComboBox'] = klass
    common.register('perl', klass, PerlComboBoxGenerator(klass),
                    'choices', ChoicesCodeHandler)
