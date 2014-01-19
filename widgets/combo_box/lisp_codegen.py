"""\
Lisp generator functions for wxComboBox objects

@copyright: 2002-2004 D. H. aka crazyinsomniac on sourceforge
@copyright: 2014 Carsten Grohmann
@license: MIT (see license.txt) - THIS PROGRAM COMES WITH NO WARRANTY
"""


import common
import wcodegen
from ChoicesCodeHandler import *


class LispComboBoxGenerator(wcodegen.LispWidgetCodeWriter):
    tmpl = '(setf %(name)s (%(klass)s_Create %(parent)s %(id)s "" -1 -1 -1 ' \
           '-1 %(choices_len)s (vector %(choices)s) %(style)s))\n'

    default_style = 'wxCB_DROPDOWN'
    has_choice = True
    set_default_style = True

# end of class LispComboBoxGenerator


def initialize():
    common.class_names['EditComboBox'] = 'wxComboBox'

    codegen = common.code_writers.get('lisp')
    if codegen:
        codegen.add_widget_handler('wxComboBox', LispComboBoxGenerator())
        codegen.add_property_handler('choices', ChoicesCodeHandler)
